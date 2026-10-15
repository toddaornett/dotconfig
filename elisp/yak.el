;;; yak.el --- Message generators for agents and perhaps humans -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Todd Ornett
;;
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: July 28, 2026
;; Modified: October 15, 2026
;; Version: 0.0.1
;; Keywords: vc tools agent llm convenience
;; Package-Requires: ((emacs "31.1"))
;; Homepage: https://github.com/toddaornett/dotconfig
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Generate various messages and prompts, including a compact
;; `/accept-pr-duplicates` command from a GitHub Actions
;; duplicate-gate job log.  Listed ranges may contain finding
;; locations, so overlapping clones collapse per file.
;;
;;; Code:

(require 'git-tools)
(require 'caveman)
(require 'subr-x)

(defconst yak-pr-duplicate-range-regexp
  (rx-let ((sep (any ?{ ?} ?: ?` " \t\n\r\f\v"))
            (tok (not sep)))
    (rx (or bol (not (any alnum ?. ?_ ?/)))
      (group (not (any ?{ ?} ?: ?` ?/ " \t\n\r\f\v"))
        (* tok)
        "/"
        (+ tok))
      ":"
      (group (any "1-9") (* digit))
      "-"
      (group (any "1-9") (* digit))))
  "Regexp matching one `/accept-pr-duplicates' source range.

Group 1 is a slash-containing relative path, group 2 the start
line, and group 3 the end line.  Any standalone
`path/to/file:start-end' token matches; roots are not
hardcoded.")

(defconst yak-pr-duplicate-job-name-regexp "duplicate"
  "Regexp matching a GitHub Actions job that reports PR duplicates.

Compared case-insensitively against the job name.")

(defconst yak-pr-duplicate-report-regexp
  (rx (or "/accept-pr-duplicates"
        "DUPLICATION GATE"
        "duplicate metrics"
        "duplicated added"))
  "Regexp locating the duplicate-report section of a job log.")

(defun yak--github-actions-parts (url)
  "Return (OWNER REPO RUN-ID JOB-ID) from a GitHub Actions URL.

JOB-ID is nil when URL names only the workflow run.  URL is
trimmed.  Return nil when URL is not a GitHub Actions run or job
URL."
  (let ((trimmed (and (stringp url) (string-trim url))))
    (when (and trimmed (not (string-empty-p trimmed)))
      (cond
        ((string-match
           (concat "\\`https?://github\\.com/\\([^/]+\\)/\\([^/]+\\)"
             "/actions/runs/\\([0-9]+\\)/job/\\([0-9]+\\)"
             "\\(?:[?#].*\\)?\\'")
           trimmed)
          (list (match-string 1 trimmed)
            (match-string 2 trimmed)
            (match-string 3 trimmed)
            (match-string 4 trimmed)))
        ((string-match
           (concat "\\`https?://github\\.com/\\([^/]+\\)/\\([^/]+\\)"
             "/actions/runs/\\([0-9]+\\)\\(?:[?#].*\\)?\\'")
           trimmed)
          (list (match-string 1 trimmed)
            (match-string 2 trimmed)
            (match-string 3 trimmed)
            nil))))))

(defun yak--github-actions-url-from-clipboard-or-prompt ()
  "Return a GitHub Actions run or job URL from the clipboard or a prompt.

If the clipboard holds a GitHub Actions URL, return it.
Otherwise prompt until one is entered."
  (let ((clip (git-tools--clipboard-string)))
    (if (yak--github-actions-parts clip)
      (string-trim clip)
      (let (url)
        (while (not url)
          (let ((typed (string-trim
                         (read-string "GitHub Actions job URL: "))))
            (if (yak--github-actions-parts typed)
              (setq url typed)
              (message "That doesn't look like a GitHub Actions job URL; try again.")
              (sit-for 1))))
        url))))

(defun yak--strip-ansi (text)
  "Remove ANSI color sequences from TEXT."
  (replace-regexp-in-string "\e\\[[0-9;]*[A-Za-z]" "" text))

(defun yak--call-gh (&rest args)
  "Run the `gh' CLI with ARGS and return its stdout.

Signal `user-error' when `gh' is missing or exits non-zero."
  (unless (executable-find "gh")
    (user-error "GH CLI not found for yak-accept-pr-duplicates"))
  (with-temp-buffer
    (let ((code (apply #'call-process "gh" nil t nil args)))
      (unless (zerop code)
        (user-error "Client gh %s failed: %s"
          (mapconcat #'identity args " ")
          (string-trim (buffer-string))))
      (buffer-string))))

(defun yak--pr-duplicate-delta-job-id (owner repo run-id)
  "Return a duplicate-report job id for RUN-ID in OWNER/REPO.

Matches any job whose name contains
`yak-pr-duplicate-job-name-regexp' (case-insensitive).  When the
run lists more than one matching job, use the last one."
  (let* ((raw (string-trim
                (yak--call-gh "api"
                  (format "repos/%s/%s/actions/runs/%s/jobs" owner repo run-id)
                  "--jq" (format
                           ".jobs[] | select(.name | test(%s; \"i\")) | .id"
                           (json-serialize yak-pr-duplicate-job-name-regexp)))))
          (ids (and (not (string-empty-p raw))
                 (split-string raw "\n" t))))
    (unless ids
      (user-error "No job matching %s in run %s"
        yak-pr-duplicate-job-name-regexp run-id))
    (car (last ids))))

(defun yak--github-job-log (owner repo job-id)
  "Return the raw job log for JOB-ID in OWNER/REPO."
  (yak--call-gh "api" "--allow-escape-sequences"
    (format "repos/%s/%s/actions/jobs/%s/logs" owner repo job-id)))

(defun yak--pr-duplicate-report-excerpt (log)
  "Return the duplicate-report portion of job LOG.

Earlier scan output in the same job can print other
`path:start-end' clones.  Start at
`yak-pr-duplicate-report-regexp' and stop at the next Actions
group, if any.  When no report marker is found, return LOG
unchanged."
  (let* ((plain (yak--strip-ansi log))
          (start (string-match yak-pr-duplicate-report-regexp plain)))
    (if (not start)
      plain
      (let ((limit (string-match "##\\[group\\]" plain start)))
        (substring plain start (or limit (length plain)))))))

(defun yak--pr-duplicate-range-< (a b)
  "Return non-nil if duplicate range A sorts before B.

A and B are (PATH START END) lists."
  (or (string< (car a) (car b))
    (and (string= (car a) (car b))
      (or (< (nth 1 a) (nth 1 b))
        (and (= (nth 1 a) (nth 1 b))
          (< (nth 2 a) (nth 2 b)))))))

(defun yak--parse-pr-duplicate-ranges (text)
  "Return unique sorted (PATH START END) ranges found in TEXT.

PATH is a slash-containing source path.  START and END are
inclusive line numbers.  TEXT may contain ANSI color sequences."
  (let ((plain (yak--strip-ansi text))
         (start 0)
         (seen (make-hash-table :test #'equal))
         ranges)
    (while (string-match yak-pr-duplicate-range-regexp plain start)
      (let ((range (list (match-string 1 plain)
                     (string-to-number (match-string 2 plain))
                     (string-to-number (match-string 3 plain)))))
        (unless (gethash range seen)
          (puthash range t seen)
          (push range ranges))
        (setq start (match-end 0))))
    (sort ranges #'yak--pr-duplicate-range-<)))

(defun yak--merge-overlapping-pr-duplicate-ranges (ranges)
  "Merge overlapping, nested, or abutting ranges on the same path.

RANGES is a list of unique (PATH START END) lists, sorted by
`yak--pr-duplicate-range-<'.  Return a new list in the same
order."
  (let (merged)
    (dolist (range ranges)
      (let ((prev (car merged)))
        (if (and prev
              (string= (car prev) (car range))
              (>= (1+ (nth 2 prev)) (nth 1 range)))
          (setcar merged
            (list (car prev) (nth 1 prev) (max (nth 2 prev) (nth 2 range))))
          (push range merged))))
    (nreverse merged)))

(defun yak--covering-pr-duplicate-ranges-by-path (ranges)
  "Collapse RANGES to one min-max span per path.

RANGES must be sorted by `yak--pr-duplicate-range-<'."
  (let (out)
    (dolist (range ranges)
      (let ((prev (car out)))
        (if (and prev (string= (car prev) (car range)))
          (setcar out
            (list (car prev) (nth 1 prev) (max (nth 2 prev) (nth 2 range))))
          (push range out))))
    (nreverse out)))

(defun yak--merge-pr-duplicate-ranges (ranges)
  "Return compact covering ranges for `/accept-pr-duplicates'.

The bypass parser accepts a listed range that contains a finding
location, so nested detector spans on the same file can be one
argument.  When findings span two or more paths, each path
becomes a single min-max span.  Never return fewer than two
ranges when RANGES has two or more: the parser ignores a command
with one range."
  (let* ((overlapping (yak--merge-overlapping-pr-duplicate-ranges ranges))
          (covering (yak--covering-pr-duplicate-ranges-by-path overlapping)))
    (cond
      ((< (length ranges) 2) ranges)
      ((>= (length covering) 2) covering)
      ((>= (length overlapping) 2) overlapping)
      (t
        (let* ((cover (car covering))
                (extra (catch 'found
                         (dolist (range ranges)
                           (unless (equal range cover)
                             (throw 'found range)))
                         nil)))
          (if extra
            (sort (list cover extra) #'yak--pr-duplicate-range-<)
            covering))))))

(defun yak--one-line (text)
  "Return TEXT with every end-of-line character removed.

Strips ASCII CR, LF, VT, and FF, plus Unicode line and paragraph
separators, then trims leftover leading and trailing space."
  (string-trim
    (replace-regexp-in-string
      "[\n\r\v\f\u2028\u2029]+" "" text)))

(defun yak--format-accept-pr-duplicates (ranges)
  "Return a one-line `/accept-pr-duplicates' command covering RANGES.

RANGES is a list of (PATH START END) lists, already compacted by
`yak--merge-pr-duplicate-ranges'.  The result is a single line
with no end-of-line characters."
  (yak--one-line
    (concat "/accept-pr-duplicates "
      (mapconcat
        (lambda (range)
          (format "%s:%s-%s" (nth 0 range) (nth 1 range) (nth 2 range)))
        ranges
        " "))))

(defun yak--directory-and-branch ()
  "Return (DIRECTORY . BRANCH) for a yak prompt.
DIRECTORY is `git-tools-review-directory': the parallel review
clone for the current repository under `git-tools-review-home',
created on demand.  BRANCH is the current branch in DIRECTORY."
  (let ((dir (git-tools-review-directory)))
    (cons dir (git-tools-current-branch-name dir))))

;;;###autoload
(defun yak-implement (text)
  "Copy an implementation prompt for TEXT onto the kill ring.

Interactively, prompt for TEXT."
  (interactive "MText: ")
  (let* ((branch (git-tools-current-branch-name))
          (output (format "In %s, under the current branch %s, please make this change: %s."
                    default-directory branch text)))
    (kill-new output)
    (message "yak-implement: current branch %s in %s" branch default-directory)))

;;;###autoload
(defun yak-update-stacked-pull-request (text)
  "Copy an implementation prompt for TEXT and update stacked PR onto the kill ring.

Interactively, prompt for TEXT."
  (interactive "MText: ")
  (let* ((branch (git-tools-current-branch-name))
          (output (concat
                    (format "In %s, under the current branch %s, please make this change: %s "
                      default-directory branch text)
                    (format "commit it with only title message, update entire stack as needed, ")
                    (format "and push --force-with-lease as needed."))))
    (with-temp-buffer
      (insert output)
      (caveman-copy-region (point-min) (point-max )))
    (message "yak-update-stacked-request: current branch %s in %s" branch default-directory)))

;;;autoload
(defun yak-self-review-and-update ()
  "Copy a self code review and update prompt into the kill ring."
  (interactive "")
  (let* ((branch (git-tools-current-branch-name))
          (output (concat
                    (format "In %s, under the current branch %s, please review this code. "
                      default-directory branch)
                    (format "Make uncommitted changes for your comments."))))
    (kill-new output)
    (message "yak-review: current branch %s in %s" branch default-directory)))

;;;autoload
(defun yak-commit (text)
  "Copy a commit prompt for TEXT onto the kill ring.

The prompt is scoped to `git-tools-review-directory' and the
current branch in that repository.  TEXT is used as a suggested
commit title; a short body is requested only if necessary.

Interactively, prompt for TEXT."
  (interactive "MText: ")
  (let* ((ctx (yak--directory-and-branch))
          (dir (car ctx))
          (branch (cdr ctx))
          (output (concat
                    (format "In %s, under the current branch %s, " dir branch)
                    (format "please commit the changes with a title similar to \"%s\" and a " text)
                    "short concise body only if it is necessary.")))
    (kill-new output)
    (message "yak-commit: current branch %s in %s" branch dir)))

;;;###autoload
(defun yak-review-pull-request (arg)
  "Copy a pull-request review prompt onto the kill ring.

Uses `git-tools-review-directory' as the repository (the parallel
review clone under `git-tools-review-home') and the current
branch in that repository.

The prompt refers to the latest commit, or the latest N commits,
where N is `git-tools-commits-ahead-of-main' — the number of
commits on the current branch that are not in the repository's
main branch (`git-tools-main-branch-name', typically `main' or
`master').

With a numeric prefix ARG, use that count instead of the
computed value.

The prompt asks for a concise approval or changes-requested
verdict for breaking issues, plus a short list of improvement
comments if applicable."
  (interactive "P")
  (let* ((ctx (yak--directory-and-branch))
          (dir (car ctx))
          (branch (cdr ctx))
          (count (if arg
                   (prefix-numeric-value arg)
                   (git-tools-commits-ahead-of-main dir branch)))
          (scope (if (> count 1)
                   (format "%d commits" count)
                   "commit"))
          (output (concat
                    (format "In %s, under the current branch %s, " dir branch)
                    (format "please review the latest %s for the current branch %s" scope branch)
                    ", providing a concise approval or changes requested verdict only for breaking issues. "
                    "Also provide a short list of a few comments for improvement if applicable.")))
    (kill-new output)
    (message "yak-review-pull-request: current branch %s in %s (%s)"
      branch dir scope)))

;;;###autoload
(defun yak-review-respond-to-comment (text)
  "Copy a review-comment response prompt for TEXT onto the kill ring.

Uses `git-tools-review-directory' as the repository (the parallel
review clone under `git-tools-review-home') and the current
branch in that repository.  TEXT is the review comment to answer.

Interactively, prompt for TEXT."
  (interactive "MText: ")
  (let* ((ctx (yak--directory-and-branch))
          (dir (car ctx))
          (branch (cdr ctx))
          (output (concat
                    (format "In %s, " dir)
                    (format "concerning the current branch %s, please " branch)
                    (format "paste a reasonable concise response to this review comment %s." text))))
    (kill-new output)
    (message "yak-review-respond-to-comment: current branch %s in %s" branch dir)))

;;;###autoload
(defun yak-update-for-comment (text)
  "Copy an implementation-update prompt for TEXT onto the kill ring.

The prompt is scoped to `git-tools-review-directory' and the
current branch in that repository.  TEXT is the review comment to
implement.

Interactively, prompt for TEXT."
  (interactive "MText: ")
  (let* ((ctx (yak--directory-and-branch))
          (dir (car ctx))
          (branch (cdr ctx))
          (output (concat
                    (format "In %s, " dir)
                    (format "concerning the current branch %s, please " branch)
                    (format "make an update to the implementation based on this review comment: %s." text))))
    (kill-new output)
    (message "yak-update-for-comment: current branch %s in %s" branch dir)))

;;;###autoload
(defun yak-accept-pr-duplicates (&optional url)
  "Copy `/accept-pr-duplicates' from a duplicate-gate job URL.

Reads the report section of the GitHub Actions job log at URL,
collects `path:start-end' finding locations, and copies a compact
`/accept-pr-duplicates RANGE...' command to the kill ring and the
echo area as one line.

URL is a GitHub Actions job or run URL.  Interactively, the
clipboard is used when it already holds one; otherwise prompt.

A listed range may contain a finding location, so overlapping
detector spans on one file collapse, and each file becomes one
min-max span when that still leaves at least two arguments."
  (interactive)
  (let* ((url (or url (yak--github-actions-url-from-clipboard-or-prompt)))
          (parts (or (yak--github-actions-parts url)
                   (user-error "Not a GitHub Actions job URL: %s" url)))
          (owner (nth 0 parts))
          (repo (nth 1 parts))
          (run-id (nth 2 parts))
          (job-id (or (nth 3 parts)
                    (yak--pr-duplicate-delta-job-id owner repo run-id)))
          (log (yak--github-job-log owner repo job-id)))
    (when (string-match-p "additional PR-related findings are available" log)
      (user-error
        "Job log omitted some findings; download the duplicate-check artifact"))
    (let* ((found (yak--parse-pr-duplicate-ranges
                    (yak--pr-duplicate-report-excerpt log)))
            (ranges (yak--merge-pr-duplicate-ranges found)))
      (when (< (length ranges) 2)
        (user-error
          "Need at least two PR duplicate ranges; found %d in job %s"
          (length found) job-id))
      (let ((command (yak--one-line
                       (yak--format-accept-pr-duplicates ranges))))
        (kill-new command)
        (message "%s" command)
        command))))

(provide 'yak)
;;; yak.el ends here
