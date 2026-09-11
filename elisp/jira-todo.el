;;; jira-todo.el --- Generate org-mode TODO a1d Slack message for JIRA tickets -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Todd Ornett
;;
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: April 22, 2026
;; Modified: September 11, 2026
;; Version: 0.0.1
;; Keywords: jira, org, tools
;; Homepage: https://github-tao/toddaornett/dotconfig
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Fetches a JIRA ticket and generates an org-mode TODO entry and Slack message.
;; Requires the `request` package and the following variables to be set:
;;   jira-base-url, jira-issue-key-prefix, jira-username, jira-token
;;
;;; Code:

(require 'request)
(require 'json)
(require 'subr-x)
(require 'cl-lib)
(require 'org)
(require 'git-tools)

(defgroup jira-todo nil
  "Generate `org-mode' TODOs from JIRA tickets."
  :group 'tools)

(defcustom jira-todo-base-url
  (or (getenv "JIRA_ISSUE_BASE_URL") "https://atlassian.net")
  "Base URL for JIRA instance."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-issue-key-prefix
  (or (getenv "JIRA_ISSUE_KEY_PREFIX") "JIRA")
  "JIRA issue key prefix, e.g. JIRA."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-username
  (or (getenv "JIRA_USER") "")
  "JIRA username (email)."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-token
  (or (getenv "JIRA_TOKEN") "")
  "JIRA API token."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-pr-messaging-provider
  (or (getenv "MESSAGING_PROVIDER") "slack")
  "Determine the format of message format.
`slack' or `teams' (Microsoft Teams)"
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-git-directory
  (or (getenv "JIRA_TODO_GIT_DIRECTORY") "~/Projects")
  "Directory for creating git branch from todo."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-pr-reviewers
  (or (getenv "PULL_REQUEST_REVIEWERS") "")
  "GitHub PR reviewers for Slack or Teams message."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-github-user-map nil
  "Alist mapping git author emails to PTAL display names.
Each element is a cons cell (EMAIL . DISPLAY-NAME), both strings.
DISPLAY-NAME is the mention to write, typically \"@Full Name\"."
  :type '(alist :key-type (string :tag "Git author email")
           :value-type (string :tag "Display name"))
  :group 'jira-todo)

(defun jira-todo--string-equal-fold (a b)
  "Return non-nil if A and B are equal, ignoring case."
  (and (stringp a) (stringp b)
    (string= (downcase a) (downcase b))))

(defun jira-todo--email-local-part (email)
  "Return EMAIL's local part, stripping a plus-address tag."
  (when (and (stringp email) (not (string-empty-p email)))
    (car (split-string (car (split-string email "@")) "\\+"))))

(defun jira-todo--email-plus-stripped (email)
  "Return EMAIL with a plus-address tag removed from the local part."
  (when (and (stringp email)
          (string-match "\\`\\([^@+]+\\)\\(?:\\+[^@]*\\)?@\\(.+\\)\\'" email))
    (concat (match-string 1 email) "@" (match-string 2 email))))

(defun jira-todo-github-user-map-get (key)
  "Return the PTAL display name for KEY, or nil if unset.

KEY is a git author email.  Lookup is case-insensitive.  A
plus-address (user+tag@domain) also matches user@domain.  When
that fails, the local part is compared across domains so
gem.hung@levelblue.com matches gem.hung@cybereason.com.
Return nil when the map is unset or KEY is not present."
  (when (and key jira-todo-github-user-map)
    (or (alist-get key
          jira-todo-github-user-map
          nil nil #'jira-todo--string-equal-fold)
      (let ((stripped (jira-todo--email-plus-stripped key)))
        (and stripped
          (not (jira-todo--string-equal-fold stripped key))
          (alist-get stripped
            jira-todo-github-user-map
            nil nil #'jira-todo--string-equal-fold)))
      (let ((local (jira-todo--email-local-part key)))
        (when local
          (cl-some (lambda (cell)
                     (and (jira-todo--string-equal-fold
                            local (jira-todo--email-local-part (car cell)))
                       (cdr cell)))
            jira-todo-github-user-map))))))

(defun jira-todo-github-user-map-set (key display-name)
  "Set DISPLAY-NAME for KEY in the mapping, adding or updating."
  (setf (alist-get key
          jira-todo-github-user-map
          nil nil #'jira-todo--string-equal-fold)
    display-name))

(defun jira-todo-github-user-map-remove (key)
  "Remove KEY from the mapping, if present."
  (setf (alist-get key
          jira-todo-github-user-map
          nil 'remove #'jira-todo--string-equal-fold)
    nil))

(defun jira-todo--rest-url (issue-number)
  "Return the JIRA REST API URL for ISSUE-NUMBER."
  (format "%s/rest/api/3/issue/%s-%s"
    jira-todo-base-url
    jira-todo-issue-key-prefix
    issue-number))

(defun jira-todo--browse-url (key)
  "Return the JIRA browse URL for KEY."
  (format "%s/browse/%s" jira-todo-base-url key))

(defun jira-todo--auth-header ()
  "Return the Basic Auth header value."
  (concat "Basic "
    (base64-encode-string
      (concat jira-todo-username ":" jira-todo-token)
      t)))

(defun jira-todo--smart-open-line-above ()
  "Insert an empty line above the current line and indent it.
If in an `org-mode' buffer within a TODO, move point to a new line
immediately above the first sibling TODO under the parent heading."
  (if (derived-mode-p 'org-mode)
    (let ((target nil)
           (current-level (save-excursion
                            (org-back-to-heading t)
                            (org-current-level))))
      (save-excursion
        ;; Go up to parent heading
        (org-back-to-heading t)
        (when (org-up-heading-safe)
          (let ((parent-end (save-excursion (org-end-of-subtree t t))))
            (while (and (not target)
                     (re-search-forward org-heading-regexp parent-end t))
              (when (and (= (org-current-level) current-level)
                      (org-entry-is-todo-p))
                (setq target (line-beginning-position)))))))
      (if target
        (progn
          (goto-char target)
          (open-line 1))
        ;; Fallback: original behavior
        (move-beginning-of-line nil)
        (newline-and-indent)
        (forward-line -1)
        (indent-according-to-mode)))
    ;; Not in org-mode: original behavior
    (move-beginning-of-line nil)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)))

(defun jira-todo--format-slack-message (summary)
  "Format message with SUMMARY for Slack."
  (concat
    (format "Slack:\n")
    (format "--begin--\n")
    (format ":pull_request: PTAL %s\n" jira-todo-pr-reviewers)
    (format "PR <PR-TBD>\n")
    (format "%s\n" summary)
    (format "--end--\n")))

(defun jira-todo--format-teams-message (summary)
  "Format message with SUMMARY for Microsoft Teams."
  (concat
    (format "Teams:\n")
    (format "--begin--\n")
    (format "PTAL PR \n")
    (format "PTAL %s\n" jira-todo-pr-reviewers)
    (format "PR <PR-TBD>\n")
    (format "%s\n" summary)
    (format "--end--\n")))

(defun jira-todo--format-output (data)
  "Format `org-mode' TODO and message from parsed JIRA DATA."
  (let* ((key                 (format "%s" (alist-get 'key data)))
          (fields             (alist-get 'fields data))
          (summary            (format "%s" (alist-get 'summary fields)))
          (url                (jira-todo--key-to-browse-url key))
          (clean-summary      (replace-regexp-in-string "\\[[A-Z]+\\][ ]*" "" summary))
          (branch-words       (replace-regexp-in-string "[^A-Za-z0-9]+" "-" clean-summary))
          (branch-compact     (replace-regexp-in-string "-+" "-" branch-words))
          (branch-trimmed     (replace-regexp-in-string "-+$" "" branch-compact))
          (branch-normalized  (replace-regexp-in-string "_-+" "_" branch-trimmed))
          (branch-summ        (downcase branch-normalized))
          (branch             (format "%s_%s" key branch-summ)))
    (concat
      (format "*** TODO CR: %s %s\n" key clean-summary)
      (format "JIRA: [[%s][%s]]\n" url key)
      (format "Branch: %s\n" branch)
      (format "Git Directory: %s\n" jira-todo-git-directory)
      (format "Prompt:\n")
      (format "--begin--\n")
      (format "Under the %s directory in my current branch %s" jira-todo-git-directory branch)
      (format " please implement the JIRA at %s\n" url)
      (format "--end--\n")
      (format "Title: %s: %s\n" key clean-summary)
      (format "PR: <PR-TBD>\n")
      (format "PR Text:\n")
      (format "--begin--\n")
      (format "## JIRA\n")
      (format "[%s](%s)\n" key url)
      (format "## Description\n")
      (format "%s\n" clean-summary)
      (format "--end--\n")
      (pcase jira-todo-pr-messaging-provider
        ("slack" (jira-todo--format-slack-message clean-summary))
        ("teams" (jira-todo--format-teams-message clean-summary))
        (_ ""))
      (format ":LOGBOOK:\n")
      (format ":END:"))))

(defun jira-todo--parse-labeled-fields (text)
  "Parse TEXT for lines of the form \"Label: value\".
Return an alist of (LABEL . VALUE), both trimmed strings.  LABEL is
everything before the first colon on the line; VALUE is everything
after the first \": \" that follows it.  If a label appears more than
once, the first occurrence wins."
  (let ((fields nil)
         (case-fold-search nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*\\([^:\n]+\\): \\(.+\\)$" nil t)
        (let ((label (string-trim (match-string 1)))
               (value (string-trim (match-string 2))))
          (unless (assoc label fields)
            (push (cons label value) fields)))))
    (nreverse fields)))

(defun jira-todo--insert-output (data)
  "Insert formatted `org-mode' TODO for JIRA DATA at point in the current buffer."
  (let ((output (jira-todo--format-output data))
         (buf (current-buffer)))
    (with-current-buffer buf
      (jira-todo--smart-open-line-above)
      (let ((start (point)))
        (insert output)
        (goto-char start)
        (org-back-to-heading t)))
    (let* ((fields (jira-todo--parse-labeled-fields output))
            (branch (cdr (assoc "Branch" fields)))
            (title (cdr (assoc "Title" fields))))
      (when (fboundp 'evil-force-normal-state)
        (evil-force-normal-state))
      (cond
        ((or (null branch) (string-empty-p branch))
          (message "You must manually create branch, could not identify name."))
        ((git-tools-branch-create-from-main branch jira-todo-git-directory)
          (git-tools-empty-commit-message title jira-todo-git-directory))))))

(defun jira-todo--parse-input (input)
  "Parse INPUT into a JIRA key.

INPUT can be in any of the following forms:
- Full URL:  https://atlassian.net/browse/JIRA-11111
- Full key:  JIRA-11111
- Number:    11111

Returns the JIRA key as a string, e.g. JIRA-11111.
Signals an error if INPUT cannot be parsed."
  (cond
    ;; Full URL: https://atlassian.net/browse/JIRA-11111
    ((string-match "/browse/\\([A-Z]+-[0-9]+\\)" input)
      (match-string 1 input))
    ;; Full key: JIRA-11111
    ((string-match "^\\([A-Z]+-[0-9]+\\)$" input)
      (match-string 1 input))
    ;; Just a number: 11111
    ((string-match "^[0-9]+$" input)
      (format "%s-%s" jira-todo-issue-key-prefix input))
    (t
      (error "Could not parse JIRA issue from input: %s" input))))

(defun jira-todo--key-to-rest-url (key)
  "Return the JIRA REST API URL for KEY (e.g. JIRA-11111)."
  (format "%s/rest/api/3/issue/%s" jira-todo-base-url key))

(defun jira-todo--key-to-browse-url (key)
  "Return the JIRA browse URL for KEY."
  (format "%s/browse/%s" jira-todo-base-url key))

(defun jira-todo--clipboard-text ()
  "Return trimmed kill-ring/clipboard text, or nil if unavailable."
  (let ((text (ignore-errors (current-kill 0 t))))
    (when (and (stringp text) (not (string-empty-p (string-trim text))))
      (string-trim text))))

(defun jira-todo--http-url-p (text)
  "Return non-nil if TEXT is a single bare http(s) URL."
  (and (stringp text)
    (string-match-p "\\`https?://[^[:space:]]+\\'" text)))

(defun jira-todo--clipboard-jira-url ()
  "Return clipboard text when it names a JIRA issue, otherwise nil.

Accepts a browse URL, a key such as JIRA-11111, or a bare issue number."
  (let ((text (jira-todo--clipboard-text)))
    (when (and text (ignore-errors (jira-todo--parse-input text)))
      text)))

(defun jira-todo--clipboard-pr-url ()
  "Return the clipboard text when it is a non-JIRA http(s) URL, otherwise nil."
  (let ((url (jira-todo--clipboard-text)))
    (when (and (jira-todo--http-url-p url)
            (not (string-search jira-todo-issue-key-prefix url)))
      url)))

(defun jira-todo--heading-bounds ()
  "Return (START . END) of the current org heading subtree.

END is a marker.  Includes folded/invisible text."
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be called from an org-mode TODO"))
  (save-excursion
    (org-back-to-heading t)
    (cons (point)
      (copy-marker (save-excursion (org-end-of-subtree t t) (point))))))

(defun jira-todo--heading-text ()
  "Return the current org heading and its subtree as a string."
  (let* ((bounds (jira-todo--heading-bounds))
          (text (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (set-marker (cdr bounds) nil)
    text))

(defun jira-todo--replace-heading-text (new-text)
  "Replace the current org heading subtree with NEW-TEXT.
Reveal the heading so the edit is visible.  Return NEW-TEXT."
  (let ((bounds (jira-todo--heading-bounds))
         (search-invisible t))
    (save-excursion
      (delete-region (car bounds) (cdr bounds))
      (goto-char (car bounds))
      (insert new-text)
      (set-marker (cdr bounds) nil))
    (save-excursion
      (org-back-to-heading t)
      (cond
        ((fboundp 'org-fold-show-subtree) (org-fold-show-subtree))
        ((fboundp 'org-show-subtree) (org-show-subtree))
        (t (org-fold-show-entry))))
    new-text))

(defun jira-todo--heading-fields ()
  "Return labeled fields from the current org heading."
  (jira-todo--parse-labeled-fields (jira-todo--heading-text)))

(defun jira-todo--heading-field (label &optional fields)
  "Return the trimmed value of LABEL from FIELDS or the current heading."
  (let ((value (cdr (assoc label (or fields (jira-todo--heading-fields))))))
    (and (stringp value) (not (string-empty-p value)) value)))

(defun jira-todo--first-pr-url-in-text (text)
  "Return the first GitHub-style pull-request URL in TEXT, or nil."
  (when (and (stringp text)
          (string-match "https?://[^[:space:]]+/pull/[0-9]+" text))
    (match-string 0 text)))

(defun jira-todo--pr-url-owner-repo (url)
  "Return (OWNER . REPO) parsed from a pull-request URL, or nil."
  (when (and (stringp url)
          (string-match
            "/\\([^/]+\\)/\\([^/]+\\)/pull/[0-9]+"
            url))
    (cons (match-string 1 url)
      (replace-regexp-in-string "\\.git\\'" "" (match-string 2 url)))))

(defun jira-todo--heading-pr-url (&optional fields)
  "Return an already-filled PR URL with optional FIELDS from the current heading.

Prefers the PR labeled field, then the first /pull/ URL in the
heading text (Teams/Slack message body)."
  (or (let ((pr (jira-todo--heading-field "PR" fields)))
        (when (and pr
                (jira-todo--http-url-p pr)
                (not (string-match-p "<\\(PR-\\)?TBD>" pr)))
          pr))
    (jira-todo--first-pr-url-in-text
      (ignore-errors (jira-todo--heading-text)))))

(defun jira-todo--origin-matches-p (dir owner repo)
  "Return non-nil if DIR's origin is OWNER/REPO."
  (when-let* ((pair (ignore-errors
                      (git-tools--pr-owner-repo
                        (file-name-as-directory (file-truename dir))))))
    (and (string= (downcase (car pair)) (downcase owner))
      (string= (downcase (cdr pair)) (downcase repo)))))

(defun jira-todo--repo-has-branch-p (dir branch)
  "Return non-nil if DIR has BRANCH or origin/BRANCH."
  (let ((default-directory (file-name-as-directory dir)))
    (or (magit-branch-p branch)
      (magit-branch-p (concat "origin/" branch)))))

(defun jira-todo--git-directory-search-roots ()
  "Return directories to search for a matching local clone.

Only `jira-todo-git-directory' and `git-tools-review-home' are
considered.  The current buffer directory is not searched, so a
random repo (for example the org notes tree) cannot win over the
configured project roots."
  (let ((roots (delq nil
                 (list (and (stringp jira-todo-git-directory)
                         (not (string-empty-p jira-todo-git-directory))
                         (expand-file-name jira-todo-git-directory))
                   (and (boundp 'git-tools-review-home)
                     (stringp git-tools-review-home)
                     (not (string-empty-p git-tools-review-home))
                     (expand-file-name git-tools-review-home))))))
    (cl-delete-duplicates
      (mapcar (lambda (d) (directory-file-name (expand-file-name d))) roots)
      :test #'file-equal-p)))

(defun jira-todo--directory-candidates (repo)
  "Return local directories that might be a clone of REPO."
  (let (candidates)
    (dolist (root (jira-todo--git-directory-search-roots))
      (when (file-directory-p root)
        (push root candidates)
        (when (and repo (not (string-empty-p repo)))
          (dolist (name (cl-delete-duplicates
                          (list repo
                            (capitalize repo)
                            (upcase repo))
                          :test #'string=))
            (let ((child (expand-file-name name root)))
              (when (file-directory-p child)
                (push (directory-file-name child) candidates)))))
        (unless (git-tools--git-repo-p root)
          (dolist (child (directory-files root t "\\`[^.]"))
            (when (file-directory-p child)
              (push (directory-file-name child) candidates))))))
    (cl-delete-duplicates candidates :test #'file-equal-p)))

(defun jira-todo--find-repo-from-remote (owner repo branch)
  "Return a local clone matching OWNER/REPO and/or remote BRANCH.

Prefers a directory whose origin is OWNER/REPO and that has
BRANCH or origin/BRANCH.  Then origin only, then a repo that
has the remote branch.  Returns nil when nothing matches."
  (let (both origin-match branch-match)
    (dolist (dir (jira-todo--directory-candidates repo))
      (when (git-tools--git-repo-p dir)
        (let* ((origin-ok (and owner repo
                            (jira-todo--origin-matches-p dir owner repo)))
                (branch-ok (and branch
                             (not (string-empty-p branch))
                             (jira-todo--repo-has-branch-p dir branch))))
          (cond
            ((and origin-ok branch-ok) (setq both dir))
            ((and origin-ok (not origin-match)) (setq origin-match dir))
            ((and branch-ok (not branch-match)) (setq branch-match dir))))))
    (when-let* ((chosen (or both origin-match branch-match)))
      (directory-file-name (file-truename chosen)))))

(defun jira-todo--git-directory (&optional fields)
  "Return the git directory for the current heading with optional FIELDS.

Git Directory is optional.  Resolution order:
1. The heading Git Directory field, when present.
2. A local clone matching the PR URL's owner/repo and/or the
   heading Branch field (local or origin/BRANCH).
3. `jira-todo-git-directory'."
  (let* ((fields (or fields (ignore-errors (jira-todo--heading-fields))))
          (explicit (jira-todo--heading-field "Git Directory" fields))
          (pr (or (jira-todo--heading-pr-url fields)
                (ignore-errors (jira-todo--clipboard-pr-url))))
          (owner-repo (jira-todo--pr-url-owner-repo pr))
          (branch (jira-todo--heading-branch fields))
          (found (unless explicit
                   (jira-todo--find-repo-from-remote
                     (car owner-repo) (cdr owner-repo) branch))))
    (file-name-as-directory
      (expand-file-name
        (git-tools--ensure-project-directory
          (or explicit found jira-todo-git-directory))))))

(defun jira-todo--heading-branch (&optional fields)
  "Return the Branch field from the current heading, or nil with optional FIELDS."
  (jira-todo--heading-field "Branch" fields))

(defun jira-todo--fetch-remote-branch (branch root)
  "Fetch BRANCH from origin into ROOT.  Return non-nil on success."
  (let ((default-directory (file-name-as-directory root)))
    (zerop (call-process "git" nil nil nil "fetch" "origin" branch))))

(defun jira-todo--branch-rev (branch root)
  "Return a git revision for BRANCH in ROOT.

Prefer the local branch, then origin/BRANCH.  Fetch origin when
neither exists yet.  Return nil to mean HEAD."
  (let* ((default-directory (file-name-as-directory root))
          (origin (and branch (concat "origin/" branch)))
          (local-p (and branch (magit-branch-p branch)))
          (remote-p (and origin (magit-branch-p origin))))
    (cond
      (local-p branch)
      (remote-p origin)
      ((and branch (jira-todo--fetch-remote-branch branch root))
        (cond
          ((magit-branch-p branch) branch)
          ((magit-branch-p origin) origin)))
      (t nil))))

(defun jira-todo--resolve-pr-url (&optional url)
  "Return URL, else a clipboard PR URL, else the heading PR, else prompt.
Empty strings are treated as omitted so clipboard/heading/prompt still run.
When the current heading already has a PR URL, do not prompt."
  (let ((url (and (stringp url) (string-trim url))))
    (cond
      ((and url (not (string-empty-p url))) url)
      ((jira-todo--clipboard-pr-url))
      ((ignore-errors (jira-todo--heading-pr-url)))
      (t (let ((typed (string-trim (read-string "PR URL: "))))
           (when (string-empty-p typed)
             (user-error "No PR URL provided"))
           typed)))))

(defun jira-todo--replace-pr-placeholders (url)
  "Replace <PR-TBD> and <TBD> placeholders in the current org heading with URL.
Return the number of replacements, which may be zero when the PR
URL is already filled in.  Signal if point is not in an org heading.
Works even when the subtree is folded."
  (let* ((text (jira-todo--heading-text))
          (count 0)
          (new (replace-regexp-in-string
                 "<\\(PR-\\)?TBD>"
                 (lambda (_)
                   (setq count (1+ count))
                   url)
                 text t t)))
    (when (> count 0)
      (jira-todo--replace-heading-text new))
    count))

(defun jira-todo--file-parent-directory (file)
  "Return FILE's parent directory relative to the repo, or \".\"."
  (let ((dir (file-name-directory file)))
    (if (or (null dir) (string-empty-p dir))
      "."
      (directory-file-name dir))))

(defconst jira-todo--project-markers
  '("Cargo.toml" "go.mod" "package.json" "pyproject.toml"
     "pom.xml" "build.gradle" "build.gradle.kts" "mix.exs"
     "Gemfile" "composer.json")
  "Filenames that mark a project directory for PTAL author lookup.")

(defun jira-todo--project-marker-in-directory-p (dir)
  "Return non-nil if DIR contains a `jira-todo--project-markers' file."
  (cl-some (lambda (name)
             (file-exists-p (expand-file-name name dir)))
    jira-todo--project-markers))

(defun jira-todo--project-directory-for-file (file root)
  "Return the repo-relative project directory for FILE under ROOT.

Walks from FILE's parent toward ROOT and stops at the nearest
directory that contains a `jira-todo--project-markers' file
\(so a crate Cargo.toml wins over a workspace Cargo.toml).  When
no marker is found, FILE's parent directory is used."
  (let* ((root (file-name-as-directory (expand-file-name root)))
          (full (expand-file-name file root))
          (dir (file-name-directory full))
          found)
    (while (and dir (not found)
             (string-prefix-p root
               (file-name-as-directory (expand-file-name dir))))
      (if (jira-todo--project-marker-in-directory-p dir)
        (setq found dir)
        (let ((parent (file-name-directory (directory-file-name dir))))
          (setq dir (and parent
                      (not (string= (file-name-as-directory parent)
                             (file-name-as-directory dir)))
                      parent)))))
    (if (not found)
      (jira-todo--file-parent-directory file)
      (let ((rel (directory-file-name (file-relative-name found root))))
        (if (or (string= rel ".") (string-empty-p rel)
              (string-prefix-p ".." rel))
          "."
          rel)))))

(defun jira-todo--author-directories-for-files (files root)
  "Return project directories covering FILES under ROOT.

Each file is assigned to `jira-todo--project-directory-for-file'.
When several projects are touched, keep only those with the
highest changed-file count so an incidental extra crate cannot
dominate the PTAL ranking."
  (let ((files (cl-remove-if (lambda (f)
                               (or (not (stringp f)) (string-empty-p f)))
                 files))
         (counts (make-hash-table :test #'equal)))
    (dolist (file files)
      (let ((dir (jira-todo--project-directory-for-file file root)))
        (puthash dir (1+ (or (gethash dir counts) 0)) counts)))
    (let (alist)
      (maphash (lambda (dir n) (push (cons dir n) alist)) counts)
      (when alist
        (let ((max-count (apply #'max (mapcar #'cdr alist))))
          (mapcar #'car
            (cl-remove-if-not (lambda (p) (= (cdr p) max-count)) alist)))))))

(defun jira-todo--common-prefix-components (lists)
  "Return the shared leading components of LISTS of strings."
  (when lists
    (let* ((min-len (apply #'min (mapcar #'length lists)))
            (i 0)
            (done nil))
      (while (and (not done) (< i min-len))
        (let ((elt (nth i (car lists))))
          (if (cl-every (lambda (lst) (string= (nth i lst) elt)) (cdr lists))
            (setq i (1+ i))
            (setq done t))))
      (cl-subseq (car lists) 0 i))))

(defun jira-todo--group-files-by-first-component (files)
  "Group FILES by their first path component."
  (let ((table (make-hash-table :test #'equal))
         groups)
    (dolist (file files)
      (let ((key (or (car (split-string file "/" t)) ".")))
        (puthash key (cons file (gethash key table)) table)))
    (maphash (lambda (_key grouped)
               (push (nreverse grouped) groups))
      table)
    groups))

(defun jira-todo--common-parent-directories (files)
  "Return common parent directories covering FILES.

If FILES share a directory prefix, return that directory.  Otherwise
split by first path component and recurse so each cluster is passed
to `git-tools-authors-list' independently."
  (let ((files (cl-remove-if (lambda (f)
                               (or (not (stringp f)) (string-empty-p f)))
                 files)))
    (cond
      ((null files) nil)
      ((= (length files) 1)
        (list (jira-todo--file-parent-directory (car files))))
      (t
        (let* ((parts (mapcar (lambda (f) (split-string f "/" t)) files))
                (prefix (jira-todo--common-prefix-components parts)))
          (cond
            ((and prefix
               (cl-some (lambda (p) (> (length p) (length prefix))) parts))
              (list (string-join prefix "/")))
            ((and prefix
               (cl-every (lambda (p) (= (length p) (length prefix))) parts))
              (list (jira-todo--file-parent-directory (car files))))
            (t
              (cl-delete-duplicates
                (mapcan #'jira-todo--common-parent-directories
                  (jira-todo--group-files-by-first-component files))
                :test #'string=))))))))

(defun jira-todo--username-from-email (email)
  "Return a git/GitHub username derived from EMAIL."
  (let ((local (car (split-string email "@"))))
    (if (string-match-p "\\+" local)
      (car (last (split-string local "\\+")))
      local)))

(defun jira-todo--author-email (author-string)
  "Extract an email from AUTHOR-STRING.

Accepts \"Name <email>\", \"email (name)\", or a bare email."
  (cond
    ((and (stringp author-string)
       (string-match "<\\([^<>[:space:]]+@[^<>[:space:]]+\\)>" author-string))
      (string-trim (match-string 1 author-string)))
    ((and (stringp author-string)
       (string-match "\\`\\([^[:space:]]+@[^[:space:]]+\\)" author-string))
      (string-trim (match-string 1 author-string)))
    ((and (stringp author-string)
       (string-match "\\([^[:space:]]+@[^[:space:]]+\\)" author-string))
      (string-trim (match-string 1 author-string)))))

(defun jira-todo--author-username (author-string)
  "Extract a git username from AUTHOR-STRING."
  (if-let* ((email (jira-todo--author-email author-string)))
    (jira-todo--username-from-email email)
    author-string))

(defun jira-todo--author-display-name (author-string)
  "Return a human display name from AUTHOR-STRING, or nil."
  (cond
    ((and (stringp author-string)
       (string-match "\\`\\([^[:space:]]+\\) (\\(.*\\))\\'" author-string))
      (let ((name (string-trim (match-string 2 author-string))))
        (unless (string-empty-p name) name)))
    ((and (stringp author-string)
       (string-match "\\`\\(.+\\)[ \t]+<[^>]+>\\'" author-string))
      (let ((name (string-trim (match-string 1 author-string))))
        (unless (string-empty-p name) name)))))

(defun jira-todo--ensure-at-mention (name)
  "Return NAME with a leading `@' and surrounding whitespace trimmed."
  (let ((name (string-trim (or name ""))))
    (cond
      ((string-empty-p name) nil)
      ((string-prefix-p "@" name) name)
      (t (concat "@" name)))))

(defun jira-todo--mapped-display-name (email)
  "Return the mapped PTAL display name for EMAIL, or nil.

EMAIL is compared case-insensitively against
`jira-todo-github-user-map'.  Returns nil when the map or EMAIL
is unset, or when EMAIL is not a key.  A leading `@' is added
when the map value does not already have one."
  (when (and email jira-todo-github-user-map)
    (let ((mapped (jira-todo-github-user-map-get email)))
      (and mapped (jira-todo--ensure-at-mention mapped)))))

(defun jira-todo--format-ptal-mention (author-string)
  "Format AUTHOR-STRING for a PTAL mention.

If AUTHOR-STRING contains an email that is a key in
`jira-todo-github-user-map', return that map value and never the
email.  Accepts \"Name <email>\", \"@Name <email>\",
\"email (name)\", or a bare email.

If the map is unset or has no entry, use the original email
name: the name that accompanies the email, without the address.
The result always has a leading `@'."
  (when (consp author-string)
    (setq author-string (car author-string)))
  (let* ((author-string (and (stringp author-string) author-string))
          (email (jira-todo--author-email author-string))
          (mapped (jira-todo--mapped-display-name email))
          (original (jira-todo--author-display-name author-string)))
    (jira-todo--ensure-at-mention
      (or mapped original email author-string))))

(defun jira-todo--apply-email-map-to-text (text)
  "Replace mapped emails in TEXT with `jira-todo-github-user-map' values.

Rewrites \"@Name <email>\", \"Name <email>\", \"email (name)\",
and leftover bare emails.  Unmapped emails are left unchanged."
  (let ((text (or text "")))
    (dolist (email (jira-todo--emails-in-text text))
      (when-let* ((mapped (jira-todo--mapped-display-name email)))
        (setq text (jira-todo--replace-mapped-email text email mapped))))
    text))

(defun jira-todo--emails-in-text (text)
  "Return unique email addresses found in TEXT."
  (let ((text (or text ""))
         emails start)
    (while (string-match
             "\\([A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]\\{2,\\}\\)"
             text (or start 0))
      (let ((email (match-string 1 text)))
        (unless (member (downcase email) (mapcar #'downcase emails))
          (push email emails))
        (setq start (match-end 0))))
    (nreverse emails)))

(defun jira-todo--replace-mapped-email (text email mapped)
  "Replace EMAIL (and its surrounding name) in TEXT with MAPPED."
  (let ((q (regexp-quote email)))
    (dolist (re (list
                  (concat "@[^@<\n]+[ \t]+<" q ">")
                  (concat "[^@<\n]+[ \t]+<" q ">")
                  (concat "<" q ">")
                  (concat q "[ \t]+([^)]*)")
                  q))
      (setq text (replace-regexp-in-string re mapped text t t)))
    text))

(defun jira-todo--effective-pr-reviewers ()
  "Return reviewer text from the custom or the environment.

GUI Emacs often lacks `PULL_REQUEST_REVIEWERS' at load time
because it is not copied by `exec-path-from-shell'.  Re-read it
here, also accepting `GITHUB_PULL_REQUEST_REVIEWERS'."
  (let ((custom (and (stringp jira-todo-pr-reviewers)
                  (not (string-empty-p jira-todo-pr-reviewers))
                  jira-todo-pr-reviewers)))
    (or custom
      (getenv "PULL_REQUEST_REVIEWERS")
      (getenv "GITHUB_PULL_REQUEST_REVIEWERS")
      "")))

(defun jira-todo--split-at-mentions (text)
  "Split TEXT on `@' mentions, trimming each name.

A leading token without `@' is kept.  Mentions run until the
next `@' so names such as \"@Xin Tang\" stay intact."
  (let ((text (string-trim (or text "")))
         names)
    (unless (string-empty-p text)
      (when (string-match "\\`\\([^@]+\\)" text)
        (let ((lead (string-trim (match-string 1 text))))
          (unless (string-empty-p lead)
            (push lead names)))
        (setq text (substring text (match-end 0))))
      (while (string-match "\\`@\\([^@]+\\)" text)
        (let ((name (string-trim (match-string 1 text))))
          (unless (string-empty-p name)
            (push (concat "@" name) names)))
        (setq text (substring text (match-end 0)))))
    (nreverse names)))

(defun jira-todo--split-pr-reviewer-names (&optional reviewers)
  "Split REVIEWERS into display-name tokens.

REVIEWERS defaults to `jira-todo--effective-pr-reviewers'
(`PULL_REQUEST_REVIEWERS' / `GITHUB_PULL_REQUEST_REVIEWERS').
Names may be separated by commas and/or `@' mentions.  Each
token is trimmed.  Emails are rewritten via
`jira-todo-github-user-map' when present."
  (let* ((text (string-trim (or reviewers (jira-todo--effective-pr-reviewers) "")))
          names)
    (unless (string-empty-p text)
      (dolist (chunk (split-string text "," t))
        (let ((chunk (string-trim chunk)))
          (unless (string-empty-p chunk)
            (setq names
              (nconc names
                (if (string-match-p "@" chunk)
                  (jira-todo--split-at-mentions chunk)
                  (list chunk))))))))
    (delq nil
      (mapcar (lambda (name)
                (let* ((name (string-trim name))
                        (email (jira-todo--author-email name))
                        (mapped (and email (jira-todo-github-user-map-get email))))
                  (jira-todo--ensure-at-mention (or mapped name))))
        names))))

(defun jira-todo--normalize-ptal-name (name)
  "Normalize NAME for uniqueness comparison.

Trims, drops a leading `@', treats `.' and `_' as spaces,
collapses whitespace, downcases, and sorts name tokens.  So
\"@Shashank Vangari\" and \"@Shashank.Vangari\" compare equal,
and \"@Xie Zirui\" matches \"@Zirui Xie\"."
  (let ((name (string-trim (or name ""))))
    (setq name (replace-regexp-in-string "\\`@" "" name))
    (setq name (replace-regexp-in-string "[._]+" " " name))
    (setq name (replace-regexp-in-string "[ \t]+" " " name))
    (setq name (downcase (string-trim name)))
    (mapconcat #'identity (sort (split-string name) #'string-lessp) " ")))

(defun jira-todo--merge-ptal-names (auto-names extra-names)
  "Return AUTO-NAMES followed by EXTRA-NAMES not already rendered.

Each display name appears once.  Comparison is case-insensitive
and ignores a leading `@' and `.'/ `_' separators.  AUTO-NAMES
keep their original order; EXTRA-NAMES that are new are appended
in their original order."
  (let ((seen (make-hash-table :test #'equal))
         merged)
    (dolist (name (append auto-names extra-names))
      (let* ((name (jira-todo--ensure-at-mention name))
              (key (jira-todo--normalize-ptal-name name)))
        (unless (or (null name) (string-empty-p key) (gethash key seen))
          (puthash key t seen)
          (push name merged))))
    (nreverse merged)))

(defun jira-todo--author-identity-keys (author-string)
  "Return identity keys used to merge AUTHOR-STRING with aliases.

Keys are drawn from the mapped PTAL name, the git display name,
and the email local-part (plus-tags and case ignored)."
  (let* ((email (jira-todo--author-email author-string))
          (mapped (jira-todo--mapped-display-name email))
          (display (jira-todo--author-display-name author-string))
          keys)
    (when mapped
      (push (concat "n:" (jira-todo--normalize-ptal-name mapped)) keys))
    (when display
      (push (concat "n:" (jira-todo--normalize-ptal-name display)) keys))
    (when-let* ((local (jira-todo--email-local-part email)))
      (push (concat "l:" (downcase local)) keys))
    keys))

(defun jira-todo--self-identity-keys (&optional root)
  "Return identity keys for the current git user in ROOT."
  (let* ((default-directory (file-name-as-directory
                              (or root default-directory)))
          (git-email (ignore-errors (git-tools-git-config-value "user.email")))
          (git-name (ignore-errors (git-tools-git-config-value "user.name")))
          (email (or git-email user-mail-address))
          (name (or git-name user-full-name)))
    (jira-todo--author-identity-keys
      (git-tools--author-display name email))))

(defun jira-todo--identity-keys-overlap-p (a b)
  "Return non-nil if identity key lists A and B share an element."
  (cl-some (lambda (k) (member k b)) a))

(defun jira-todo--choose-author-string (a a-lines b b-lines)
  "Pick a representative author string from A and B.
Prefer a mapped PTAL email; otherwise the string with more lines."
  (let ((a-mapped (jira-todo--mapped-display-name (jira-todo--author-email a)))
         (b-mapped (jira-todo--mapped-display-name (jira-todo--author-email b))))
    (cond
      ((and a-mapped (not b-mapped)) a)
      ((and b-mapped (not a-mapped)) b)
      ((> b-lines a-lines) b)
      (t a))))

(defun jira-todo--merge-author-line-counts (entries)
  "Merge ENTRIES (AUTHOR-STRING . LINES) that represent the same person.

Same person means the same mapped PTAL name, the same display
name, or the same email local-part (plus-tags ignored, case
ignored)."
  (let (groups)
    (dolist (entry entries)
      (let* ((author (car entry))
              (lines (or (cdr entry) 0))
              (keys (jira-todo--author-identity-keys author))
              matches others)
        (dolist (g groups)
          (if (jira-todo--identity-keys-overlap-p keys (nth 0 g))
            (push g matches)
            (push g others)))
        (let ((best-author author)
               (best-lines lines)
               (total lines)
               (all-keys keys))
          (dolist (g matches)
            (setq all-keys (cl-delete-duplicates
                             (append all-keys (nth 0 g)) :test #'equal)
              total (+ total (nth 3 g))
              best-author (jira-todo--choose-author-string
                            best-author best-lines (nth 1 g) (nth 2 g))
              best-lines (if (string= best-author (nth 1 g))
                           (nth 2 g)
                           best-lines)))
          (setq groups (cons (list all-keys best-author best-lines total)
                         others)))))
    (mapcar (lambda (g) (cons (nth 1 g) (nth 3 g))) groups)))

(defun jira-todo--exclude-self-authors (entries &optional root)
  "Drop ENTRIES whose identity matches the git user in ROOT."
  (let ((self (jira-todo--self-identity-keys root)))
    (if (null self)
      entries
      (cl-remove-if
        (lambda (entry)
          (jira-todo--identity-keys-overlap-p
            (jira-todo--author-identity-keys (car entry)) self))
        entries))))

(defun jira-todo--ptal-reviewer-line ()
  "Return the merged PTAL reviewer line, or nil if none.

Top 5 git authors for the project (crate) with the most changed
files come first, formatted via `jira-todo--format-ptal-mention'.
Display names from `jira-todo-pr-reviewers' are appended when
they are not already present."
  (let* ((authors (condition-case err
                    (jira-todo--top-authors-for-changed-dirs 5)
                    (error
                      (message "jira-todo: could not list PTAL authors: %s"
                        (error-message-string err))
                      nil)))
          (auto (mapcar #'jira-todo--format-ptal-mention authors))
          (extra (jira-todo--split-pr-reviewer-names))
          (merged (jira-todo--merge-ptal-names auto extra)))
    (when merged
      (jira-todo--apply-email-map-to-text
        (mapconcat #'identity merged " ")))))

(defun jira-todo--existing-authors-directory (dir root)
  "Return DIR under ROOT if it exists, else the nearest existing ancestor."
  (let* ((root (file-name-as-directory (expand-file-name root)))
          (dir (if (string= dir ".")
                 root
                 (file-name-as-directory (expand-file-name dir root)))))
    (while (and dir
             (not (file-directory-p dir))
             (not (string= dir root))
             (string-prefix-p root dir))
      (setq dir (file-name-as-directory
                  (file-name-directory (directory-file-name dir)))))
    (when (file-directory-p dir)
      dir)))

(defun jira-todo--changed-files-against-main (&optional root branch)
  "Return files changed on BRANCH versus the main-branch merge-base.

ROOT defaults to `jira-todo--git-directory'.  BRANCH defaults to
the heading Branch field.  When BRANCH is missing locally, use
origin/BRANCH (fetching if needed).  When no branch can be
resolved, fall back to HEAD (`MAIN...')."
  (let* ((default-directory (or root (jira-todo--git-directory)))
          (main (git-tools-main-branch-name default-directory))
          (rev (jira-todo--branch-rev
                 (or branch (jira-todo--heading-branch))
                 default-directory))
          (range (concat main "..." (or rev ""))))
    (unless main
      (user-error "Could not determine main branch for repo in %s"
        default-directory))
    (or (ignore-errors
          (magit-git-items "diff" "-z" "--name-only" range))
      (with-temp-buffer
        (unless (zerop (call-process "git" nil t nil
                         "diff" "-z" "--name-only" range))
          (user-error "Call git diff %s failed in %s" range default-directory))
        (split-string (buffer-string) "\0" t)))))

(defun jira-todo--top-authors-for-changed-dirs (&optional limit)
  "Return the top LIMIT git author strings for changed-file projects.

LIMIT defaults to 5.  Files from `jira-todo--changed-files-against-main'
are grouped by nearest project directory (crate Cargo.toml, go.mod,
package.json, ...).  When more than one project is touched, only the
project(s) with the most changed files are used, so a one-file
sidecar crate cannot rank authors from a sibling workspace.

Calls `git-tools-authors-list' (default sort: lines changed,
descending) on each kept directory, merges aliases of the same
person, drops the current git user, then returns the top LIMIT
author strings in that order."
  (let* ((limit (or limit 5))
          (default-directory (jira-todo--git-directory))
          (files (jira-todo--changed-files-against-main default-directory))
          (dirs (jira-todo--author-directories-for-files
                  files default-directory))
          (merged (make-hash-table :test #'equal)))
    (dolist (dir dirs)
      (when-let* ((abs (jira-todo--existing-authors-directory
                         dir default-directory))
                   (dir-authors (git-tools-authors-list abs)))
        (dolist (entry dir-authors)
          (let* ((author (car entry))
                  (lines (cdr entry))
                  (prev (gethash author merged)))
            (puthash author (+ lines (or prev 0)) merged)))))
    (let (alist)
      (maphash (lambda (author lines)
                 (push (cons author lines) alist))
        merged)
      (setq alist
        (jira-todo--exclude-self-authors
          (jira-todo--merge-author-line-counts alist)
          default-directory))
      (setq alist
        (sort alist
          (lambda (a b)
            (if (= (cdr a) (cdr b))
              (string-lessp (car a) (car b))
              (> (cdr a) (cdr b))))))
      (mapcar #'car (cl-subseq alist 0 (min limit (length alist)))))))

(defun jira-todo--ptal-replacement-line (line reviewers)
  "Return LINE rewritten with REVIEWERS, or nil to leave LINE unchanged."
  (when (string-match
          "\\`\\([ \t]*\\)\\(:pull_request: \\)?PTAL\\(?:[ \t]+\\(.*\\)\\)?[ \t]*\r?\\'"
          line)
    (let ((indent (or (match-string 1 line) ""))
           (prefix (or (match-string 2 line) ""))
           (rest (or (match-string 3 line) "")))
      (unless (string-match-p "\\`PR[ \t]*\\'" rest)
        (concat indent prefix "PTAL " reviewers)))))

(defun jira-todo--insert-ptal-in-message-block (text reviewers)
  "Insert a PTAL line with REVIEWERS into the Teams/Slack block in TEXT.
Return (NEW-TEXT . COUNT)."
  (let ((count 0)
         (new text))
    (setq new
      (replace-regexp-in-string
        "\\(\\(?:Teams\\|Slack\\):\n--begin--\n\\)"
        (lambda (m)
          (setq count (1+ count))
          (concat m "PTAL " reviewers "\n"))
        new t t))
    (when (zerop count)
      (setq new
        (replace-regexp-in-string
          "\\(--begin--\n\\)"
          (lambda (m)
            (setq count (1+ count))
            (concat m "PTAL " reviewers "\n"))
          new t t)))
    (cons new count)))

(defun jira-todo--messaging-section-body (&optional text)
  "Return the Teams/Slack message body from TEXT or the current heading.

The body is the lines between the first `--begin--' and `--end--'
under a `Teams:' or `Slack:' label.  Those marker lines are not
included.  Prompt and PR Text blocks are ignored."
  (let ((in-section nil)
         (in-body nil)
         body)
    (dolist (line (split-string (or text (jira-todo--heading-text)) "\n" nil))
      (cond
        ((and (not in-body)
           (string-match-p "\\`[ \t]*\\(?:Teams\\|Slack\\):[ \t]*\\'" line))
          (setq in-section t))
        ((and in-section (not in-body)
           (string-match-p "\\`[ \t]*--begin--[ \t]*\\'" line))
          (setq in-body t))
        ((and in-body
           (string-match-p "\\`[ \t]*--end--[ \t]*\\'" line))
          (setq in-body nil
            in-section nil))
        (in-body
          (push line body))))
    (when body
      (mapconcat #'identity (nreverse body) "\n"))))

(defun jira-todo--copy-messaging-section (&optional text)
  "Copy the Teams/Slack `--begin--'/`--end--' body to the kill ring.
Return the copied text, or nil if no such section exists."
  (when-let* ((body (jira-todo--messaging-section-body text)))
    (kill-new body)
    body))

(defun jira-todo--replace-ptal-reviewers (reviewers)
  "Replace PTAL reviewer lists in the current org heading with REVIEWERS.
Leaves the Teams \"PTAL PR\" line unchanged.  If no PTAL line exists,
insert one under the Teams/Slack --begin-- marker.  Works when the
subtree is folded.  Return the number of replacements."
  (let* ((text (jira-todo--heading-text))
          (count 0)
          (new
            (mapconcat
              (lambda (line)
                (let ((rewritten (jira-todo--ptal-replacement-line line reviewers)))
                  (if rewritten
                    (progn (setq count (1+ count)) rewritten)
                    line)))
              (split-string text "\n" nil)
              "\n")))
    (when (zerop count)
      (let ((inserted (jira-todo--insert-ptal-in-message-block text reviewers)))
        (setq new (car inserted)
          count (cdr inserted))))
    (when (> count 0)
      (jira-todo--replace-heading-text new))
    count))

;;;###autoload
(defun jira-todo-fetch (&optional input)
  "Fetch a JIRA ticket and generate an `org-mode' TODO and Slack message.
INPUT can be a full URL, a key like JIRA-11111, or just an issue number.
If INPUT is not provided, prompt interactively."
  (interactive)
  (let* ((input (or (jira-todo--clipboard-jira-url) input
                  (and (not (called-interactively-p 'any)) nil)
                  (read-string "JIRA issue (URL, key, or number): ")))
          (key (jira-todo--parse-input input))
          (rest-url (jira-todo--key-to-rest-url key)))
    (request rest-url
      :headers `(("Accept"        . "application/json")
                  ("Authorization" . ,(jira-todo--auth-header)))
      :parser #'json-read
      :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (jira-todo--insert-output data)))
      :error (cl-function
               (lambda (&key error-thrown &allow-other-keys)
                 (message "Error fetching JIRA ticket: %S" error-thrown))))))

;;;###autoload
(defun jira-todo-update-with-pr (&optional url)
  "Update the current TODO with URL, clipboard, or then prompt for it.

Replace <PR-TBD> patterns in the current TODO when present.  An
already-filled PR URL in the heading is reused and is not an error.

Also rewrite the Teams/Slack PTAL reviewer line with the top 5 git
authors from `git-tools-authors-list' (default order: lines
changed) called on the nearest project/crate of files from
`git diff --name-only MAIN...BRANCH'.  BRANCH is the heading
Branch field when present (local, else origin/BRANCH after fetch);
otherwise HEAD.  Git Directory is optional: the heading field if
present, else a local clone matching the PR URL repo and/or the
remote Branch name, else `jira-todo-git-directory'.  Names from
`jira-todo-pr-reviewers' that are not already on the line are
appended.  After the heading is updated, copy the Teams/Slack
text between `--begin--' and `--end--' to the kill ring."
  (interactive)
  (let* ((search-invisible t)
          (url (jira-todo--resolve-pr-url url))
          (count (jira-todo--replace-pr-placeholders url))
          (ptal (or (jira-todo--apply-email-map-to-text
                      (jira-todo--ptal-reviewer-line))
                  (user-error
                    "Could not build a PTAL reviewer list (no authors and no PULL_REQUEST_REVIEWERS)")))
          (ptal-count (jira-todo--replace-ptal-reviewers ptal))
          (copied (jira-todo--copy-messaging-section)))
    (message "Updated %d PR placeholder(s), PTAL %s%s%s"
      count ptal
      (if (zerop ptal-count) " (heading not rewritten)" "")
      (if copied " (copied Teams/Slack message)" ""))
    count))

(provide 'jira-todo)
;;; jira-todo.el ends here
