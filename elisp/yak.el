;;; yak.el --- Message generators for agents and perhaps humans -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Todd Ornett
;;
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: July 28, 2026
;; Modified: September 14, 2026
;; Version: 0.0.1
;; Keywords: vc tools agent llm convenience
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/toddaornett/dotconfig
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'git-tools)

(defun yak--directory-and-branch ()
  "Return (DIRECTORY . BRANCH) for a yak prompt.
DIRECTORY is `git-tools-review-directory', which uses
`git-tools-review-home' when that override is set, otherwise the
current git project.  BRANCH is the current branch in DIRECTORY."
  (let ((dir (git-tools-review-directory)))
    (cons dir (git-tools-current-branch-name dir))))

;;;###autoload
(defun yak-implement (text)
  "Copy an implementation prompt for TEXT onto the kill ring.

The prompt is scoped to `git-tools-review-directory' and the
current branch in that repository.  TEXT describes the change to
make.

Interactively, prompt for TEXT."
  (interactive "MText: ")
  (let* ((ctx (yak--directory-and-branch))
          (dir (car ctx))
          (branch (cdr ctx))
          (output (format "In %s, under the current branch %s, please make this change: %s."
                    dir branch text)))
    (kill-new output)
    (message "yak-implement: current branch %s in %s" branch dir)))

;;;###autoload
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

Uses `git-tools-review-directory' as the repository (honoring
`git-tools-review-home' when that override is set) and the current
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

Uses `git-tools-review-directory' as the repository (honoring
`git-tools-review-home' when that override is set) and the current
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

(provide 'yak)
;;; yak.el ends here
