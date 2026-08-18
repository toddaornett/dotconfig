;;; git-tools.el --- Some git tools and workflows -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Todd Ornett
;;
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: April 02, 2025
;; Modified: August 18, 2026
;; Version: 0.0.1
;; Keywords: vc tools convenience files
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/toddaornett/dotconfig
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides some git tools and workflows.
;;
;;; Code:

(require 'magit)

;; Keep `user-full-name'/`user-mail-address' in sync with the current
;; repo's git config (falling back to Emacs' original values outside
;; a repo).  This matters because a few things read those variables
;; directly and need per-repo values, not one global identity:
;;   - the `emacs-lisp-mode' `__package' yasnippet template, which
;;     stamps `(user-full-name)'/`(user-email)' into Author/Maintainer
;;     and copyright header lines when scaffolding a new elisp file
;;   - `insert-random-uuid-into-buffer.el', which reads `(user-full-name)'
;; See `git-tools-set-user-from-git-or-default', added below to the
;; relevant hooks.

;; Save Emacs' original identity
(defvar git-tools-original-user-full-name user-full-name)
(defvar git-tools-original-user-mail-address user-mail-address)

;; Cache: (ROOT . (NAME . EMAIL))
(defvar git-tools-git-identity-cache (make-hash-table :test #'equal))

(defvar git-tools-review-home nil)

(defun git-tools--project-root ()
  "Return the root directory of the current git project, or nil if none.
Delegates to `magit-toplevel', which also handles worktrees and
submodules correctly."
  (when-let* ((root (magit-toplevel)))
    (file-name-as-directory root)))

;;;###autoload
(defun git-tools-project-name (path)
  "Return the git repo directory containing specified PATH."
  (when (stringp path)
    (let* ((clean-path (string-trim path))
           ;; Find the actual git root if a random file/folder was passed
           (true-root (or (vc-git-root clean-path)
                          (if (file-directory-p clean-path) clean-path (file-name-directory clean-path))))
           (root-dir (directory-file-name true-root))
           (basename (file-name-nondirectory root-dir)))

      ;; 1. Handle bare repositories ending in ".git"
      (if (string-suffix-p ".git" basename t)
          (file-name-sans-extension basename)

        ;; 2. Handle internal Git paths (like submodules or .git/config files)
        (if (string-prefix-p ".git" basename)
            (file-name-nondirectory (directory-file-name (file-name-directory root-dir)))

          ;; 3. Return the clean project folder name
          basename)))))

;;;###autoload
(defun git-tools-project-name (&optional path)
  "Return the git repo directory containing specified PATH.
If PATH is nil, use `default-directory`. When called interactively,
prompt for a directory and print the project name."
  (interactive (list (read-directory-name "Directory: " default-directory)))
  (let* ((target-path (or path default-directory))
          (clean-path (string-trim target-path))
          ;; Find the actual git root if a random file/folder was passed
          (true-root (or (vc-git-root clean-path)
                       (if (file-directory-p clean-path) clean-path (file-name-directory clean-path))))
          (root-dir (directory-file-name true-root))
          (basename (file-name-nondirectory root-dir))
          (result (if (string-suffix-p ".git" basename t)
                    (file-name-sans-extension basename)
                    (if (string-prefix-p ".git" basename)
                      (file-name-nondirectory (directory-file-name (file-name-directory root-dir)))
                      basename))))
    (when (called-interactively-p 'interactive)
      (message "Git working directory: %s" result))
    result))

(defun git-tools--project-relative-path (root)
  "The function gets the ROOT path relative to a natural base.

If ROOT is under the user's home directory, the base is `~', so the
result includes everything between the home directory and ROOT — e.g.
\"dev/Phoenix\" for ~/dev/Phoenix, or just \"Phoenix\" for ~/Phoenix,
or \"\" if ROOT is the home directory itself.

Otherwise, the base is `/', so the result is ROOT's absolute path with
the leading slash stripped — e.g. \"opt/repos/Phoenix\" for
/opt/repos/Phoenix."
  (let* ((root (directory-file-name (expand-file-name root)))
          (home (directory-file-name (expand-file-name "~"))))
    (cond
      ((string= root home) "")
      ((string-prefix-p (concat home "/") root)
        (substring root (1+ (length home))))
      (t (string-remove-prefix "/" root)))))

(defun git-tools--project-files (root)
  "Return absolute paths of all files tracked by git in repo ROOT.
Backed by `git ls-files' so it works without projectile or a
`project.el' backend that indexes the tree."
  (let ((default-directory root))
    (mapcar (lambda (f) (expand-file-name f root))
      (git-tools--nonempty-lines
        (shell-command-to-string "git ls-files")))))

(defun git-tools-git-config-value (key)
  "Return git config value for KEY in current repo, or nil."
  (when (and (not (file-remote-p default-directory))
          (locate-dominating-file default-directory ".git"))
    (let ((default-directory
            (locate-dominating-file default-directory ".git")))
      (condition-case nil
        (car (process-lines "git" "config" "--get" key))
        (error nil)))))

(defun git-tools-git-identity-for-root (root)
  "Return (NAME . EMAIL) for git repo ROOT, using cache."
  (or (gethash root git-tools-git-identity-cache)
    (let ((name  (git-tools-git-config-value "user.name"))
           (email (git-tools-git-config-value "user.email")))
      (let ((pair (cons name email)))
        (puthash root pair git-tools-git-identity-cache)
        pair))))

(defun git-tools-set-user-from-git-or-default ()
  "Set user identity from git config, or fall back to original values."
  (let ((root (and (not (file-remote-p default-directory))
                (locate-dominating-file default-directory ".git"))))
    (if root
      (pcase-let ((`(,name . ,email)
                    (git-tools-git-identity-for-root root)))
        (setq user-full-name (or name git-tools-original-user-full-name)
          user-mail-address (or email git-tools-original-user-mail-address)))
      ;; Not in a repo → restore defaults
      (setq user-full-name git-tools-original-user-full-name
        user-mail-address git-tools-original-user-mail-address))))

;; Update when opening files
(add-hook 'find-file-hook #'git-tools-set-user-from-git-or-default)

;; Refresh identity after switching projects with built-in `project.el'.
(defun git-tools--after-project-switch (&rest _)
  "Refresh git user identity after `project-switch-project'."
  (git-tools-set-user-from-git-or-default))

(advice-add 'project-switch-project :after #'git-tools--after-project-switch)

(defun git-tools-remote-origin-url ()
  "Return HTTPS URL derived from git remote origin."
  (when-let* ((default-directory (locate-dominating-file default-directory ".git"))
               (remote (string-trim
                         (shell-command-to-string
                           "git config --get remote.origin.url"))))
    (setq remote
      (cond
        ((string-match "\\`git@\\([^:]+\\):\\(.+\\)\\'" remote)
          (format "https://%s/%s"
            (match-string 1 remote)
            (match-string 2 remote)))
        (remote)))
    (replace-regexp-in-string "\\.git\\'" "" remote)))

(defvar git-tools-project-directory nil
  "Cached fallback git project directory.
Used by `git-tools-branch-create-from-main' when its DIR
argument (or `default-directory') is not inside a git
repository.  Set interactively the first time a prompt is
needed, and reused for the remainder of the Emacs session.")

(defun git-tools--git-repo-p (dir)
  "Return non-nil if DIR is inside a git repository."
  (let ((default-directory (file-name-as-directory dir)))
    (magit-toplevel)))

(defun git-tools--ensure-project-directory (dir)
  "Return a git repository directory derived from DIR.
DIR defaults to `default-directory' when nil.  If the
resulting directory is not inside a git repository, fall back
to `git-tools-project-directory' if that is still valid, or
else prompt for a directory via `read-directory-name' and
cache the answer in `git-tools-project-directory' for later
calls in this session."
  (let ((candidate (or dir default-directory)))
    (cond
      ((git-tools--git-repo-p candidate) candidate)
      ((and git-tools-project-directory
         (git-tools--git-repo-p git-tools-project-directory))
        git-tools-project-directory)
      (t
        (let ((chosen (read-directory-name
                        "Select git project directory: " nil nil t)))
          (unless (git-tools--git-repo-p chosen)
            (user-error "%s is not a git repository" chosen))
          (setq git-tools-project-directory chosen)
          chosen)))))

;;;###autoload
(defun git-tools-current-branch-name (&optional dir)
  "Return the current branch name for the repository in DIR (or current directory).
Return nil if DIR is not inside a git repository, if git is not
available, or if the repository is in a detached HEAD state.
When called interactively, also display the result in the echo area."
  (interactive)
  (let* ((default-directory (or dir default-directory))
          (git (executable-find "git"))
          (branch (when git
                    (with-temp-buffer
                      (if (zerop (call-process git nil t nil
                                   "rev-parse" "--abbrev-ref" "HEAD"))
                        (let ((b (string-trim (buffer-string))))
                          (if (or (string-empty-p b)
                                (string= b "HEAD"))
                            nil
                            b))
                        nil)))))
    (when (called-interactively-p 'interactive)
      (message (if branch
                 (format "Current branch: %s" branch)
                 "Not on a branch (detached HEAD or not a git repo)")))
    branch))

;;;###autoload
(defun git-tools-main-branch-name (&optional dir)
  "Return the main branch name for the repository in DIR (or current directory).
Tries several names or falls back to the default branch from git symbolic-ref.
When called interactively, also display the result in the echo area."
  (interactive)
  (let* ((default-directory (or dir default-directory))
          (branch (cond
                    ((magit-branch-p "main") "main")
                    ((magit-branch-p "master") "master")
                    ((magit-branch-p "develop") "develop")
                    ((magit-branch-p "trunk") "trunk")
                    (t (condition-case nil
                         (string-trim
                           (shell-command-to-string "git symbolic-ref refs/remotes/origin/HEAD | sed 's@^refs/remotes/origin/@@'"))
                         (error nil))))))
    (when (called-interactively-p 'interactive)
      (message (if branch
                 (format "Main branch: %s" branch)
                 "No main branch found")))
    branch))

;;;###autoload
(defun git-tools-branch-create-from-main (branch &optional dir)
  "Create BRANCH with starting point at main for repo in DIR.
DIR defaults to `default-directory' when nil.  If the
resulting directory is not a git repository, prompt for one
via `git-tools--ensure-project-directory' (see that function
for caching behavior).

Signal a `user-error' if the repository has uncommitted or
unstaged changes.  Otherwise, update the local main branch
from \"origin\", create BRANCH from it, and check out BRANCH."
  (interactive "sBranch name: ")
  (let* ((default-directory (git-tools--ensure-project-directory dir))
          (main-branch (git-tools-main-branch-name default-directory)))
    (unless main-branch
      (user-error "Could not determine main branch for repo in %s"
        default-directory))
    (when (magit-anything-modified-p)
      (user-error
        "Repository is not clean; commit or stash changes first"))
    (if (equal (magit-get-current-branch) main-branch)
      (magit-run-git "pull" "origin" main-branch)
      (magit-run-git "fetch" "origin"
        (format "%s:%s" main-branch main-branch)))
    (magit-run-git "checkout" "-b" branch main-branch)
    (message "Created and checked out `%s' from `%s'"
      branch main-branch)))

;;;###autoload
(defun git-tools-empty-commit-message (&optional message dir)
  "Create an empty commit with MESSAGE for repo in DIR.
MESSAGE defaults to \"chore: trigger CI\" when nil or empty.
DIR defaults to `default-directory' when nil.  If the
resulting directory is not a git repository, prompt for one
via `git-tools--ensure-project-directory' (see that function
for caching behavior).

This does not require or check for a clean working tree, since
an empty commit records no tree changes regardless of what else
is going on in the repo."
  (interactive
    (list (read-string "Commit message: " nil nil "chore: trigger CI")))
  (let ((default-directory (git-tools--ensure-project-directory dir))
         (message (if (and message (not (string-empty-p (string-trim message))))
                    message
                    "chore: trigger CI")))
    (if (string-equal (magit-get-current-branch) (git-tools-main-branch-name))
      (message (format "Error - will not create empty message on %s branch" (git-tools-main-branch-name)))
    (magit-run-git "commit" "--allow-empty" "-m" message)
    (message "Created empty commit: %s" message))))

(defun git-tools-discard-unstaged-changes (&optional parent-dir force)
  "Discard all unstaged commits in git subdirectories under PARENT-DIR.

If PARENT-DIR is nil, defaults to `~/Projects'. If FORCE is non-nil,
the function skips the confirmation prompt; otherwise, it asks for
confirmation for each repository with options: `y' (yes), `n' (no),
or `!' (yes to all remaining). The results are displayed in the
`*Git Discarded Unstaged Changes*' buffer. Untracked files and
staged changes are not affected."
  (interactive "P")
  (let* ((parent-dir (or parent-dir
                       (expand-file-name
                         (read-string (concat "Git discard unstaged changes in "
                                        "all sub-directories under path "
                                        "(default '~/Projects'): ")
                           nil
                           nil
                           "~/Projects"))))
          (default-directory (expand-file-name parent-dir))
          (dirs (directory-files default-directory nil "^[^.]" t)) ; Exclude . and ..
          (buffer (get-buffer-create "*Git Discarded Unstaged Changes*"))
          (discarded-dirs nil)
          (yes-to-all nil))
    (unless (file-directory-p default-directory)
      (error "Parent directory '%s' does not exist" default-directory))
    (dolist (dir dirs)
      (let ((full-path (expand-file-name dir default-directory)))
        (when (and (file-directory-p full-path)
                (not (file-symlink-p full-path))
                (file-exists-p (expand-file-name ".git" full-path)))
          (let ((status (shell-command-to-string
                          (format "cd %s && git status --porcelain" full-path))))
            (when (string-match-p "^.M" status)
              (let ((proceed
                      (or force
                        yes-to-all
                        (let ((response (read-string
                                          (format
                                            "Discard unstaged changes in %s? (y/n/!): "
                                            full-path))))
                          (cond
                            ((string= response "!") (setq yes-to-all t) t)
                            ((string-match-p "^[yY]" response) t)
                            (t nil))))))
                (when proceed
                  (let ((restore-result (shell-command
                                          (format "cd %s && git restore ." full-path))))
                    (if (= restore-result 0)
                      (setq discarded-dirs (cons dir discarded-dirs))
                      (message "Failed to discard changes in %s" full-path))))))))))
    (with-current-buffer buffer
      (erase-buffer)
      (if discarded-dirs
        (progn
          (insert (format "Discarded unstaged changes in %d git projects under %s:\n\n"
                    (length discarded-dirs)
                    parent-dir))
          (insert (mapconcat #'identity (sort discarded-dirs) "\n")))
        (insert "No git projects with unstaged changes were found."))
      (goto-char (point-min))
      (display-buffer buffer))))

(defun git-tools-show-untracked (&optional parent-dir)
  "List subdirectories under PARENT-DIR with untracked files.

List subdirectories under PARENT-DIR (default '~/Projects')
with files untracked by git. Displays results in a new buffer."
  (interactive)
  (let* ((parent-dir (or parent-dir
                       (expand-file-name
                         (read-string (concat "Git show untracked files in all "
                                        "sub-directories under path "
                                        "(default '~/Projects'): ")
                           nil
                           nil
                           "~/Projects"))))
          (default-directory (expand-file-name parent-dir))
          (dirs (directory-files default-directory
                  nil
                  "^[^.]"
                  t))  ; Exclude . and ..
          (buffer (get-buffer-create "*Git Untracked Files*"))
          (changed-dirs nil))
    ;; Ensure parent directory exists
    (unless (file-directory-p default-directory)
      (error "Parent directory '%s' does not exist" default-directory))
    ;; Check each subdirectory for untracked files
    (dolist (dir dirs)
      (let ((full-path (expand-file-name dir default-directory)))
        (when (and (file-directory-p full-path)
                (file-exists-p (expand-file-name ".git" full-path)))
          (let ((status (shell-command-to-string
                          (format
                            "cd %s && git status --porcelain --untracked-files=all"
                            full-path))))
            (when (string-match-p "^\\?\\?" status)
              (push dir changed-dirs))))))
    ;; Display results
    (with-current-buffer buffer
      (erase-buffer)
      (if changed-dirs
        (progn
          (insert (format "Files untracked by git in %d projects under the %s directory:\n\n"
                    (length changed-dirs)
                    parent-dir))
          (insert (mapconcat #'identity (sort changed-dirs) "\n")))
        (insert "No projects with files untracked by git were found."))
      (goto-char (point-min))
      (display-buffer buffer))))

(defun git-tools-show-unstaged (&optional parent-dir)
  "List subdirectories under PARENT-DIR with work.

List subdirectories under PARENT-DIR (default '~/Projects')
with unstaged Git changes. Displays results in a new buffer."
  (interactive)
  (let* ((parent-dir (or parent-dir
                       (expand-file-name
                         (read-string (concat "Git show unstaged commits in all "
                                        "sub-directories under path "
                                        "(default '~/Projects'): ")
                           nil nil "~/Projects"))))
          (default-directory (expand-file-name parent-dir))
          (dirs (directory-files default-directory nil "^[^.]" t)) ; Exclude . and ..
          (buffer (get-buffer-create "*Git Unstaged Files*"))
          (changed-dirs nil))
    ;; Ensure parent directory exists
    (unless (file-directory-p default-directory)
      (error "Parent directory '%s' does not exist" default-directory))
    ;; Check each subdirectory for Git changes
    (dolist (dir dirs)
      (let ((full-path (expand-file-name dir default-directory)))
        (when (and (file-directory-p full-path)
                (not (file-symlink-p full-path))
                (file-exists-p (expand-file-name ".git" full-path)))
          (let ((status (shell-command-to-string
                          (format "cd %s && git status --porcelain" full-path))))
            (unless (string-empty-p status)
              (push dir changed-dirs))))))
    ;; Display results
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Uncommitted git changes in %d projects under the %s directory:\n\n"
                (length changed-dirs)
                parent-dir))
      (if changed-dirs
        (insert (mapconcat #'identity (sort changed-dirs) "\n"))
        (insert "No git projects with uncommitted changes were found."))
      (goto-char (point-min))
      (display-buffer buffer))))

(defun git-tools-show-non-main-branches (&optional parent-dir)
  "List subdirectories under PARENT-DIR with non-main branches.

List subdirectories under PARENT-DIR (default '~/Projects')
with non-main branches. Displays results in a new buffer."
  (interactive)
  (let* ((parent-dir (or parent-dir
                       (expand-file-name
                         (read-string (concat "Git show non-main branches in all "
                                        "sub-directories under path "
                                        "(default '~/Projects'): ")
                           nil nil "~/Projects"))))
          (default-directory (file-truename (expand-file-name parent-dir)))
          (dirs (directory-files default-directory nil "^[^.]" t)) ; Exclude . and ..
          (buffer (get-buffer-create "*Git Non-main Branches*"))
          (results nil))
    ;; Ensure parent directory exists
    (unless (file-directory-p default-directory)
      (error "Parent directory '%s' does not exist" default-directory))
    ;; Check each subdirectory for non-main branches
    (dolist (dir dirs)
      (let ((full-path (expand-file-name dir default-directory)))
        (when (and (file-directory-p full-path)
                (not (file-symlink-p full-path))
                (file-exists-p (expand-file-name ".git" full-path)))
          (let* ((branch-names-raw (shell-command-to-string
                                     (format "cd %s && git branch --list" full-path)))
                  (branch-names (split-string branch-names-raw "\n" t))
                  (filtered-branches (mapcar
                                       (lambda (branch)
                                         (string-trim (string-replace "*" "" branch)))
                                       (seq-remove
                                         (lambda (branch)
                                           (let ((clean-branch (string-trim (string-replace "*" "" branch))))
                                             (or (string-empty-p clean-branch)
                                               (string= clean-branch "main")
                                               (string= clean-branch "master")
                                               (string= clean-branch "develop"))))
                                         branch-names))))
            (when filtered-branches
              (push (cons dir filtered-branches) results))))))
    ;; Display results
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Non-main git branches in %d projects under the %s directory:\n\n"
                  (length results)
                  parent-dir))
        (if results
          (dolist (entry (sort results (lambda (a b) (string< (car a) (car b)))))
            (insert (format "%s:\n" (car entry)))
            (dolist (branch (cdr entry))
              (insert (format "  - %s\n" branch))))
          (insert "No git projects with non-main branches found."))
        (goto-char (point-min))
        (display-buffer buffer)))))

(defun git-tools-pull-all-main (root-dir &optional force-pull)
  "Iterate through all subdirectories in ROOT-DIR and pull.
With prefix argument FORCE-PULL, pull even if there are unstaged changes.
Skips non-Git directories and symbolic links."
  (interactive "DDirectory: ~/Projects \nP")
  (let ((default-directory root-dir)
         (error-count 0)
         (skipped-repos nil)
         (success-repos nil))
    (dolist (dir (directory-files root-dir t "\\`[^.]"))
      (when (and (file-directory-p dir)
              (not (file-symlink-p dir))
              (file-exists-p (expand-file-name ".git" dir)))
        (let ((default-directory dir))
          (message "Processing: %s" dir)
          (let ((main-branch (git-tools-main-branch-name)))
            (if main-branch
              (condition-case err
                (progn
                  (message "Switching to %s in %s" main-branch dir)
                  (if (and (not force-pull) (magit-anything-unstaged-p))
                    (progn
                      (message "Skipping pull in %s: unstaged changes detected" dir)
                      (push dir skipped-repos))
                    (unless (string-equal (magit-get-current-branch) main-branch)
                      (magit-run-git "checkout" main-branch))
                    (if (magit-get-upstream-branch)
                      (progn
                        (magit-run-git "pull")
                        (message "Successfully pulled %s in %s" main-branch dir)
                        (push dir success-repos))
                      (message "Skipping pull in %s: no upstream branch configured" dir)
                      (push dir skipped-repos))))
                (error
                  (message "Error in %s: %s" dir (error-message-string err))
                  (setq error-count (1+ error-count))))
              (message "No valid main branch found in %s" dir)
              (push dir skipped-repos))))))
    (message "\n=== Pull Summary ===")
    (message "Successfully pulled: %d repos" (length success-repos))
    (dolist (repo success-repos)
      (message "  - %s" repo))
    (message "Skipped: %d repos" (length skipped-repos))
    (dolist (repo skipped-repos)
      (message "  - %s" repo))
    (if (> error-count 0)
      (message "Completed with %d errors" error-count)
      (message "Completed successfully with no errors"))))

(defun git-tools-update-yaml-file (current-file file variable value)
  "Set all instances of VARIABLE to VALUE in YAML FILE.
Scans FILE line by line for lines containing
VARIABLE. The next line must start
with `value: '. Replaces everything after `value: ' with
VALUE, preserving formatting. Edits FILE in place.
CURRENT-FILE is expected to prevent killing open buffer,

Return t if any changes made, nil otherwise."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (let ((found nil))
      (let ((variable-line (concat (regexp-quote variable) ":")))
        (while (re-search-forward variable-line nil t)
          (setq found t)
          (forward-line 1)
          (when (looking-at-p "^[ \t]*value: ")
            (re-search-forward "^[ \t]*value: " (line-end-position) t)
            (delete-region (point) (line-end-position))
            (insert value))))
      (when found
        (progn
          (save-buffer)
          (message "Updated YAML file: %s" (file-name-nondirectory file))))
      (unless (string= current-file file)
        (kill-buffer))
      found)))

(defun git-tools-set-yaml-and-commit (variable value)
  "Update VARIABLE to VALUE in the current YAML file and commit."
  (interactive
    (let* ((default-variable
             (or
               (when (and (eq major-mode 'yaml-mode)
                       (thing-at-point 'symbol))
                 (let ((symbol (thing-at-point 'symbol t)))
                   (if (string-match-p "^[a-zA-Z0-9_-]+$" symbol)
                     symbol
                     nil)))
               ""))
            (default-value
              (when (eq major-mode 'yaml-mode)
                (save-excursion
                  (forward-line 1)
                  (when (looking-at "[ \t]*value: \\(.*\\)")
                    (let ((val (string-trim (match-string 1))))
                      (cond
                        ((string= val "true") "false")
                        ((string= val "false") "true")
                        (t val)))))))
            (variable (read-string "Set variable: " default-variable))
            (value (read-string "Set to: " default-value)))
      (list variable value)))
  (let* ((project-root (or (git-tools--project-root)
                         (error "No project root found. Ensure a project is active")))
          (project-name (git-tools-project-name project-root))
          (project-files (git-tools--project-files project-root))
          (updates-made nil)
          (git-main-branch (git-tools-main-branch-name))
          (target-branch (concat "build/update-env-" (string-replace "_" "-" (downcase  variable)))))
    (message "Setting %s to %s in project %s" variable value project-name)

    ;; Check for any working files for git and make branch from main
    (when (and git-main-branch (not (magit-branch-p target-branch)))
      (progn
        (when (magit-anything-unstaged-p)
          (error "Commit or discard changes first"))
        (let ((process-buffer (magit-process-buffer)))
          (unless (string= (magit-get-current-branch) git-main-branch)
            (magit-run-git "checkout" git-main-branch))
          (let ((upstream (magit-get-upstream-branch)))
            (when upstream
              (magit-run-git "pull" "origin" git-main-branch)))
          (when (and process-buffer (buffer-live-p process-buffer))
            (kill-buffer process-buffer)))))

    ;; find yaml files in project to process
    (dolist (file project-files)
      (when (and (file-exists-p file)
              (string-equal-ignore-case
                (or (file-name-extension file) "")
                "yaml"))
        (when (git-tools-update-yaml-file (or buffer-file-name "") file variable value)
          (setq updates-made t))))

    ;; update branch and commit
    (if updates-made
      (let ((process-buffer (magit-process-buffer)))
        (unless (magit-branch-p target-branch)
          (magit-run-git "branch" target-branch git-main-branch))
        (unless (string-equal-ignore-case (magit-get-current-branch) target-branch)
          (magit-run-git "checkout" target-branch))
        (let ((commit-message (format "build(config): update %s\n\nset %s to %s" variable variable value)))
          (condition-case err
            (progn
              (magit-run-git "add" ".")
              (magit-run-git "commit" "-m" commit-message)
              (message "Committed changes with message 'build: update variable'"))
            (error
              (message "Failed to commit: %s" (error-message-string err))
              (magit-run-git "reset")))
          (when (and process-buffer (buffer-live-p process-buffer))
            (kill-buffer process-buffer)))))))

;;;###autoload
(defun git-tools-open-all-conflict-files ()
  "Open all files with merge conflicts in the current git repo."
  (interactive)
  (let ((files (magit-unmerged-files)))
    (if (null files)
      (message "No conflicting files found.")
      (dolist (file files)
        (find-file (expand-file-name file (magit-toplevel))))
      (message "Opened %d conflicting file(s)." (length files)))))

(defun git-tools--author-weights (directory)
  "Return an alist of (AUTHOR-STRING . LINE-COUNT) for DIRECTORY.
AUTHOR-STRING is \"Name <email>\". LINE-COUNT sums added+deleted
lines from commits touching files within DIRECTORY (not the whole
repo, if DIRECTORY is a subdirectory of a larger repository)."
  (let* ((default-directory (expand-file-name directory))
          (output (shell-command-to-string
                    "git log --no-merges --format='@@@%aN <%aE>' --numstat -- ."))
          (table (make-hash-table :test 'equal))
          (current-author nil))
    (dolist (line (split-string output "\n"))
      (cond
        ((string-prefix-p "@@@" line)
          (setq current-author (substring line 3)))
        ((string-match "\\`\\([0-9]+\\)\t\\([0-9]+\\)\t" line)
          (when current-author
            (let ((added (string-to-number (match-string 1 line)))
                   (deleted (string-to-number (match-string 2 line))))
              (puthash current-author
                (+ (gethash current-author table 0) added deleted)
                table))))
        ;; Binary files show as "-\t-\tpath"; contribute 0, ignored.
        ))
    ;; Make sure authors with zero countable lines (e.g. only touched
    ;; binary files) still show up.
    (dolist (author (split-string
                      (shell-command-to-string "git log --format='%aN <%aE>' -- .")
                      "\n" t))
      (unless (gethash author table)
        (puthash author 0 table)))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) table)
      result)))

(defun git-tools--sorted-author-weights (directory &optional alphabetical)
  "Return author weights for DIRECTORY, sorted.
By default, sort by line count descending, breaking ties
alphabetically. When ALPHABETICAL is non-nil, sort by author name
instead."
  (let ((alist (git-tools--author-weights directory)))
    (if alphabetical
      (sort alist (lambda (a b) (string-lessp (car a) (car b))))
      (sort alist (lambda (a b)
                    (if (= (cdr a) (cdr b))
                      (string-lessp (car a) (car b))
                      (> (cdr a) (cdr b))))))))

;;;###autoload
(defun git-tools-authors-insert (directory &optional alphabetical)
  "List all unique authors (name and email) for DIRECTORY.
Results are annotated with total lines changed (added+deleted) in
commits touching files within DIRECTORY, and displayed in a
dedicated buffer. Sorted by line count descending by default; with
a prefix argument (ALPHABETICAL), sort alphabetically by name
instead."
  (interactive "DGit repository directory: \nP")
  (let* ((default-directory (expand-file-name directory))
          (entries (git-tools--sorted-author-weights directory alphabetical))
          (buf (get-buffer-create "*Git Authors*")))
    (if (null entries)
      (message "No authors found or not a git repository: %s" directory)
      (with-current-buffer buf
        (read-only-mode -1)
        (erase-buffer)
        (insert (format "Authors in: %s\n" (abbreviate-file-name default-directory)))
        (insert (format "(sorted by %s)\n"
                  (if alphabetical "name" "lines changed, descending")))
        (insert (make-string 40 ?=) "\n")
        (dolist (entry entries)
          (insert (format "%-50s %6d lines\n" (car entry) (cdr entry))))
        (goto-char (point-min))
        (read-only-mode 1))
      (pop-to-buffer buf))))

;;;###autoload
(defun git-tools-authors-list (directory &optional alphabetical)
  "Return a list of (AUTHOR-STRING . LINE-COUNT) for DIRECTORY.
Sorted by line count descending by default; with a prefix argument
(ALPHABETICAL), sort alphabetically by name instead. When called
interactively, also prints a summary in the echo area."
  (interactive "DGit repository directory: \nP")
  (let ((entries (git-tools--sorted-author-weights directory alphabetical)))
    (if (null entries)
      (progn
        (message "No authors found or not a git repository: %s" directory)
        nil)
      (when (called-interactively-p 'interactive)
        (message "Authors in %s (%d found, sorted by %s)"
          (abbreviate-file-name (expand-file-name directory))
          (length entries)
          (if alphabetical "name" "lines changed, descending")))
      entries)))

(defun git-tools--default-directory ()
  "Return the directory of the current buffer's file, or `default-directory'."
  (if buffer-file-name
    (file-name-directory buffer-file-name)
    default-directory))

(defun git-tools--sorted-author-weights (directory &optional alphabetical)
  "Return author weights for DIRECTORY, sorted.
By default, sort by line count descending, breaking ties
alphabetically. When ALPHABETICAL is non-nil, sort by author name
instead."
  (let ((alist (git-tools--author-weights directory)))
    (if alphabetical
      (sort alist (lambda (a b) (string-lessp (car a) (car b))))
      (sort alist (lambda (a b)
                    (if (= (cdr a) (cdr b))
                      (string-lessp (car a) (car b))
                      (> (cdr a) (cdr b))))))))

;;;###autoload
(defun git-tools-authors-insert (directory &optional alphabetical)
  "List all unique authors (name and email) for DIRECTORY.
Results are annotated with total lines changed (added+deleted) in
commits touching files within DIRECTORY, and displayed in a new,
uniquely-named buffer each time this is called. Sorted by line
count descending by default; with a prefix argument (ALPHABETICAL),
sort alphabetically by name instead."
  (interactive
    (list (read-directory-name "Git repository directory: "
            (git-tools--default-directory) nil t)
      current-prefix-arg))
  (let* ((default-directory (expand-file-name directory))
          (entries (git-tools--sorted-author-weights directory alphabetical))
          (buf (generate-new-buffer
                 (format "*Git Authors: %s*"
                   (abbreviate-file-name default-directory)))))
    (if (null entries)
      (progn
        (kill-buffer buf)
        (message "No authors found or not a git repository: %s" directory))
      (with-current-buffer buf
        (insert (format "Authors in: %s\n" (abbreviate-file-name default-directory)))
        (insert (format "(sorted by %s)\n"
                  (if alphabetical "name" "lines changed, descending")))
        (insert (make-string 40 ?=) "\n")
        (dolist (entry entries)
          (insert (format "%-50s %6d lines\n" (car entry) (cdr entry))))
        (goto-char (point-min))
        (read-only-mode 1)
        (pop-to-buffer buf)))))

;;;###autoload
(defun git-tools-commit-amend-no-edit ()
  "Git amend commit automatically without editor."
  (interactive)
  (let ((proc
          (magit-run-git-with-editor
            "commit" "--amend" "--no-edit")))
    (set-process-sentinel
      proc
      (lambda (process _event)
        (when (eq (process-status process) 'exit)
          (magit-refresh-all))))))

;;;###autoload
(defun git-tools-review-directory ()
  "Return the effective git working directory for review."
  (interactive)
  (let ((dir (file-name-as-directory
              (or (and (stringp git-tools-review-home)
                       (not (string-empty-p git-tools-review-home))
                       (expand-file-name git-tools-review-home))
                  (git-tools--project-root)
                  default-directory))))
    (when (called-interactively-p 'interactive)
      (message "Git review directory: %s" dir))
    dir))

(defun git-tools--pr-owner-repo (directory)
  "Return (OWNER . REPO) parsed from origin's remote URL in DIRECTORY, or nil.
Handles both HTTPS URLs (https://github.com/owner/repo.git) and
SSH URLs, including SSH config host aliases
(e.g. git@github-lb:owner/repo.git where `github-lb' is a Host
alias in ~/.ssh/config, not the literal github.com)."
  (let* ((default-directory directory)
          (url (magit-git-string "remote" "get-url" "origin")))
    (when (and url
            (string-match
              "\\`\\(?:[[:alnum:]_.-]+@\\)?[^:/@]+[:/]\\([^/]+\\)/\\([^/.]+\\)\\(?:\\.git\\)?/?\\'"
              url))
      (cons (match-string 1 url) (match-string 2 url)))))

(defun git-tools--pr-head-branch-via-gh (owner repo pr-number)
  "Look up the head branch of PR-NUMBER using the `gh' CLI, or nil."
  (when (executable-find "gh")
    (with-temp-buffer
      (when (zerop (call-process "gh" nil t nil
                     "pr" "view" (format "%s" pr-number)
                     "--repo" (format "%s/%s" owner repo)
                     "--json" "headRefName"
                     "-q" ".headRefName"))
        (let ((name (string-trim (buffer-string))))
          (unless (string-empty-p name) name))))))

(defun git-tools--pr-head-branch-via-api (owner repo pr-number)
  "Look up the head branch of PR-NUMBER via the GitHub REST API, or nil."
  (condition-case nil
    (let (result)
      (with-current-buffer
        (url-retrieve-synchronously
          (format "https://api.github.com/repos/%s/%s/pulls/%s"
            owner repo pr-number)
          t t 10)
        (goto-char (point-min))
        (when (re-search-forward "\n\n" nil t)
          (let* ((json-object-type 'alist)
                  (data (json-read))
                  (head (alist-get 'head data))
                  (ref (alist-get 'ref head)))
            (when (stringp ref) (setq result ref))))
        (kill-buffer))
      result)
    (error nil)))

(defun git-tools--pr-head-branch (directory pr-number)
  "Return PR-NUMBER's actual head branch name for the repo in DIRECTORY.
Tries `gh' first, then the GitHub REST API. Returns nil if both fail."
  (let* ((owner-repo (git-tools--pr-owner-repo directory))
          (owner (car owner-repo))
          (repo (cdr owner-repo)))
    (when (and owner repo)
      (or (git-tools--pr-head-branch-via-gh owner repo pr-number)
        (git-tools--pr-head-branch-via-api owner repo pr-number)))))

;;;###Autoload
(defun git-tools-review-start ()
  "Start reviewing a GitHub pull request in a dedicated repo directory.
Use `git-tools-review-home' as the repo directory if it is set to a
non-empty string; otherwise fall back to `git-tools--project-root'
(based on the current buffer, like the rest of git-tools).
In that repo:
1. Clean the working tree (`git reset --hard' + `git clean -fd',
   discarding local changes and untracked files).
2. Check out the main branch, per `git-tools-main-branch-name'.
3. Pull the latest changes from origin.
4. Determine which pull request to review from the URL currently on
   the system clipboard, if it looks like a GitHub PR URL (e.g.
   https://github.com/OWNER/REPO/pull/123).  If the clipboard is
   empty or doesn't hold such a URL, prompt for one interactively.
   Fetch that PR from origin and check it out into a local branch
   using the PR's actual head branch name (looked up via `gh' or the
   GitHub API), falling back to `review/pr-NUMBER' if that lookup
   fails.
5. Add prompt to the kill ring."
  (interactive)
  (let* ((default-directory
           (file-name-as-directory
             (or (and (stringp git-tools-review-home)
                    (not (string-empty-p git-tools-review-home))
                    (expand-file-name git-tools-review-home))
               (git-tools--project-root)
               (user-error "Could not determine a git repository directory"))))
          (main-branch (or (git-tools-main-branch-name)
                         (user-error "Could not determine main branch for %s"
                           default-directory)))
          (pr-number (git-tools--pr-number-from-clipboard-or-prompt)))
    (unless (file-directory-p default-directory)
      (user-error "Directory does not exist: %s" default-directory))
    (message "git-tools-review: preparing %s" default-directory)
    ;; 1. Clean the working tree.
    (magit-run-git "reset" "--hard" "HEAD")
    (magit-run-git "clean" "-fd")
    ;; 2. Switch to the main/master branch.
    (unless (string= (magit-get-current-branch) main-branch)
      (magit-run-git "checkout" main-branch))
    ;; 3. Pull the latest from origin.
    (if (magit-get-upstream-branch)
      (magit-run-git "pull" "origin" main-branch)
      (magit-run-git "fetch" "origin" (format "%s:%s" main-branch main-branch)))
    ;; 4. Fetch and check out the PR, using its real branch name if we can find it.
    (let* ((review-branch
             (or (git-tools--pr-head-branch default-directory pr-number)
               (format "review/pr-%s" pr-number)))
            (output (concat (format "In the directory %s, " default-directory)
                            (format "please review the latest commits in the current branch %s " review-branch)
                            (format "to be merged into %s " (git-tools-main-branch-name default-directory))
                            (format "and start with a simple Approve 'Yes' or 'No' and ")
                            (format "if not approved, provide concise list of critical problems. ")
                            (format "Also provide a short list of a few comments for improvement if applicable."))))
      (magit-run-git "fetch" "origin"
        (format "pull/%s/head:%s" pr-number review-branch))
      (magit-run-git "checkout" review-branch)
      (kill-new output)
      (message "git-tools-review: checked out PR #%s as `%s' in %s"
        pr-number review-branch default-directory))))

(defun git-tools--clipboard-string ()
  "Return the current system clipboard contents as a string, or nil."
  (or (ignore-errors (gui-get-selection 'CLIPBOARD 'STRING))
    (ignore-errors (current-kill 0 t))))

(defun git-tools--github-pr-number (url)
  "Return the PR number as a string if URL looks like a GitHub PR URL.
Return nil otherwise (including when URL is nil or empty)."
  (when (and (stringp url)
          (not (string-empty-p (string-trim url)))
          (string-match
            "\\`https?://github\\.com/[^/]+/[^/]+/pull/\\([0-9]+\\)/?\\'"
            (string-trim url)))
    (match-string 1 (string-trim url))))

(defun git-tools--pr-number-from-clipboard-or-prompt ()
  "Return a GitHub PR number, taken from the clipboard if possible.
If the clipboard holds a string that looks like a GitHub pull
request URL, extract and return its PR number.  Otherwise (empty
clipboard, or a URL that doesn't match), prompt interactively,
re-prompting until a valid GitHub PR URL is entered."
  (or (git-tools--github-pr-number (git-tools--clipboard-string))
    (let (number)
      (while (not number)
        (setq number
          (git-tools--github-pr-number
            (read-string "GitHub pull request URL: ")))
        (unless number
          (message "That doesn't look like a GitHub pull request URL; try again.")
          (sit-for 1)))
      number)))

(defun git-tools--default-copy-dir ()
  "Compute the default destination directory: ~/wip/RELATIVE-PATH/BRANCH.
RELATIVE-PATH is the git repo's root expressed relative to the user's
home directory (or to `/' if the repo is outside the home directory),
preserving any intermediate directories — see
`git-tools--project-relative-path'.  BRANCH is the current branch."
  (let* ((root (or (git-tools--project-root)
                 (error "Not inside a git repository")))
          (default-directory root)
          (relative-path (git-tools--project-relative-path root))
          (branch (magit-get-current-branch))
          (subdir (if (string-empty-p relative-path)
                    branch
                    (concat relative-path "/" branch))))
    (expand-file-name subdir "~/wip")))

(defun git-tools--nonempty-lines (str)
  "Split STR into lines, trimmed, dropping empty lines."
  (delq nil
    (mapcar (lambda (line)
              (setq line (string-trim line))
              (unless (string-empty-p line) line))
      (split-string str "\n"))))

(defun git-tools--commit-list (commit-count)
  "Return the last COMMIT-COUNT commit hashes on HEAD, oldest first."
  (git-tools--nonempty-lines
    (shell-command-to-string
      (format "git log --reverse --format=%%H -%d" commit-count))))

(defun git-tools--commit-files (commit)
  "Return list of files (relative paths) touched by COMMIT."
  (git-tools--nonempty-lines
    (shell-command-to-string
      (format "git diff-tree --no-commit-id --name-only -r --root %s"
        (shell-quote-argument commit)))))

(defun git-tools--copy-object (spec dest)
  "Write the content of the git object at SPEC to DEST.
SPEC is anything `git show' accepts for a single blob, e.g.
\"HEAD:path/to/file\" for a commit, or \":path/to/file\" for the
current index (staged content).  Creates parent directories of DEST
as needed.  Returns t on success, or nil if SPEC does not resolve to
a blob (e.g. the file was deleted, or is not staged)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let* ((coding-system-for-read 'no-conversion)
            (coding-system-for-write 'no-conversion)
            (exit-code (call-process "git" nil t nil "show" spec)))
      (when (zerop exit-code)
        (make-directory (file-name-directory dest) t)
        (write-region (point-min) (point-max) dest nil 'quiet nil nil)
        t))))

(defun git-tools--copy-blob (commit file dest)
  "Write the content of FILE as of COMMIT to DEST.
See `git-tools--copy-object' for return value and directory handling."
  (git-tools--copy-object (format "%s:%s" commit file) dest))

(defun git-tools--staged-files ()
  "Return list of files (relative paths) with staged changed files."
  (git-tools--nonempty-lines
    (shell-command-to-string "git diff --cached --name-only")))

(defun git-tools--working-changed-files ()
  "Return list of files (relative paths) that are modified or untracked.
\"Modified\" means the working tree differs from the index — i.e.
unstaged edits to an already-tracked file, whether or not it also has
separate staged changes.  Untracked files are included too.  Files
that are only unstaged-deleted are included as well; callers should
expect `file-exists-p' to fail for those and skip them."
  (let* ((output (shell-command-to-string
                   "git status --porcelain --untracked-files=all"))
          (lines (split-string output "\n" t)))
    (delete-dups
      (delq nil
        (mapcar
          (lambda (line)
            (when (>= (length line) 4)
              (let* ((xy (substring line 0 2))
                      (worktree-status (aref line 1))
                      (path (string-trim (substring line 3))))
                (when (string-match " -> " path)
                  (setq path (car (last (split-string path " -> ")))))
                (when (or (string= xy "??")
                        (memq worktree-status '(?M ?A ?D)))
                  path))))
          lines)))))

;;;###autoload
(defun git-tools-copy-commits (&optional dir commit-count)
  "Copy files from the latest commit or COMMIT-COUNT commits to DIR.

Commits are walked oldest to newest, and each touched file is written
as it existed in that commit, overwriting anything already copied to
DIR from an earlier commit in the range.  A file deleted by a commit
in the range is simply skipped (existing copies of it in DIR are left
alone).

If DIR is omitted, use ~/wip concatenated with the project name and
current branch.  For example, if the git repo is in ~/Projects/myproj
and the current branch is `main', the default destination is
~/wip/myproj/main.

If COMMIT-COUNT is omitted, copy files from the single latest commit.

Interactively:
- No prefix argument: use both defaults (latest commit, default dir).
- Numeric prefix argument N (e.g. `C-u 3 \\[git-tools-copy-commits]'):
  copy files touched by the latest N commits, to the default directory.
- Plain `\\[universal-argument]' (non-numeric prefix): prompt for the
  destination directory, copying only the latest commit.

To specify both a custom directory and a custom commit count, call
this function from Lisp, e.g.:
  (git-tools-copy-commits \"/tmp/out\" 3)"
  (interactive
    (list
      (when (consp current-prefix-arg)
        (read-directory-name "Copy commits to directory: "))
      (unless (consp current-prefix-arg)
        (prefix-numeric-value current-prefix-arg))))
  (let* ((commit-count (max 1 (or commit-count 1)))
          (root (or (git-tools--project-root)
                  (error "Not inside a git repository")))
          (default-directory root)
          (dir (or dir (git-tools--default-copy-dir)))
          (commits (git-tools--commit-list commit-count))
          (copied 0)
          (skipped 0))
    (unless commits
      (user-error "No commits found"))
    (dolist (commit commits)
      (dolist (file (git-tools--commit-files commit))
        (if (git-tools--copy-blob commit file (expand-file-name file dir))
          (setq copied (1+ copied))
          (setq skipped (1+ skipped)))))
    (message "Copied %d file(s) from %d commit(s) to %s%s"
      copied (length commits) dir
      (if (> skipped 0) (format " (%d skipped, likely deleted)" skipped) ""))))

;;;###autoload
(defun git-tools-copy-staged (&optional dir)
  "Copy currently staged files to DIR, using their staged (index) content.

Each file is written as it exists in the index right now — i.e. what
`git commit' would record — not the working-tree version, so any
additional unstaged edits on top of the staged version are not
included.  A staged deletion is skipped (there is no content to copy).

If DIR is omitted, use ~/wip concatenated with the project name and
current branch, same as `git-tools-copy-commits'.

Interactively, a prefix argument (e.g. `\\[universal-argument]
\\[git-tools-copy-staged]') prompts for the destination directory;
with no prefix argument, the default directory is used."
  (interactive
    (list
      (when current-prefix-arg
        (read-directory-name "Copy staged files to directory: "))))
  (let* ((root (or (git-tools--project-root)
                 (error "Not inside a git repository")))
          (default-directory root)
          (dir (or dir (git-tools--default-copy-dir)))
          (files (git-tools--staged-files))
          (copied 0)
          (skipped 0))
    (unless files
      (user-error "No staged files found"))
    (dolist (file files)
      (if (git-tools--copy-object (concat ":" file) (expand-file-name file dir))
        (setq copied (1+ copied))
        (setq skipped (1+ skipped))))
    (message "Copied %d staged file(s) to %s%s"
      copied dir
      (if (> skipped 0) (format " (%d skipped, likely deleted)" skipped) ""))))

;;;###autoload
(defun git-tools-copy-working-changes (&optional dir)
  "Copy modified and untracked working-tree files to DIR.

Copies the current on-disk content of any file that has unstaged
modifications relative to the index, or that is untracked by git, per
`git status'.  This includes files that also have separate staged
changes, since the point is what's currently on disk.  A file that is
unstaged-deleted is skipped, since it no longer exists on disk.

If DIR is omitted, use ~/wip concatenated with the project name and
current branch, same as `git-tools-copy-commits'.

Interactively, a prefix argument (e.g. `\\[universal-argument]
\\[git-tools-copy-working-changes]') prompts for the destination
directory; with no prefix argument, the default directory is used."
  (interactive
    (list
      (when current-prefix-arg
        (read-directory-name "Copy working changes to directory: "))))
  (let* ((root (or (git-tools--project-root)
                 (error "Not inside a git repository")))
          (default-directory root)
          (dir (or dir (git-tools--default-copy-dir)))
          (files (git-tools--working-changed-files))
          (copied 0)
          (skipped 0))
    (unless files
      (user-error "No modified or untracked files found"))
    (dolist (file files)
      (let ((src (expand-file-name file root))
             (dest (expand-file-name file dir)))
        (if (file-exists-p src)
          (progn
            (make-directory (file-name-directory dest) t)
            (copy-file src dest t)
            (setq copied (1+ copied)))
          (setq skipped (1+ skipped)))))
    (message "Copied %d modified/untracked file(s) to %s%s"
      copied dir
      (if (> skipped 0) (format " (%d skipped, likely deleted)" skipped) ""))))

(provide 'git-tools)
;;; git-tools.el ends here
