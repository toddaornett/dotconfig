;;; git-tools.el --- some git tools and workflows -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Todd Ornett
;;
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: April 02, 2025
;; Modified: April 02, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/todd.ornett/git-tools
;; Package-Requires: ((emacs "29.1") (magit "4.0.0") (projectile "2.8.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides some git tools and workflows.
;;
;;; Code:

(require 'magit)
(require 'projectile)

(defun git-tools-main-branch-name ()
  "Determine the effective main branch for the current repository.

The effective main branch is determined as main, develop, trunk, or master.

return nil if not currently in directory managed with git."
  (let ((git-dir (magit-git-dir))) ;; Check if we're in a Git repo
    (when git-dir
      (cond
       ((magit-branch-p "main") "main")    ;; Check if 'main' exists
       ((magit-branch-p "develop") "develop") ;; Check if 'develop' exists
       ((magit-branch-p "trunk") "trunk")  ;; Check if 'trunk' exists
       (t "master")))))                    ;; Default to 'master'

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

(defun git-tools-pull-all-main (root-dir)
  "Iterate through all subdirectories in ROOT-DIR, switch to main branch, and pull.

When iterating first switch to the effective main branch and then pull."
  (interactive "DDirectory: ~/Projects ")
  (let ((default-directory root-dir)
        (error-count 0))
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
                      (if (magit-anything-unstaged-p)
                          (message "Skipping pull in %s: unstaged changes detected" dir)
                        (let ((process-buffer (magit-process-buffer)))
                          (let ((upstream (magit-get-upstream-branch)))
                            (if upstream
                                (unless (string-equal-ignore-case (magit-get-current-branch) main-branch)
                                  (magit-run-git "checkout" main-branch))
                              (error "No upstream branch configured")))
                          (message "Successfully pulled %s in %s" main-branch dir)
                          (when (and process-buffer (buffer-live-p process-buffer))
                            (kill-buffer process-buffer)))))
                  (error
                   (message "Error in %s: %s" dir (error-message-string err))
                   (setq error-count (1+ error-count))))
              (message "No valid main branch found in %s" dir))))))
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
  (let* ((project-name (projectile-project-name))
         (project-root (projectile-project-root))
         (project-files (projectile-project-files project-root))
         (updates-made nil)
         (git-main-branch (git-tools-main-branch-name))
         (target-branch (concat "build/update-env-" (string-replace "_" "-" (downcase  variable)))))
    (unless project-root
      (error "No project root found. Ensure a project is active"))
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
    (dolist (relative-file project-files)
      (let ((file (expand-file-name relative-file project-root)))
        (when (and (file-exists-p file)
                   (string-equal-ignore-case
                    (or (file-name-extension file) "")
                    "yaml"))
          (when (git-tools-update-yaml-file (or buffer-file-name "") file variable value)
            (setq updates-made t)))))

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

(provide 'git-tools)
;;; git-tools.el ends here
