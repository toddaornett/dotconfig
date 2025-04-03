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
;; Package-Requires: ((emacs "24.3") (magit))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides some git tools and workflows.
;;
;;; Code:

(require 'magit)

(defun git-tools-main-branch-name ()
  "Determine the effective main branch for the current repository.

  The effective main branch is determined as main, trunk, or master."
  (let ((git-dir (magit-git-dir))) ;; Check if we're in a Git repo
    (when git-dir
      (cond
       ((magit-branch-p "main") "main")    ;; Check if 'main' exists
       ((magit-branch-p "trunk") "trunk")  ;; Check if 'trunk' exists
       (t "master")))))                    ;; Default to 'master'

(defun git-tools-show-untracked (&optional parent-dir)
  "List subdirectories under PARENT-DIR with untracked files.

List subdirectories under PARENT-DIR (default '~/Projects')
with files untracked by git. Displays results in a new buffer."
  (interactive)
  (let* ((parent-dir (or parent-dir
                         (expand-file-name
                          (read-string "Enter parent directory path (default '~/Projects'): " nil nil "~/Projects"))))
         (default-directory (expand-file-name parent-dir))
         (dirs (directory-files default-directory nil "^[^.]" t)) ; Exclude . and ..
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
                         (format "cd %s && git status --porcelain --untracked-files=all" full-path))))
            (when (string-match-p "^\\?\\?" status)
              (push dir changed-dirs))))))
    ;; Display results
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Directories with untracked files in %s:\n\n" parent-dir))
      (if changed-dirs
          (insert (mapconcat #'identity (nreverse changed-dirs) "\n"))
        (insert "No directories with untracked files found."))
      (goto-char (point-min))
      (display-buffer buffer))))

(defun git-tools-discard-unstaged-changes (&optional parent-dir force)
  "Discard all unstaged work in Git repos under a parent directory.

If PARENT-DIR is nil, defaults to '~/Projects'. If FORCE is non-nil,
the function skips the confirmation prompt; otherwise, it asks for
confirmation for each repository using 'y-or-n-p'. The results are
displayed in the '*Git Discarded Unstaged Changes*' buffer."
  (interactive "P") ; 'P' allows prefix arg (e.g., C-u) to set force
  (let* ((parent-dir (or parent-dir "~/Projects"))
         (default-directory (expand-file-name parent-dir))
         (dirs (directory-files default-directory nil "^[^.]" t)) ; Exclude . and ..
         (buffer (get-buffer-create "*Git Discarded Unstaged Changes*"))
         (discarded-dirs nil))
    ;; Ensure parent directory exists
    (unless (file-directory-p default-directory)
      (error "Parent directory '%s' does not exist" default-directory))
    ;; Check each subdirectory and discard untracked files
    (dolist (dir dirs)
      (let ((full-path (expand-file-name dir default-directory)))
        (when (and (file-directory-p full-path)
                   (not (file-symlink-p full-path))
                   (file-exists-p (expand-file-name ".git" full-path)))
          (let ((status (shell-command-to-string
                         (format "cd %s && git status --porcelain" full-path))))
              (when (or force (y-or-n-p (format "Discard files with unstaged changes in %s? " full-path)))
                (shell-command (format "cd %s && git clean -f -d" full-path))
                (push dir discarded-dirs))))))
    ;; Display results
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Directories with discarded untracked files in %s:\n\n" parent-dir))
      (if discarded-dirs
          (insert (mapconcat #'identity (nreverse discarded-dirs) "\n"))
        (insert "No directories with untracked files found."))
      (goto-char (point-min))
      (display-buffer buffer))))

(defun git-tools-show-unstaged (&optional parent-dir)
  "List subdirectories under PARENT-DIR with work.

  List subdirectories under PARENT-DIR (default '~/Projects')
  with unstaged Git changes. Displays results in a new buffer."
  (interactive)
  (let* ((parent-dir (or parent-dir "~/Projects"))
         (default-directory (expand-file-name parent-dir))
         (dirs (directory-files default-directory nil "^[^.]" t)) ; Exclude . and ..
         (buffer (get-buffer-create "*Git Unstaged Changes*"))
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
      (insert (format "Directories with uncommitted changes in %s:\n\n" parent-dir))
      (if changed-dirs
          (insert (mapconcat #'identity (nreverse changed-dirs) "\n"))
        (insert "No directories with uncommitted changes found."))
      (goto-char (point-min))
      (display-buffer buffer))))

(defun git-tools-pull-all-main (root-dir)
  "Iterate through all subdirectories in ROOT-DIR, switch to main branch, and pull.

  When iterating first switch to the effective main branch and then pull."
  (interactive "DDirectory: ~/Projects ")
  (let ((default-directory root-dir))
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
                          (magit-pull-from-upstream nil)
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

(provide 'git-tools)
;;; git-tools.el ends here
