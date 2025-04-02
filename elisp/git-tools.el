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
;;  This package provides some git tools and workflows.
;;
;;; Code:

(defun git-tools-main-branch-name ()
  "Determine the effective main branch for the current repository.

  The effective main branch is determined as main, trunk, or master."
  (let ((git-dir (magit-git-dir))) ;; Check if we're in a Git repo
    (when git-dir
      (cond
       ((magit-branch-p "main") "main")    ;; Check if 'main' exists
       ((magit-branch-p "trunk") "trunk")  ;; Check if 'trunk' exists
       (t "master")))))                    ;; Default to 'master'

(defun git-tools-pull-all-main (root-dir)
  "Iterate through all subdirectories in ROOT-DIR, switch to main branch, and pull.

  When iterating first switch to the effective main branch and then pull."
  (interactive "DDirectory: ~/Projects ") ;; Prompt for directory, default to ~/Projects
  (let ((default-directory root-dir))
    (dolist (dir (directory-files root-dir t "\\`[^.]"))
      (when (and (file-directory-p dir)
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
