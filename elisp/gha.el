;;; gha.el --- some tools for maintaining github actions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Todd Ornett
;;
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: June 10, 2025
;; Modified: June 10, 2025
;; Version: 0.0.1
;; Keywords: convenience data extensions files internal languages lisp local maint mail matching tools
;; Homepage: https://github.com/todd.ornett/dotconfig
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Add permissions block to gha yaml files if it is missing.
;;
;;; Code:

(defun gha-add-granular-permissions (gha-dir)
  "Iterate through all YAML files in GHA-DIR and add appropriate permissions."
  (interactive "DDirectory: .github/workflows")
  (let ((default-directory gha-dir))
    (dolist (file (directory-files gha-dir t "\\.yml\\'"))
      (when (and (file-regular-p file)
                 (not (file-symlink-p file)))
        (let ((filename (file-name-nondirectory file)))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (unless (re-search-forward "^permissions:\\s-*$" nil t)
              (goto-char (point-max))
              (when (re-search-backward "^on:\\|^concurrency:" nil t)
                (forward-line 1)
                (while (and (not (eobp))
                            (or (looking-at-p "^\\s-+[^\n]*$")  ; Indented line
                                (looking-at-p "^\\s-*$")))      ; Blank line
                  (forward-line 1))
                (unless (looking-at-p "^permissions:\\s-*$")
                  (cond
                   ((string-match-p "publish.yml" filename)
                    (insert "permissions:\n  contents: write\n  pull-requests: write\n  issues: write"))
                   ((string-match-p "trigger" filename)
                    (insert "permissions: {}"))
                   ((or (string-match-p "pull" filename)
                        (string-match-p "pr" filename))
                    (insert "permissions:\n  pull-requests: read"))
                   (t
                    (insert "permissions:\n  contents: read")))
                  (insert "\n\n"))
                (write-region (point-min) (point-max) file nil 'no-message)))))))))

(defun gha-process-git-repos (root-dir)
  "Process Git repositories under ROOT-DIR to add GHA permissions.
For each Git repository under ROOT-DIR (default ~/Projects), switch
to the main branch using `git-tools-main-branch-name', run
`gha-add-granular-permissions' on the .github/workflows directory,
and if changes are detected, create a new branch named
fix/workflowPermissions, add modified files, and commit."
  (interactive "DRoot directory: ~/Projects")
  (let ((root-dir (expand-file-name (or root-dir "~/Projects"))))
    (dolist (dir (directory-files root-dir t "^[^.]"))
      (when (and (file-directory-p dir)
                 (file-directory-p (expand-file-name ".git" dir)))
        (let ((default-directory dir)
              (gha-dir (expand-file-name ".github/workflows" dir)))
          (when (file-directory-p gha-dir)
            (let ((main-branch (git-tools-main-branch-name dir)))
              (magit-call-git "checkout" main-branch)
              (gha-add-granular-permissions gha-dir)
              (let ((after-files (magit-git-lines "ls-files" "-m")))
                (when after-files
                  (message "Updated workflows in %s" dir)
                  (magit-call-git "checkout" "-b" "fix/workflowPermissions")
                  (dolist (file after-files)
                    (magit-call-git "add" file))
                  (magit-call-git "commit" "-m" "fix(ci): add permissions for workflows"))))))))))

(provide 'gha)
;;; gha.el ends here
