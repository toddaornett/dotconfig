;;; gha.el --- Tools for maintaining GitHub Actions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Todd Ornett
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: June 10, 2024
;; Modified: June 13, 2025
;; Version: 0.0.2
;; Keywords: convenience data extensions files internal languages
;; Homepage: https://github.com/todd.ornett/dotconfig
;; Package-Requires: ((emacs "24.4") (magit "3.3.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides tools to manage GitHub Actions YAML files.
;; It adds missing permissions blocks after the 'on' or 'concurrency'
;; sections in workflow files and includes a wrapper to process Git
;; repositories, handling branch switching and commits via Magit.
;;
;;; Code:

(require 'magit)
(require 'git-tools)

(defun gha-add-granular-permissions (gha-dir)
  "Add permissions to YAML files in GHA-DIR if missing.
Iterate through all YAML files in the specified directory and insert
appropriate permissions after the last `on' or `concurrency' section.
Skip files that already contain a permissions block."
  (interactive (list (expand-file-name (read-string "GHA directory: " ".github/workflows"))))
  (let ((gha-dir (expand-file-name (or gha-dir "~/Projects"))))
    (let ((default-directory gha-dir))
      (dolist (file (directory-files gha-dir t "\\.yml\\'"))
        (when (and (file-regular-p file)
                   (not (file-symlink-p file)))
          (let ((filename (file-name-nondirectory file)))
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (unless (re-search-forward "^permissions:" nil t)
                (goto-char (point-max))
                (when (re-search-backward "^on:\\|^concurrency:" nil t)
                  (forward-line 1)
                  (while (and (not (eobp))
                              (or (looking-at-p "^\\s-+[^\n]*$")
                                  (looking-at-p "^\\s-*$")))
                    (forward-line 1))
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
                  (insert "\n\n")
                  (write-region (point-min) (point-max) file nil 'no-message))))))))))

(defun gha-process-git-repos (root-dir)
  "Process Git repositories under ROOT-DIR to add GHA permissions.
For each Git repository under ROOT-DIR (default ~/Projects), switch
to the main branch using `git-tools-main-branch-name', run
`gha-add-granular-permissions' on the .github/workflows directory,
and if changes are detected, create a new branch named
fix/workflow-permissions, add modified files, and add commit"
  (interactive (list (expand-file-name (read-string "Root directory: " "~/Projects"))))
  (let ((root-dir (expand-file-name (or root-dir "~/Projects"))))
    (dolist (dir (directory-files root-dir t "^[^.]"))
      (when (and (file-directory-p dir)
                 (file-directory-p (expand-file-name ".git" dir)))
        (let* ((default-directory dir)
               (gha-dir (expand-file-name ".github/workflows" dir)))
          (message "Processing %s for possible GHA workflows update" dir)
          (when (file-directory-p gha-dir)
            (let ((main-branch (git-tools-main-branch-name dir)))
              (magit-call-git "checkout" main-branch)
              (unless (magit-git-lines "branch" "--list" "fix/workflow-permissions")
                (magit-call-git "pull")
                (gha-add-granular-permissions gha-dir)
                (let ((after-files (magit-git-lines "ls-files" "-m")))
                  (when after-files
                    (message "Updated GHA workflows in %s" dir)
                    (write-region (format "- [ ] %s\n" (file-name-nondirectory dir))
                                  nil
                                  "~/updated_workflows.txt"
                                  'append)
                    (magit-call-git "checkout" "-b" "fix/workflow-permissions")
                    (dolist (file after-files)
                      (magit-call-git "add" file))
                    (let ((commit-message (or (getenv "DEFAULT_GIT_COMMIT_MESSAGE")
                                              "fix(ci): add permissions to GHA workflows")))
                      (magit-call-git "commit" "-m" (replace-regexp-in-string "\\\\n" "\n" commit-message))))))))))))
  (message (concat "Completed GHA workflow updates in projects under " (directory-file-name root-dir) " folder.")))

(defun gha-update-publish-workflow (gha-dir &optional new-version)
  "Update semantic release version in GHA-DIR publish workflow.
If GHA-DIR is not provided, prompt interactively for the project directory.
The default for NEW-VERSION is the major version found for replacement."
  (interactive
   (list (read-directory-name "Project directory: " "~/Projects/" nil t)
         (read-string "New version (leave empty to use major version): " nil nil nil)))
  (let* ((workflow-file (expand-file-name ".github/workflows/publish.yml" gha-dir))
         (version-regex "@codedependant/semantic-release-docker@\\([0-9]+\\)\\(\\.[0-9]+\\)?\\(\\.[0-9]+\\)?")
         (replacement (if (and new-version (not (string-empty-p new-version)))
                          (concat "@codedependant/semantic-release-docker@" new-version)
                        "@codedependant/semantic-release-docker@\\1")))
    (when (file-exists-p workflow-file)
      (with-current-buffer (find-file-noselect workflow-file)
        (goto-char (point-min))
        (when (re-search-forward version-regex nil t)
          (replace-match replacement nil nil)
          (save-buffer)
          (message "Updated %s successfully" workflow-file))
        (kill-buffer)))))

(defun gha-update-all-publish-workflows (root-dir)
  "Process Git repositories under ROOT-DIR to add GHA permissions.
For each Git repository under ROOT-DIR (default ~/Projects), switch
to the main branch using `git-tools-main-branch-name', run
`gha-update-publish-workflow', and if changes are detected,
create a new branch named fix/publish-workflow,
add modified files, and add commit."
  (interactive (list (expand-file-name (read-string "Root directory: " "~/Projects"))))
  (let ((root-dir (expand-file-name (or root-dir "~/Projects"))))
    (delete-file "~/updated_publish_workflows.txt")
    (dolist (dir (directory-files root-dir t "^[^.]"))
      (when (and (file-directory-p dir)
                 (file-directory-p (expand-file-name ".git" dir)))
        (message "Processing %s for possible GHA publish workflow update" dir)
        (let ((main-branch (git-tools-main-branch-name dir))
              (default-directory dir))
          (magit-call-git "checkout" main-branch)
          (unless (magit-git-lines "branch" "--list" "fix/publish-workflow")
            (magit-call-git "pull")
            (gha-update-publish-workflow dir "5")
            (let ((after-files (magit-git-lines "ls-files" "-m")))
              (when after-files
                (message "Updated GHA publish workflow in %s" dir)
                (write-region (format "- [ ] %s\n" (file-name-nondirectory dir))
                              nil
                              "~/updated_publish_workflows.txt"
                              'append)
                (magit-call-git "checkout" "-b" "fix/publish-workflow")
                (dolist (file after-files)
                  (magit-call-git "add" file))
                (let ((commit-message (or (getenv "DEFAULT_GIT_COMMIT_MESSAGE")
                                          "fix(ci): update version in semantic-release-docker for publish workflow")))
                  (magit-call-git "commit" "-m" (replace-regexp-in-string "\\\\n" "\n" commit-message))))))))))
  (message (concat "Completed GHA publish workflow updates in projects under " (directory-file-name root-dir) " folder.")))

(provide 'gha)
;;; gha.el ends here
