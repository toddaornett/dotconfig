;;; flyway-version.el --- handle project flyway version migrations -*- lexical-binding: t; -*-
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Created: April 03, 2025
;; Modified: June 19, 2025
;; Version: 0.0.3
;; Package-Requires: ((emacs "24.3") (magit "3.3.0"))
;;
;;; Commentary:
;;
;;; Description
;;
;;; Code:

(require 'magit)
(require 'git-tools)

(defun flyway-version-upgrade-all-projects (new-version)
  "Update Flyway version to NEW-VERSION for all projects.
If NEW-VERSION is not provided, defaults to '11.5-alpine'."
  (interactive
   (list
    (let ((input (read-string "Enter new Flyway version (default 11.5-alpine): ")))
      (if (string-empty-p input)
          "11.5-alpine"
        input))))
  (let ((projects-dir (expand-file-name "~/Projects")))
    (unless (file-directory-p projects-dir)
      (error "Projects directory not found at %s" projects-dir))
    (dolist (dir (directory-files projects-dir t))
      (when (file-directory-p dir)
        (let* ((dir-name (file-name-nondirectory dir))
               (main-branch (git-tools-main-branch-name dir))
               (v-suffix (concat "v" (car (split-string new-version "\\."))))
               (migrate-branch-name (concat "build/migrate-to-flyway-" v-suffix)))
          (when (and (file-directory-p (expand-file-name ".git" dir))
                     (not (member dir-name '("." "..")))
                     (not (file-symlink-p dir)))
            (let ((default-directory dir))
              (message (concat "Processing " dir " for possible flyway update"))
              (unless (magit-git-lines "branch" "--list" migrate-branch-name)
                (magit-call-git "checkout" main-branch)
                (magit-call-git "pull")
                (let ((dockerfile (concat dir "/db-migration.Dockerfile")))
                  (when (file-exists-p dockerfile)
                    (flyway-version-update-file dockerfile new-version)))
                (let ((rust-test-file (concat dir "/.github/workflows/rust-test.yml")))
                  (when (file-exists-p rust-test-file)
                    (flyway-version-update-file-with-simple-version rust-test-file new-version)))
                (let ((flywayfile (concat dir "/.github/workflows/flyway.yml")))
                  (when (file-exists-p flywayfile)
                    (flyway-version-update-file flywayfile new-version)))
                (let ((flyway-build-file (concat dir "/.github/workflows/flyway-build.yml")))
                  (when (file-exists-p flyway-build-file)
                    (flyway-version-update-file-with-simple-version flyway-build-file new-version)))
                (let ((after-files (magit-git-lines "ls-files" "-m")))
                  (when after-files
                    (message (concat "Updated " dir " to flyway " v-suffix))
                    (let ((inhibit-message t))
                      (write-region (format "- [ ] %s\n" (file-name-nondirectory dir))
                                    nil
                                    "~/updated_flyway_migrations.txt"
                                    'append))
                    (magit-call-git "checkout" "-b" migrate-branch-name)
                    (dolist (file after-files)
                      (magit-call-git "add" file))
                    (magit-call-git "commit" "-m"
                                    (or (getenv "DEFAULT_GIT_COMMIT_MESSAGE")
                                        (concat "build: update " v-suffix " flyway")))))))))))))

(defun flyway-version-update-file (filepath new-version)
  "Update all occurrences of the Flyway version in FILEPATH to NEW-VERSION."
  (with-temp-buffer
    (insert-file-contents filepath)
    (goto-char (point-min))
    (while (re-search-forward "flyway:\\([0-9]+\\(\\.[0-9]+\\)?\\(-alpine\\)?\\)" nil t)
      (replace-match (concat "flyway:" new-version) nil nil))
    (let ((inhibit-message t))
      (write-region nil nil filepath))))

(defun flyway-version-update-file-with-simple-version (filepath new-version)
  "Update all occurrences of the version in FILEPATH to NEW-VERSION.
This will match against a pattern like FLYWAY_VERSION: <version>
Replaces any non-numeric suffix (e.g., -alpine) with 0."
  (with-temp-buffer
    (insert-file-contents filepath)
    (goto-char (point-min))
    (let ((processed-version (if (string-match "\\([0-9]+\\(\\.[0-9]+\\)*\\)\\([-a-zA-Z0-9]+\\)$" new-version)
                                 (concat (match-string 1 new-version) ".0")
                               new-version)))
      (while (re-search-forward "FLYWAY_VERSION:\\s-+\\([0-9]+\\(\\.[0-9]+\\)*\\([-a-zA-Z0-9]+\\)?\\)" nil t)
        (replace-match processed-version nil nil nil 1)))
    (let ((inhibit-message t))
      (write-region nil nil filepath))))

(provide 'flyway-version)
;;; flyway-version.el ends here
