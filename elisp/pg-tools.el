;;; pg-tools.el --- some PostgreSQL tools -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Todd Ornett
;;
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: June 11, 2025
;; Modified: June 11, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/todd.ornett/git-tools
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides some PostgreSQL tools.
;;
;;; Code:

(defun pg-tools-db-start ()
  "Start the PostgreSQL database.

Start the DB asynchronously using pg_ctl and Homebrew's data directory."
  (interactive)
  (let* ((brew-prefix (shell-command-to-string "brew --prefix"))
         (postgres-version (shell-command-to-string "brew list | grep postgres"))
         (data-dir (concat (string-trim brew-prefix) "/var/" (string-trim postgres-version)))
         (pg-ctl-command (concat "pg_ctl -D " data-dir " start"))
         (output-buffer (get-buffer-create "*PostgreSQL*")))
    (if (and brew-prefix postgres-version)
        (progn
          (message "Starting PostgreSQL with data directory: %s" data-dir)
          (with-current-buffer output-buffer
            (erase-buffer)
            (insert (format "Starting PostgreSQL at %s\n" (current-time-string))))
          (start-process "pg-tools-db-start" output-buffer "sh" "-c" pg-ctl-command)
          (display-buffer output-buffer)
          (message "PostgreSQL start command issued asynchronously. Check *PostgreSQL* buffer for output."))
      (error "Failed to locate PostgreSQL or Homebrew installation"))))

(defun pg-tools-db-terminate ()
  "Stop the PostgreSQL database.

Stop the DB asynchronously using pg_ctl and Homebrew's data directory."
  (interactive)
  (let* ((brew-prefix (shell-command-to-string "brew --prefix"))
         (postgres-version (shell-command-to-string "brew list | grep postgres"))
         (data-dir (concat (string-trim brew-prefix) "/var/" (string-trim postgres-version)))
         (pg-ctl-command (concat "pg_ctl -D " data-dir " stop"))
         (output-buffer (get-buffer-create "*PostgreSQL*")))
    (if (and brew-prefix postgres-version)
        (progn
          (message "Stopping PostgreSQL with data directory: %s" data-dir)
          (with-current-buffer output-buffer
            (erase-buffer)
            (insert (format "Stopping PostgreSQL at %s\n" (current-time-string))))
          (start-process "pg-tools-db-terminate" output-buffer "sh" "-c" pg-ctl-command)
          (display-buffer output-buffer)
          (message "PostgreSQL stop command issued asynchronously. Check *PostgreSQL* buffer for output."))
      (error "Failed to locate PostgreSQL or Homebrew installation"))))

(provide 'pg-tools)
;;; pg-tools.el ends here
