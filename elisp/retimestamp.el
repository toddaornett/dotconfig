;;; retimestamp.el --- Refresh timestamped filenames and contents -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Your Name
;;
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Created: July 07, 2026
;; Modified: July 07, 2026
;; Version: 0.1.0
;; Keywords: convenience files tools
;; Package-Requires: ((emacs "28.1") (seq "2.3"))
;; Homepage: https://github.com/yourusername/dotconfig
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package refreshes a 14-digit timestamp embedded in a filename
;; (and any occurrences of that timestamp inside the file's contents),
;; replacing it with the current time. It also sweeps the rest of the
;; current project (via built-in `project.el') for other files sharing
;; that same old timestamp and updates them in lockstep, so that
;; related, timestamp-linked files stay in sync.
;;
;;; Code:
(require 'project)
(require 'seq)
(require 'magit)
(require 'git-tools)

(defgroup retimestamp nil
  "Refresh timestamped filenames and contents."
  :group 'files)

(defcustom retimestamp-regexp "\\([0-9]\\{14\\}\\)"
  "Regexp identifying the timestamp in filenames."
  :type 'regexp)

(defcustom retimestamp-format "%Y%m%d%H%M%S"
  "Format string passed to `format-time-string'."
  :type 'string)

(defcustom retimestamp-auto-commit nil
  "Rather or not to auto ammend commit changes."
  :type 'boolean)

(defun retimestamp--replace-in-buffer (old new)
  "Replace OLD in buffer with NEW string."
  (goto-char (point-min))
  (while (search-forward old nil t)
    (replace-match new t t)))

(defun retimestamp--new-path (file old new)
  "Replace OLD with NEW string in FILE."
  (expand-file-name
    (replace-regexp-in-string old new (file-name-nondirectory file) nil 'literal)
    (file-name-directory file)))

(defun retimestamp--process-file (file old new)
  "Process FILE replacing OLD with NEW string."
  (let ((buf (get-file-buffer file)))
    (if buf
      (with-current-buffer buf
        (save-excursion
          (save-restriction
            (widen)
            (retimestamp--replace-in-buffer old new)))
        (save-buffer))
      (with-temp-buffer
        (insert-file-contents file)
        (retimestamp--replace-in-buffer old new)
        (write-region (point-min) (point-max) file nil 'silent)))
    (let ((new-file (retimestamp--new-path file old new)))
      (unless (string-equal file new-file)
        (rename-file file new-file t)
        (when retimestamp-auto-commit
          (magit-stage-files (list file new-file)))
        (when buf
          (with-current-buffer buf
            (set-visited-file-name new-file t t))))
      new-file)))

(defun retimestamp--project-files ()
  "Return absolute paths for all files in the current project.
Uses built-in `project.el'.  Signal a `user-error' when point is
not inside a recognized project."
  (let ((project (project-current)))
    (unless project
      (user-error "Not inside a project"))
    (project-files project)))

;;;autoload
(defun retimestamp ()
  "Update timestamp in current file and related project files."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((filename (file-name-nondirectory buffer-file-name)))
    (unless (string-match retimestamp-regexp filename)
      (user-error "Filename does not contain a matching timestamp"))
    (let* ((old (match-string 0 filename))
            (new (format-time-string retimestamp-format))
            (current buffer-file-name)
            (count 0))
      ;; Process current file first.
      (retimestamp--process-file current old new)
      (setq count (1+ count))
      ;; Process remaining matching project files.
      (dolist (file (retimestamp--project-files))
        (unless (or (file-equal-p file current)
                  (not (string-match-p (regexp-quote old)
                         (file-name-nondirectory file))))
          (retimestamp--process-file file old new)
          (setq count (1+ count))))
      (when retimestamp-auto-commit
        (git-tools-commit-amend-no-edit))
      (message "Retimestamped %d file%s."
        count (if (= count 1) "" "s")))))
(provide 'retimestamp)

;;; retimestamp.el ends here
