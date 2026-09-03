;;; json-tools.el --- json buffer utilities -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Todd Ornett
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: August 19, 2026
;; Modified: September 3, 2026
;; Version: 0.0.1
;; Keywords: convenience tools
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/toddaornett/dotconfig
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; WARNING:
;; This is not general-purpose yet and is rather hardcoded for a specific purpose now.
;;
;; This package provides some json tools.
;;
;;; Code:

(defun json-tools-modify-json-in-buffer (new-title)
  "Replace 'title' with NEW-TITLE and clear 'tags' in the current buffer.
Preserves all original spacing, indentation, and formatting."
  (interactive "sEnter new title: ")
  (save-excursion
    ;; 1. Replace the top-level title value
    (goto-char (point-min))
    (if (re-search-forward "\"title\"\\s-*:\\s-*\\(\"[^\"]*\"\\|[^,}\n]+\\)" nil t)
        (replace-match (concat "\"" new-title "\"") t t nil 1)
      (message "Warning: 'title' key not found."))

    ;; 2. Replace the tags array with []
    (goto-char (point-min))
    (if (re-search-forward "\"tags\"\\s-*:\\s-*\\(\\[[^]]*\\]\\)" nil t)
        (replace-match "[]" t t nil 1)
      (message "Warning: 'tags' key not found."))))

(provide 'json-tools)
;;; json-tools.el ends here
