;;; markdown-tools.el --- some markdown tools -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Todd Ornett
;;
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: May 08, 2026
;; Keywords: markdown tool url
;; Package-Requires: ((emacs "30.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides some markdown tools.
;;
;; The primary entry point is `markdown-tools-insert-human-url', which
;; formats a URL as a Markdown link using the final path segment as the
;; link text.  When invoked interactively or called from Lisp without
;; an argument, the URL is taken from the system clipboard if it
;; contains a non-blank string; otherwise the user is prompted to
;; enter one.
;;
;;; Code:
(require 'subr-x)

(defun markdown-tools--get-url-from-clipboard ()
  "Return a non-blank URL string from the clipboard or kill ring, or nil."
  (let ((clip (or (gui-get-selection 'CLIPBOARD 'STRING)
                  (gui-get-selection 'CLIPBOARD 'UTF8_STRING)
                  (and kill-ring (current-kill 0 t)))))
    (and clip (not (string-blank-p clip)) clip)))

(defun markdown-tools-insert-human-url (&optional url)
  "Insert a Markdown link for URL using the trailing path segment as text.
The link text is derived from the last non-empty segment of URL's path
\(i.e. the portion after the final slash).  If that segment consists
entirely of digits it is prefixed with \"#\", making it suitable for
use as a section or issue reference.  The resulting insertion has the
form:
    [text](URL)
URL is optional.  When omitted — whether called interactively or from
Lisp — the URL is taken from the system clipboard via
`gui-get-selection' if the clipboard contains a non-blank string.  If
the clipboard is empty or unavailable the user is prompted to enter a
URL in the minibuffer."
  (interactive)
  (let* ((url   (or url
                    (markdown-tools--get-url-from-clipboard)
                    (read-string "URL: ")))
         (parts (string-split url "/" t))
         (slug  (if parts (car (last parts)) ""))
         (text  (if (string-match-p "\\`[0-9]+\\'" slug)
                    (concat "#" slug)
                  slug)))
    (insert (format "[%s](%s)" text url))))

(provide 'markdown-tools)
;;; markdown-tools.el ends here
