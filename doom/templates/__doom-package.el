;;; `(file-name-nondirectory (or (buffer-file-name) ""))` --- $1 -*- lexical-binding: t; -*-

;; Copyright (C) `(format-time-string "%Y")`

;; Author: `(or user-full-name "")`
;; Maintainer: `(or user-full-name "")`
;; Homepage: https://github.com/toddaornett/dotconfig/tree/main/elisp/`(file-name-base (or (buffer-file-name) ""))`
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;; Keywords: `(let ((dir (file-name-nondirectory
(directory-file-name
 (file-name-directory (or (buffer-file-name) ""))))))
(concat dir " lisp"))`
;;; Commentary:

;;; Code:
$0
(provide '`(file-name-base (or (buffer-file-name) ""))`)
;;; `(file-name-nondirectory (or (buffer-file-name) ""))` ends here
