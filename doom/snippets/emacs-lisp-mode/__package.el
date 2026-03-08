# -*- mode: snippet -*-
# name: doom-package
# key: __doom-package.el
# --
;;; `(file-name-nondirectory (buffer-file-name))` --- ${1:Description} -*- lexical-binding: t; -*-
;;
;; Copyright (C) `(format-time-string "%Y")` `(user-full-name)`
;;
;; Author: `(user-full-name)` <`(user-email)`>
;; Maintainer: `(user-full-name)` <`(user-email)`>
;; Created: `(format-time-string "%B %d, %Y")`
;; Modified: `(format-time-string "%B %d, %Y")`
;; Version: 0.0.1
;; Keywords: `(tao/snippet-keywords-from-description yas-text)`
;; Homepage: `(git-tools-remote-origin-url)`
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; ${2:Commentary}
;;
;;; Code:
$0
(provide '`(file-name-base (buffer-file-name))`)
;;; `(file-name-nondirectory (buffer-file-name))` ends here
