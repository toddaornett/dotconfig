;;; ez-buffers.el --- Extra commands for buffer management -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Todd Ornett
;;
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: November 6, 2026
;; Modified: November 6, 2026
;; Version: 0.0.1
;; Keywords: tools buffers
;; Package-Requires: ((emacs "31.1"))
;; Homepage: https://github.com/toddaornett/dotconfig
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Provide extra functions for buffer management.
;;
;;; Code:

(defun ez-buffers-kill-all-temporary-buffers ()
  "Kill all buffers that start with an asterisk or a space."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and (not (string-equal name "*scratch*"))
              (or (string-prefix-p "*" name)
                (string-prefix-p " " name)))
        (kill-buffer buffer)))))

(provide 'ez-buffers)
;;; ez-buffers.el ends here
