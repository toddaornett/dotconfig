;;; yak.el --- Message generators for agents and perhaps humans -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Todd Ornett
;;
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: July 28, 2026
;; Modified: July 28, 2026
;; Version: 0.0.1
;; Keywords: vc tools agent llm convenience
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/toddaornett/dotconfig
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'git-tools)

(defun yak-implement (text)
  (interactive "MText: ")
  (let* ((branch-name (git-tools-current-branch-name))
         (output (concat
                   (format "In the current branch %s, " branch-name)
                   (format "please make this change %s." text))))
         (kill-new output)
         (message (format "yak-implement: current branch %s" branch-name))))

(defun yak-review-pull-request ()
  (interactive)
  (let* ((branch-name (git-tools-current-branch-name))
         (output (concat
                   (format "Please review the current branch %s." branch-name))))
         (kill-new output)
         (message (format "yak-review-pull-request: current branch %s" branch-name))))

(defun yak-review-respond-to-comment (text)
  (interactive "MText: ")
  (let* ((branch-name (git-tools-current-branch-name))
         (output (concat
                   (format "Concerning the current branch %s, please " branch-name)
                   (format "paste a reasonable concise response to this review comment %s." text))))
         (kill-new output)
         (message (format "yak-review-respond-to-comment: current branch %s" branch-name))))

(provide 'yak)
;;; yak.el ends here
