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

(defun yak-review-pull-request (arg)
  (interactive "P")
  (let* ((branch-name (git-tools-current-branch-name))
         (count (if (null arg) 0 (prefix-numeric-value arg)))
         (prefix (if (> count 0)
                     (format "Please review the latest %d commits in the current branch" count)
                   "Please review the latest commit in the current branch"))
         (output (format "%s %s." prefix branch-name))
         (suffix (concat ", providing a concise approval or changes requested verdict only for breaking issues. "
                          "Also provide a short list of a few comments for improvement if applicable."))
         (output (format "%s %s%s" prefix branch-name suffix)))
    (kill-new output)
    (message "yak-review-pull-request: current branch %s" branch-name)))

(defun yak-review-respond-to-comment (text)
  (interactive "MText: ")
  (let* ((branch-name (git-tools-current-branch-name))
         (output (concat
                   (format "Concerning the current branch %s, please " branch-name)
                   (format "paste a reasonable concise response to this review comment %s." text))))
         (kill-new output)
         (message (format "yak-review-respond-to-comment: current branch %s" branch-name))))

(defun yak-update-for-comment (text)
  (interactive "MText: ")
  (let* ((branch-name (git-tools-current-branch-name))
         (output (concat
                   (format "Concerning the current branch %s, please " branch-name)
                   (format "make an update to the implementation based on this review comment: %s." text))))
         (kill-new output)
         (message (format "yak-update-for-comment: current branch %s" branch-name))))

(provide 'yak)
;;; yak.el ends here
