;;; $DOOMDIR/config/hooks.el --- various hooks -*- lexical-binding: t; -*-
;; Created: July 30, 2026
;; Modified: August 1, 2026

(defun tao/update-modified-timestamp ()
  (when (derived-mode-p 'emacs-lisp-mode)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (when (re-search-forward "^;; Modified: " nil t)
            (when (nth 4 (syntax-ppss (match-end 0)))
              (delete-region (point) (line-end-position))
              (insert (format-time-string "%B %-d, %Y")))))))))
(add-hook 'before-save-hook #'tao/update-modified-timestamp)
