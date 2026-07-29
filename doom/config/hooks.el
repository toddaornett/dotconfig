;;; $DOOMDIR/config/hooks.el --- various hooks -*- lexical-binding: t; -*-

(defun tao/update-modified-timestamp ()
  (when  (derived-mode-p 'emacs-lisp-mode)
      (save-excursion
        (goto-char (point-min))
        (when (search-forward ";; Modified: July 30, 2026
          (delete-region (point) (line-end-position))
          (insert (format-time-string "%B %d, %Y"))))))
(add-hook 'before-save-hook #'tao/update-modified-timestamp)
