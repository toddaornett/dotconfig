;;; $DOOMDIR/config/ui.el --- ui config -*- lexical-binding: t -*-
(global-auto-revert-mode 1)

(setq window-divider-default-right-width 1)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-places t)
(window-divider-mode 1)

(set-face-attribute 'window-divider nil :foreground "yellow")
(set-face-attribute 'window-divider-first-pixel nil :foreground "yellow")
(set-face-attribute 'window-divider-last-pixel nil :foreground "yellow")

(defcustom tao/auto-visual-line-max-scan-length 5000
  "Maximum number of lines to scan when checking a buffer for
`tao/auto-visual-line-mode'. If the buffer has more lines than this,
scanning stops early and `tao/buffer-longest-line' returns whatever
longest length it has found so far (treated as a best-effort answer,
not an exhaustive one). This exists purely to keep the scan fast on
huge buffers; it has nothing to do with how long any individual line is.
Emacs's own `so-long-threshold' is about line *length* for performance
reasons; this variable is about buffer *line count* for the same
reason."
  :type 'integer
  :group 'convenience)

(defun tao/buffer-longest-line (&optional max-lines)
  "Return the length of the longest line among the first MAX-LINES
lines of the current buffer (or `tao/auto-visual-line-max-scan-length'
lines if MAX-LINES is nil). Only scans up to that many lines, regardless
of how long any individual line is."
  (let ((limit (or max-lines tao/auto-visual-line-max-scan-length))
         (max-len 0)
         (lines-checked 0))
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp)) (< lines-checked limit))
        (let ((len (- (line-end-position) (line-beginning-position))))
          (when (> len max-len)
            (setq max-len len)))
        (forward-line 1)
        (setq lines-checked (1+ lines-checked))))
    max-len))

(defun tao/auto-visual-line-mode ()
  "Enable or disable `visual-line-mode' based on whether the buffer's
longest line (up to `tao/auto-visual-line-max-scan-length' lines) exceeds
the current window's text width."
  (unless (minibufferp)
    (let ((needs-wrap (> (tao/buffer-longest-line) (window-body-width))))
      (cond
        ((and needs-wrap (not visual-line-mode))
          (visual-line-mode 1))
        ((and (not needs-wrap) visual-line-mode)
          (visual-line-mode -1))))))

(add-hook 'find-file-hook #'tao/auto-visual-line-mode)
(add-hook 'window-configuration-change-hook #'tao/auto-visual-line-mode)
