;;; $DOOMDIR/config/actions.el --- user actions -*- lexical-binding: t -*-

(defvar-local tao-last-region-start nil
  "Marker tracking the start of the last selected region.")

(defvar-local tao-last-region-end nil
  "Marker tracking the end of the last selected region.")

(defvar tao-last-region-buffer nil
  "The buffer where the last region was captured.")

(defun tao/save-last-region ()
  "Save the boundaries of the current region and its buffer."
  (when (region-active-p)
    (unless tao-last-region-start (setq tao-last-region-start (make-marker)))
    (unless tao-last-region-end (setq tao-last-region-end (make-marker)))
    (set-marker tao-last-region-start (region-beginning))
    (set-marker tao-last-region-end (region-end))
    (setq tao-last-region-buffer (current-buffer))))

(add-hook 'post-command-hook #'tao/save-last-region)

(defun tao/reselect-last-region ()
  "Reselect the previously selected region and move point to its start.
Switches to the region's buffer if called from a different one."
  (interactive)
  (let ((current-buf (current-buffer)))
    (cond
     ((not (buffer-live-p tao-last-region-buffer))
      (message "No previously selected region is available."))

     ((not (eq current-buf tao-last-region-buffer))
      (with-current-buffer tao-last-region-buffer
        (if (and tao-last-region-start
                 tao-last-region-end
                 (marker-position tao-last-region-start)
                 (marker-position tao-last-region-end)
                 (/= (marker-position tao-last-region-start)
                     (marker-position tao-last-region-end)))
            (let ((old-buffer-name (buffer-name current-buf)))
              (switch-to-buffer tao-last-region-buffer)
              (deactivate-mark)
              (goto-char (marker-position tao-last-region-end))
              (set-mark (point))
              (goto-char (marker-position tao-last-region-start))
              (activate-mark)
              (message "Switched from %s." old-buffer-name))
          (message "Previously selected region buffer %s was deleted."
                   (buffer-name tao-last-region-buffer)))))

     (t
      (if (and tao-last-region-start
               tao-last-region-end
               (marker-position tao-last-region-start)
               (marker-position tao-last-region-end)
               (/= (marker-position tao-last-region-start)
                   (marker-position tao-last-region-end)))
          (progn
            (deactivate-mark)
            (goto-char (marker-position tao-last-region-end))
            (set-mark (point))
            (goto-char (marker-position tao-last-region-start))
            (activate-mark))
        (message "Previously selected region was deleted or is unavailable."))))))

;; redefine exit behavior
(defun tao/save-and-kill-emacs-silently ()
  "Save all file buffers silently, prompt for new non-temp buffers, and exit without asking."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (buffer-modified-p)
        (let ((name (buffer-name))
              (file (buffer-file-name)))
          (cond
           (file
            (ignore-errors
              (save-buffer)))
           ((and (not file)
                 (not (string-prefix-p " " name))
                 (not (string-prefix-p "*" name)))
            (setq-local buffer-offer-save t)))))))
  (ignore-errors
    (save-some-buffers t))
  (cl-letf (((symbol-function 'process-list) (lambda () nil))
            ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
            ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
            (kill-emacs-query-functions nil)
            (confirm-kill-emacs nil))
    (kill-emacs)))
(global-set-key (kbd "C-x C-c") 'tao/save-and-kill-emacs-silently)
