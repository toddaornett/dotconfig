;;; nodoze.el --- Prevent system from sleeping -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Nodoze is a simple Emacs package that prevents your system from sleeping
;; for a specified number of hours using the macOS `caffeinate` utility.
;; It provides a single interactive command, `nodoze`, which prompts for the
;; duration in hours and launches `caffeinate` in a detached process that
;; continues running even if Emacs exits. This is useful for long-running
;; tasks or when you need to keep your system awake without manual intervention.
;;
;; Usage:
;;   M-x nodoze RET
;;   Enter the number of hours (e.g., 1 or 0.0167 for 1 minute) when prompted.
;;
;; The command displays progress messages in the minibuffer and logs activity
;; to the `*nodoze*` buffer.
;; Note: This package requires `caffeinate`, which is macOS-specific.
;;
;;; Code:
(defun nodoze (hours)
  "Prevents the system from sleeping for HOURS, logging to *nodoze* buffer."
  (interactive "nHours to stay active: ")
  (let* ((seconds (* hours 3600))
         (buffer (get-buffer-create "*nodoze*"))
         (command (format "nohup caffeinate -d -i -m -s -u -t %d > /dev/null 2>&1 &" seconds)))
    ;; Ensure buffer is visible and clean
    (with-current-buffer buffer
      (erase-buffer)
      (display-buffer buffer))

    ;; Log start message
    (with-current-buffer buffer
      (insert (format "[%s] Starting caffeinate for %d hours\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))
                      hours)))

    ;; Kill existing caffeinate processes
    (shell-command "pkill -u $USER caffeinate" nil nil)

    ;; Start process with output redirection to buffer
    (let ((process (start-process-shell-command "nodoze" buffer command)))
      (set-process-filter process
                          (lambda (proc output)
                            (with-current-buffer (process-buffer proc)
                              (goto-char (point-max))
                              (insert (format "[%s] %s"
                                              (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))
                                              output))))))

    ;; Schedule end message
    (run-at-time seconds nil
                 (lambda ()
                   (with-current-buffer buffer
                     (goto-char (point-max))
                     (insert (format "[%s] Caffeinate finished - system can now sleep\n"
                                     (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))))))

    ;; Immediate feedback
    (message "Caffeinate started, logging to *nodoze* buffer for %d hours" hours)))

(provide 'nodoze)
;;; nodoze.el ends here
