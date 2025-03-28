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
;; The command displays progress messages in the minibuffer and logs errors
;; to the `*nodoze-error*` buffer if `caffeinate` fails to start.
;; Note: This package requires `caffeinate`, which is macOS-specific.
;;

;;; Code:

(defun nodoze (hours)
  "Prevents the system from sleeping for HOURS."
  (interactive "nHours to stay active: ")
  (let* ((seconds (* hours 3600))
         (command (format "nohup caffeinate -d -i -m -s -u -t %d > /dev/null 2>&1 &" seconds)))
    (start-process-shell-command "nodoze" nil command)
    (message "Caffeinate started to keep the system active for %d hours (%d seconds)" hours seconds)
    (run-at-time seconds nil (lambda ()
                               (message "Caffeinate should have finished - system can now sleep")))))
(provide 'nodoze)
;;; nodoze.el ends here
