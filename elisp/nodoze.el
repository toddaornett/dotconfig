;;; nodoze.el --- Prevent system from sleeping -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Nodoze prevents your system from sleeping for a specified number of hours
;; using the macOS `caffeinate` utility. Run `M-x nodoze` and enter the duration
;; in hours (e.g., 1 or 0.0167 for 1 minute). Logs activity to the `*nodoze*` buffer.
;; Requires macOS `caffeinate`.
;;
;;; Code:
(defgroup nodoze nil
  "Customize settings for the running of nodoze to keep the system awake."
  :group 'hardware)

(defcustom nodoze-command-options
  "-d -i -m -s -u -t"
  "The default arguments passed by nodoze to NODOZE_COMMAND_PROGRAM."
  :group 'nodoze
  :type "string")

(defcustom nodoze-command-program
  "caffeinate"
  "The default program for nodoze to keep system awake."
  :group 'nodoze
  :type "string")

(defun nodoze (hours)
  "Prevent the system from sleeping for HOURS, logging to *nodoze* buffer."
  (interactive "nHours to stay active: ")
  (let* ((seconds (round (* hours 3600))) ; Round to avoid floating-point issues
         (buffer (get-buffer-create "*nodoze*"))
         (command (format "nohup %s %s %d >/dev/null 2>&1 &" nodoze-command-program nodoze-command-options seconds))
         (unit (if (= hours 1) "hour" "hours")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "[%s] Starting caffeinate for %s %s\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))
                        hours unit))))
    (display-buffer buffer)
    (with-current-buffer buffer
      (redisplay t))
    (shell-command "pkill -u $USER caffeinate" nil nil)
    (shell-command command nil nil)
    (run-at-time seconds nil
                 (lambda ()
                   (with-current-buffer buffer
                     (let ((inhibit-read-only t))
                     (goto-char (point-max))
                     (insert (format "[%s] Caffeinate finished - system can now sleep\n"
                                     (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))))))))
    (message "Caffeinate started for %s %s (logging to *nodoze* buffer)" hours unit)))

(provide 'nodoze)
;;; nodoze.el ends here
