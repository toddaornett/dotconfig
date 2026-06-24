;;; teamscount.el --- Display Teams unread counts in the mode line -*- lexical-binding: t -*-

;; Author: Todd Ornett
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, teams
;; Homepage: https://github.com/toddaornett/dotconfig

;;; Commentary:

;; teamscount.el reads Microsoft Teams 2.0's local IndexedDB via a small
;; Python helper script and displays unread and mention counts in the mode line.
;;
;; The mode line segment shows:
;;   "<icon><mentions>/<unreads> "
;; where mentions are highlighted in orange when non-zero.
;;
;; Usage:
;;   (use-package teamscount
;;     :load-path "~/path/to/teamscount"
;;     :if (teamscount-available-p)
;;     :config (teamscount-mode 1))
;;
;; Customise via M-x customize-group RET teamscount RET

;;; Code:

(require 'json)
(require 'nerd-icons)
(unless (and (fboundp 'play-sound-internal)
             (subrp (symbol-function 'play-sound-internal)))
  (require 'play-sound))

(defgroup teamscount nil
  "Display Teams unread counts in the mode line."
  :group 'convenience
  :prefix "teamscount-")

(defcustom teamscount-alert-sound t
  "Whether to play a sound when there are direct mentions.
If t, play the system beep.
If a string, treat it as a path to a sound file to play.
If nil, no sound is played."
  :type '(choice (const :tag "System beep" t)
          (string :tag "Sound file path")
          (const :tag "No sound" nil))
  :group 'teamscount)

(defcustom teamscount-python-executable
  (expand-file-name "~/.config/pylib/teamscount/.venv/bin/python3")
  "Path to the Python executable (inside the teamscount venv)."
  :type 'file
  :group 'teamscount)

(defcustom teamscount-script
  (expand-file-name "~/.config/pylib/teamscount/teamscount.py")
  "Path to the teamscount.py helper script."
  :type 'file
  :group 'teamscount)

(defcustom teamscount-refresh-interval 30
  "How often (in seconds) to refresh the Teams unread counts."
  :type 'integer
  :group 'teamscount)

(defvar teamscount--timer nil
  "Timer object for periodic refresh.")

(defvar teamscount--mode-line-string ""
  "Current mode line string for teamscount.")
(put 'teamscount--mode-line-string 'risky-local-variable t)

(defvar teamscount--alerted nil
  "Non-nil if the mention alert sound has already been played this cycle.")

(defun teamscount--play-alert ()
  "Play the configured alert sound."
  (when teamscount-alert-sound
    (cond
     ((stringp teamscount-alert-sound)
      (when (file-exists-p teamscount-alert-sound)
        (play-sound `(sound :file ,teamscount-alert-sound))))
     (t
      (beep)))))

(defun teamscount--python-available-p ()
  "Return non-nil if the Python venv executable exists."
  (file-executable-p teamscount-python-executable))

(defun teamscount--script-available-p ()
  "Return non-nil if the teamscount.py script exists."
  (file-readable-p teamscount-script))

(defun teamscount--fetch ()
  "Fetch Teams unread data and return a plist (:unreads N :mentions N).
Returns nil if data cannot be retrieved."
  (unless (teamscount--python-available-p)
    (message "teamscount: Python not found at %s" teamscount-python-executable)
    (cl-return-from teamscount--fetch nil))
  (unless (teamscount--script-available-p)
    (message "teamscount: script not found at %s" teamscount-script)
    (cl-return-from teamscount--fetch nil))
  (condition-case err
      (with-temp-buffer
        (let ((exit-code (call-process teamscount-python-executable
                                       nil t nil
                                       teamscount-script)))
          (if (zerop exit-code)
              (let* ((output (string-trim (buffer-string)))
                     (data   (json-read-from-string output)))
                (list :unreads  (alist-get 'unreads  data 0)
                      :mentions (alist-get 'mentions data 0)))
            (message "teamscount: script exited with code %d" exit-code)
            nil)))
    (error
     (message "teamscount: error running script: %s" (error-message-string err))
     nil)))

(defun teamscount--format (data)
  "Format DATA (a plist from `teamscount--fetch') into a mode-line string."
  (let ((icon (propertize (nerd-icons-mdicon "nf-md-microsoft_teams")
                          'face '(:foreground "#6264A7")
                          'display '(raise 0.05))))
    (if (null data)
        (concat icon "? ")
      (let* ((unreads  (plist-get data :unreads))
             (mentions (plist-get data :mentions)))
        (cond
         ;; Direct mentions take priority — alert and show in orange
         ((> mentions 0)
          (unless teamscount--alerted
            (teamscount--play-alert)
            (setq teamscount--alerted t))
          (concat icon
                  (propertize (format "%d" mentions)
                              'face '(:foreground "orange"))
                  (when (> unreads 0)
                    (format "/%d" unreads))
                  " "))
         ;; Unreads only
         ((> unreads 0)
          (setq teamscount--alerted nil)
          (concat icon (format "%d " unreads)))
         ;; Nothing to show
         (t
          (setq teamscount--alerted nil)
          ""))))))

(defun teamscount-update ()
  "Refresh the Teams unread count and update the mode line."
  (interactive)
  (let ((data (teamscount--fetch)))
    (setq teamscount--mode-line-string (teamscount--format data))
    (force-mode-line-update t)))

;;;###autoload
(defun teamscount-available-p ()
  "Return non-nil if the Python venv and teamscount script are both present."
  (and (teamscount--python-available-p)
       (teamscount--script-available-p)))

;;;###autoload
(define-minor-mode teamscount-mode
  "Minor mode to display Teams unread counts in the mode line.

When enabled, the mode line shows mention counts (orange when non-zero)
and general unread counts, refreshed every `teamscount-refresh-interval'
seconds."
  :global t
  :lighter nil
  :group 'teamscount
  (if teamscount-mode
      (if (not (teamscount-available-p))
          (progn
            (message "teamscount: disabled — Python venv or script not found")
            (setq teamscount-mode nil))
        (unless (memq 'teamscount--mode-line-string global-mode-string)
          (add-to-list 'global-mode-string '(:eval teamscount--mode-line-string) t))
        (teamscount-update)
        (setq teamscount--timer
              (run-at-time t teamscount-refresh-interval #'teamscount-update)))
    (when teamscount--timer
      (cancel-timer teamscount--timer)
      (setq teamscount--timer nil))
    (setq global-mode-string
          (delete '(:eval teamscount--mode-line-string) global-mode-string))
    (setq teamscount--mode-line-string "")))

(provide 'teamscount)

;;; teamscount.el ends here
