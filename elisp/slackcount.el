;;; slackcount.el --- Display Slack unread counts in the mode line -*- lexical-binding: t -*-

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, slack
;; Homepage: https://github.com/toddaornett/dotconfig

;;; Commentary:

;; slackcount.el reads Slack's local root-state.json file and displays
;; direct-message highlights and unread counts in the Emacs mode line.
;;
;; The mode line segment shows:
;;   "# <highlights><unreads-indicator>"
;; where <unreads-indicator> is "*" when there are unreads, " " otherwise.
;;
;; Usage:
;;   (use-package slackcount
;;     :load-path "~/path/to/slackcount.el"
;;     :if (slackcount-available-p)
;;     :config (slackcount-mode 1))
;;
;; Customise the refresh interval and jq path via M-x customize-group RET slackcount RET

;;; Code:

(require 'json)
(require 'nerd-icons)
(unless (and (fboundp 'play-sound-internal)
          (subrp (symbol-function 'play-sound-internal)))
  (require 'play-sound))

(defgroup slackcount nil
  "Display Slack unread counts in the mode line."
  :group 'convenience
  :prefix "slackcount-")

(defcustom slackcount-enabled t
  "Rather to enable slackcount or not."
  :type 'boolean
  :group 'slackcount)

;; Customization variables - add these with your other defcustom declarations
(defcustom slackcount-alert-sound t
  "Whether to play a sound when there are direct messages.
If t, play the system beep.
If a string, treat it as a path to a sound file to play.
If nil, no sound is played."
  :type '(choice (const :tag "System beep" t)
           (string :tag "Sound file path")
           (const :tag "No sound" nil))
  :group 'slackcount)

(defcustom slackcount-state-file
  (expand-file-name "~/Library/Application Support/Slack/storage/root-state.json")
  "Path to Slack's root-state.json file."
  :type 'file
  :group 'slackcount)

(defcustom slackcount-jq-executable
  "/opt/homebrew/bin/jq"
  "Path to the jq executable."
  :type 'file
  :group 'slackcount)

(defcustom slackcount-refresh-interval 30
  "How often (in seconds) to refresh the Slack unread counts."
  :type 'integer
  :group 'slackcount)

(defvar slackcount--timer nil
  "Timer object for periodic refresh.")

(defvar slackcount--mode-line-string ""
  "Current mode line string for slackcount.")
(put 'slackcount--mode-line-string 'risky-local-variable t)

(defvar slackcount--alerted nil
  "Non-nil if the direct-message alert sound has already been played this cycle.")

(defun slackcount--play-alert ()
  "Play the configured alert sound."
  (when slackcount-alert-sound
    (cond
      ((stringp slackcount-alert-sound)
        (when (file-exists-p slackcount-alert-sound)
          (play-sound `(sound :file ,slackcount-alert-sound))))
      (t
        (beep)))))

(defun slackcount--jq-available-p ()
  "Return non-nil if jq is available at `slackcount-jq-executable'."
  (file-executable-p slackcount-jq-executable))

(defun slackcount--state-file-exists-p ()
  "Return non-nil if the Slack state file exists."
  (file-readable-p slackcount-state-file))

(defun slackcount--run-jq (query)
  "Run jq with QUERY against `slackcount-state-file'.
Returns the raw output string, or nil on failure."
  (condition-case err
    (with-temp-buffer
      (let ((exit-code (call-process slackcount-jq-executable
                         nil t nil
                         query
                         slackcount-state-file)))
        (if (zerop exit-code)
          (string-trim (buffer-string))
          (message "slackcount: jq exited with code %d" exit-code)
          nil)))
    (error
      (message "slackcount: error running jq: %s" (error-message-string err))
      nil)))

(defun slackcount--fetch ()
  "Fetch Slack unread data and return a plist (:direct N :unreads N).
Returns nil if data cannot be retrieved."
  (cond
    ((not (slackcount--jq-available-p))
      (message "slackcount: jq not found at %s" slackcount-jq-executable)
      nil)
    ((not (slackcount--state-file-exists-p))
      (message "slackcount: state file not found at %s" slackcount-state-file)
      nil)
    (t
      (let* ((team-id     (slackcount--run-jq ".webapp.teams | keys[0]"))
              (direct-str  (when team-id
                             (slackcount--run-jq
                               (format ".webapp.teams.%s.unreads.unreadHighlights" team-id))))
              (unreads-str (when team-id
                             (slackcount--run-jq
                               (format ".webapp.teams.%s.unreads.unreads" team-id)))))
        (when (and direct-str unreads-str)
          (list :direct  (string-to-number direct-str)
            :unreads (string-to-number unreads-str)))))))

(defun slackcount--format (data)
  "Format DATA (a plist from `slackcount--fetch') into a mode-line string."
  (let ((icon (propertize (nerd-icons-faicon "nf-fa-slack")
                'face '(:foreground "#E01E5A")
                'display '(raise 0.05))))
    (if (null data)
      (concat icon "? ")
      (let* ((direct    (plist-get data :direct))
              (unreads   (plist-get data :unreads))
              (indicator (if (> unreads 0) "*" "")))
        (cond
          ((> direct 0)
            (unless slackcount--alerted
              (slackcount--play-alert)
              (setq slackcount--alerted t))
            (concat icon
              (propertize (format "%d%s " direct indicator)
                'face '(:foreground "orange"))))
          ((> unreads 0)
            (setq slackcount--alerted nil)
            (concat icon (format "%d%s " unreads indicator)))
          (t
            (setq slackcount--alerted nil)
            ""))))))

(defun slackcount--clear-mode-line ()
  "Blank out the slackcount mode-line segment."
  (setq slackcount--mode-line-string "")
  (force-mode-line-update t))

(defun slackcount-update ()
  "Refresh the Slack unread count and update the mode line.
If `slackcount-enabled' is nil, this just clears the mode-line segment
instead of fetching, so that turning the variable off removes
slackcount from the mode line even while `slackcount-mode' itself is
still on."
  (interactive)
  (if (not slackcount-enabled)
    (slackcount--clear-mode-line)
    (let ((data (slackcount--fetch)))
      (setq slackcount--mode-line-string (slackcount--format data))
      (force-mode-line-update t))))

;;;###autoload
(defun slackcount-available-p ()
  "Return non-nil if the Slack state file and jq executable are both present."
  (and (slackcount--jq-available-p)
    (slackcount--state-file-exists-p)))

;;;###autoload
(define-minor-mode slackcount-mode
  "Minor mode to display Slack unread counts in the mode line.

When enabled, the mode line shows direct-message highlights and an
indicator for general unreads, refreshed every `slackcount-refresh-interval'
seconds."
  :global t
  :lighter nil
  :group 'slackcount
  (if (and slackcount-enabled slackcount-mode)
    (if (not (slackcount-available-p))
      (progn
        (message "slackcount: disabled — jq or Slack state file not found")
        (setq slackcount-mode nil))
      ;; Add segment to global mode line if not already present.
      (unless (member '(:eval slackcount--mode-line-string) global-mode-string)
        (setq global-mode-string (append global-mode-string (list '(:eval slackcount--mode-line-string)))))
      (slackcount-update)
      (setq slackcount--timer
        (run-at-time t slackcount-refresh-interval #'slackcount-update)))
    ;; Disable: cancel timer and remove mode line segment.
    (when slackcount--timer
      (cancel-timer slackcount--timer)
      (setq slackcount--timer nil))
    (setq global-mode-string (delete '(:eval slackcount--mode-line-string) global-mode-string))
    (setq slackcount--mode-line-string "")
    (force-mode-line-update t)))

(defun slackcount--enabled-watcher (symbol newval operation _where)
  "Watch `slackcount-enabled' and sync the mode line immediately.
Runs whenever SYMBOL (`slackcount-enabled') is set to NEWVAL via
OPERATION.  Only acts while `slackcount-mode' is on; when the mode
itself is toggled, `slackcount-mode' already handles the mode line."
  (when (and (eq symbol 'slackcount-enabled)
          (eq operation 'set)
          (bound-and-true-p slackcount-mode))
    (if newval
      ;; Re-enabled while the mode is on — fetch and redraw right away.
      (slackcount-update)
      ;; Disabled while the mode is on — strip the segment immediately.
      (slackcount--clear-mode-line))))

(add-variable-watcher 'slackcount-enabled #'slackcount--enabled-watcher)

(provide 'slackcount)
;;; slackcount.el ends here
