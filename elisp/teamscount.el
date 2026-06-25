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
;; Clicking the segment opens a buffer listing unread channels with links.
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

(defcustom teamscount-icon-name "nf-md-microsoft_teams"
  "The icon name id for the Nerd Fonts icon."
  :type 'string
  :group 'teamscount)

(defcustom teamscount-icon-fg-color "#6264A7"
  "Foreground color of Microsoft Teams icon.
#6264A7 — official color.
#8B8CC8 — lighter purple, still recognizably Teams
#A0A2D4 — even lighter, more pastel
#7B83EB — the brighter Teams light variant Microsoft uses in some contexts."
  :type '(choice (color :tag "Pick a color")
          (string :tag "Hex code (e.g., #6264A7)"))
  :group 'teamscount)

(defvar teamscount--timer nil
  "Timer object for periodic refresh.")

(defvar teamscount--mode-line-string ""
  "Current mode line string for teamscount.")
(put 'teamscount--mode-line-string 'risky-local-variable t)

(defvar teamscount--alerted nil
  "Non-nil if the mention alert sound has already been played this cycle.")

(defvar teamscount--last-data nil
  "Last parsed data plist from teamscount--fetch.")

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

(defun teamscount-available-p ()
  "Return non-nil if both Python and script are available."
  (and (teamscount--python-available-p)
       (teamscount--script-available-p)))

(defun teamscount--fetch ()
  "Fetch Teams unread data and return a plist.
Format is (:unreads N :mentions N :threads [...]).
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
                      :mentions (alist-get 'mentions data 0)
                      :threads  (alist-get 'threads  data [])))
            (message "teamscount: script exited with code %d" exit-code)
            nil)))
    (error
     (message "teamscount: error running script: %s" (error-message-string err))
     nil)))

(defun teamscount--make-teams-url (thread)
  "Construct a msteams:// deep link URL for THREAD alist."
  (let* ((thread-id (alist-get 'id       thread ""))
         (group-id  (alist-get 'group_id thread nil))
         (msg-id    (alist-get 'oldest_unread_id thread nil)))
    (if group-id
        ;; Channel in a team — deep link to the channel
        (concat "msteams://teams/l/channel/"
                (url-hexify-string thread-id)
                "/channel"
                "?groupId=" group-id
                (when msg-id (concat "&messageId=" msg-id)))
      ;; Direct chat or space without a group
      (concat "msteams://teams/l/chat/"
              (url-hexify-string thread-id)
              "/0"
              (when msg-id (concat "?messageId=" msg-id))))))

(defun teamscount-show-unreads ()
  "Open a buffer listing unread Teams channels with clickable links."
  (interactive)
  (let* ((data    teamscount--last-data)
         (threads (when data (plist-get data :threads)))
         (buf     (get-buffer-create "*Teams Unreads*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Teams Unread Channels\n" 'face 'bold))
        (insert (make-string 40 ?─) "\n\n")
        (if (or (null threads) (zerop (length threads)))
            (insert "No unread messages.\n")
          (seq-do
           (lambda (thread)
             (let* ((name    (alist-get 'name    thread "Unknown"))
                    (mention (alist-get 'mention thread nil))
                    (url     (teamscount--make-teams-url thread))
                    (label   (if mention
                                 (propertize (concat "[@] " name) 'face '(:foreground "orange" :weight bold))
                               (propertize (concat "[ ] " name) 'face 'default))))
               (insert-button label
                              'action (lambda (_) (browse-url url))
                              'follow-link t
                              'help-echo (concat "Open in Teams: " url))
               (insert "\n")))
           threads))
        (insert "\n")
        (insert (propertize "Press q to close, RET or click to open in Teams.\n"
                            'face 'font-lock-comment-face)))
      (special-mode)
      (local-set-key (kbd "q") #'quit-window)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun teamscount--format (data)
  "Format DATA (a plist from teamscount--fetch) into a mode-line string."
  (let* ((icon-color-spec `(:foreground ,teamscount-icon-fg-color))
         (icon (nerd-icons-mdicon teamscount-icon-name))
         (click-map (let ((map (make-sparse-keymap)))
                      (define-key map [mode-line mouse-1] #'teamscount-show-unreads)
                      map)))

    ;; Apply dynamic color properties securely to the base icon layout
    (add-face-text-property 0 (length icon) icon-color-spec t icon)
    (put-text-property 0 (length icon) 'display '(raise 0.05) icon)

    (if (null data)
        (concat icon " ")
      (let* ((unreads (plist-get data :unreads))
             (mentions (plist-get data :mentions))
             (counts (cond ((> mentions 0)
                            (unless teamscount--alerted
                              (teamscount--play-alert)
                              (setq teamscount--alerted t))
                            (let ((m-str (format "%d" mentions)))
                              (add-face-text-property 0 (length m-str) '(:foreground "orange") t m-str)
                              (concat m-str (when (> unreads 0) (format "/%d" unreads)))))
                           ((> unreads 0)
                            (setq teamscount--alerted nil)
                            (format "%d" unreads))
                           (t (setq teamscount--alerted nil) nil))))
        (if counts
            (concat (propertize icon
                                'mouse-face 'mode-line-highlight
                                'help-echo "Teams unreads — click to view"
                                'local-map click-map)
                    (propertize (concat counts " ")
                                'mouse-face 'mode-line-highlight
                                'help-echo "Teams unreads — click to view"
                                'local-map click-map))
          ;; Fallback string if there are no mentions or unreads
          (concat icon " "))))))

(defun teamscount-update ()
  "Fetch latest data and rebuild the mode line string."
  (interactive)
  (setq teamscount--last-data (teamscount--fetch))
  (setq teamscount--mode-line-string (teamscount--format teamscount--last-data))
  (force-mode-line-update t))

(defun teamscount--start-timer ()
  "Start the periodic refresh timer if it isn't running."
  (unless teamscount--timer
    (setq teamscount--timer
          (run-at-time 0 teamscount-refresh-interval #'teamscount-update))))

(defun teamscount--stop-timer ()
  "Cancel and clear the periodic refresh timer."
  (when teamscount--timer
    (cancel-timer teamscount--timer)
    (setq teamscount--timer nil)))

;;;###autoload
(define-minor-mode teamscount-mode
  "Toggle Teams notification tracking in your mode line."
  :global t
  :group 'teamscount
  (if teamscount-mode
      (progn
        ;; Add the dynamic wrapper safely to the default mode-line-format sequence
        (unless (member '((:eval teamscount--mode-line-string)) global-mode-line-format)
          (setq-default global-mode-line-format
                        (append global-mode-line-format '((:eval teamscount--mode-line-string)))))
        ;; Execute initial capture and spin up loop
        (teamscount-update)
        (teamscount--start-timer))
    ;; Mode turned off: tear down timer and remove construct safely
    (teamscount--stop-timer)
    (setq-default global-mode-line-format
                  (delete '((:eval teamscount--mode-line-string)) global-mode-line-format))
    (force-mode-line-update t)))

(provide 'teamscount)
;;; teamscount.el ends here
