;;; irc.el --- ERC config -*- lexical-binding: t; -*-

(setq auth-sources '("~/.authinfo"))

(defcustom irc-channels '("#emacs" "#linux")
  "Channels to autojoin."
  :type '(repeat string))

(defcustom irc-nickname nil
  "Your IRC nickname for Libera.Chat, used for both connecting and SASL auth."
  :type '(choice (const nil) string))

(defun +irc/connect ()
  (interactive)
  (require 'erc)
  (erc-tls :server erc-server :port erc-port :nick irc-nickname))

(defun tao/erc-ci-rx (str)
  "Build a regexp that matches STR case-insensitively, regardless of `case-fold-search'."
  (mapconcat (lambda (c)
               (if (and (characterp c)
                     (or (and (>= c ?a) (<= c ?z))
                       (and (>= c ?A) (<= c ?Z))))
                 (format "[%c%c]" (downcase c) (upcase c))
                 (regexp-quote (string c))))
    str ""))

(after! erc
  (when (and (stringp irc-nickname)
          (not (string-empty-p irc-nickname)))
    (add-to-list 'erc-modules 'sasl)
    (add-to-list 'erc-modules 'nicks) ; colorize sender nicknames consistently per-nick
    (erc-update-modules)
    (setq erc-nick irc-nickname   ; keep erc-nick in sync so other code paths see it too
      erc-prompt-for-password nil
      erc-server "irc.libera.chat"
      erc-port 6697
      erc-ssl t
      erc-autojoin-channels-alist (list (cons (tao/erc-ci-rx "libera.chat") irc-channels))
      erc-sasl-mechanism 'plain
      erc-sasl-user irc-nickname
      erc-sasl-password (let* ((res (car (auth-source-search :user irc-nickname :host "irc.libera.chat" :max 1)))
                                (secret (plist-get res :secret)))
                          (if (functionp secret) (funcall secret) secret))
      ;; Timestamps on the left for every line, whether a user message
      ;; or a server command (/join, /quit, /part, ...).
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-timestamp-format "%H:%M "
      erc-timestamp-only-if-changed-flag t ; don't repeat timestamp on every consecutive line
      erc-fill-wrap-margin-side 'left
      erc-fill-function 'erc-fill-wrap))) ; margin-based fill: left stamps on all line types

(after! erc-stamp
  (defun tao/erc-left-align-prompt (&rest _)
    "Left-align the ERC prompt in the stamp margin (no leading padding).
With `erc-fill-wrap-margin-side' set to `left', ERC right-aligns the
prompt within the stamp margin, indenting it.  Re-pad it to the left."
    (when (and erc-stamp--display-margin-mode
            erc-stamp--margin-left-p
            erc-stamp--last-prompt)
      (setq erc-stamp--last-prompt
        (propertize (string-pad (string-trim-left erc-stamp--last-prompt)
                      left-margin-width nil nil)
          'font-lock-face 'erc-prompt-face))
      (put-text-property erc-insert-marker (1- erc-input-marker)
        'display `((margin left-margin) ,erc-stamp--last-prompt))))
  (advice-add 'erc-stamp--display-prompt-in-left-margin :after
    #'tao/erc-left-align-prompt))

;; =========================================================================
;; 1. SIDEBAR WINDOW LAYOUT & BUFFERS
;; =========================================================================

(set-popup-rule! "^\\*ERC Members\\*$"
  :side 'right
  :size 25
  :ttl nil
  :quit nil
  :select nil ; Instructs Doom's display engine NEVER to focus the window on spawn.
  :window-parameters '((no-delete-other-windows . t)
                        (no-other-window . t)))

(defvar tao/erc-members-buffer "*ERC Members*")
(defvar tao/erc--refreshing nil
  "Internal flag used to prevent recursive infinite loops during display updates.")

(define-derived-mode tao/erc-members-mode tabulated-list-mode "ERC Members"
  "Major mode for displaying ERC channel members in a tabulated list."
  (setq tabulated-list-padding 1))

;; =========================================================================
;; 2. DATA COLLECTION & RENDERING LOGIC
;; =========================================================================

(defun tao/erc--users ()
  (cond
    ((boundp 'erc-channel-users)
      (cond
        ((hash-table-p erc-channel-users)
          (let (xs)
            (maphash (lambda (k v) (push (cons k v) xs)) erc-channel-users)
            xs))
        ((listp erc-channel-users) erc-channel-users)
        (t nil)))
    (t nil)))

(defun tao/erc-members-refresh (&optional buf)
  (interactive)
  ;; Block execution if this function triggered a layout shift that recalled itself
  (unless tao/erc--refreshing
    (setq buf (or buf (current-buffer)))
    (when (buffer-live-p buf)
      (let ((tao/erc--refreshing t)) ; Raise recursion shield flag
        (let* ((users (with-current-buffer buf (tao/erc--users)))
                (member-count (length users))
                (channel-name (with-current-buffer buf
                                (or (and (boundp 'erc-default-target) erc-default-target)
                                  (buffer-name))))
                (sidebar (get-buffer-create tao/erc-members-buffer)))

          (with-current-buffer sidebar
            (tao/erc-members-mode)

            ;; Dynamically set the column header: "Nick (#) <channel name>" on one line
            (setq tabulated-list-format
              (vector '("P" 2 t)
                (list (format "Nick (%d) %s" member-count channel-name) 24 t)))

            ;; Force the layout engine to recalculate and redraw the top header bar
            (tabulated-list-init-header)

            ;; Process and sort the user list rows
            (setq tabulated-list-entries
              (mapcar
                (lambda (it)
                  (let* ((nick (format "%s" (car it)))
                          (obj (cdr it))
                          (prefix (or (and (boundp 'erc-channel-user-prefix)
                                        (ignore-errors (erc-channel-user-prefix obj)))
                                    "")))
                    (list nick (vector prefix nick))))
                (sort (copy-sequence users)
                  (lambda (a b) (string-lessp (car a) (car b))))))

            (tabulated-list-print t))

          ;; Draw the window layout safely without taking cursor focus away
          (display-buffer sidebar))))))

;; =========================================================================
;; 3. BACKGROUND TIMERS & TRACKING CALLBACKS
;; =========================================================================

(defvar tao/erc-member-timer nil
  "Holds the timer object for the periodic member list updates.")

(defun tao/erc-timer-callback ()
  "Helper callback to ensure the timer uses the correct active ERC buffer."
  (let ((current-buf (current-buffer)))
    (if (with-current-buffer current-buf (derived-mode-p 'erc-mode))
      (tao/erc-members-refresh current-buf)
      ;; Fallback: If current buffer isn't ERC, look for any visible ERC buffer
      (let ((erc-win (cl-find-if (lambda (w)
                                   (with-current-buffer (window-buffer w)
                                     (derived-mode-p 'erc-mode)))
                       (window-list))))
        (when erc-win
          (tao/erc-members-refresh (window-buffer erc-win)))))))

(defun tao/erc-start-member-timer ()
  "Start updating the member list whenever Emacs is idle for 30 seconds."
  (interactive)
  (when tao/erc-member-timer
    (cancel-timer tao/erc-member-timer))
  (setq tao/erc-member-timer
    (run-with-idle-timer 5 t #'tao/erc-timer-callback)))

(defun tao/erc-update-members-sidebar (&rest _)
  (when (derived-mode-p 'erc-mode)
    (let ((target-buf (current-buffer)))
      (run-at-time 0 nil #'tao/erc-members-refresh target-buf))))

;; =========================================================================
;; 4. HOOK REGISTRATIONS
;; =========================================================================

;; IRC Network Events Lifecycle Updates
(add-hook 'erc-mode-hook #'tao/erc-update-members-sidebar)
(add-hook 'erc-mode-hook #'tao/erc-start-member-timer)
(add-hook 'erc-join-hook #'tao/erc-update-members-sidebar)
(add-hook 'erc-part-hook #'tao/erc-update-members-sidebar)
(add-hook 'erc-quit-hook #'tao/erc-update-members-sidebar)
(add-hook 'erc-kick-hook #'tao/erc-update-members-sidebar)

;; Global Window Tracking Hooks
;; We check (windowp win) explicitly to prevent frame argument crashes.
(add-hook 'window-buffer-change-functions
  (lambda (win)
    (when (windowp win)
      (let ((buf (window-buffer win)))
        (when (and (buffer-live-p buf)
                (with-current-buffer buf (derived-mode-p 'erc-mode)))
          (with-selected-window win
            (tao/erc-members-refresh buf)))))))

;; Post-command wrapper to effortlessly track internal buffer flips safely
(add-hook 'post-command-hook
  (lambda ()
    (when (derived-mode-p 'erc-mode)
      (tao/erc-members-refresh (current-buffer)))))

;; 5. AUTO-SCROLL TO BOTTOM ON USER MESSAGES
;; =========================================================================

(defvar tao/erc--autoscroll-timer nil
  "Holds the debounced autoscroll timer.")

(defvar tao/erc--pending-privmsg nil
  "Non-nil between a PRIVMSG being received and its line being inserted.")

(defun tao/erc--mark-privmsg (_proc _parsed)
  "Flag the upcoming insertion as a PRIVMSG.
Must return nil so ERC's normal PRIVMSG handling is not interrupted."
  (setq tao/erc--pending-privmsg t)
  nil)

(add-hook 'erc-server-PRIVMSG-functions #'tao/erc--mark-privmsg)

(defun tao/erc--near-bottom-p ()
  "Return non-nil if the end of the buffer is visible in the selected window.
Uses the window's rendered display, so this is correct regardless of
soft-wrap or window width."
  (pos-visible-in-window-p (point-max) (selected-window)))

(defun tao/erc--autoscroll-bottom ()
  "Scroll ERC channel buffer to bottom if near the current end.
Debounce is handled by cancelling any existing timer before scheduling a new one."
  (when (and (derived-mode-p 'erc-mode)
          (tao/erc--near-bottom-p))
    (when tao/erc--autoscroll-timer
      (cancel-timer tao/erc--autoscroll-timer))
    (setq tao/erc--autoscroll-timer
      (run-with-idle-timer 1 nil
        (lambda ()
          (when (and (derived-mode-p 'erc-mode)
                  (tao/erc--near-bottom-p))
            (with-current-buffer (current-buffer)
              (goto-char (point-max))))
          (setq tao/erc--autoscroll-timer nil))))))

(defun tao/erc--maybe-autoscroll-after-msg ()
  "Auto-scroll ERC buffer only right after a PRIVMSG line was inserted, not system events."
  (when tao/erc--pending-privmsg
    (setq tao/erc--pending-privmsg nil)
    (tao/erc--autoscroll-bottom)))

(add-hook 'erc-insert-post-hook #'tao/erc--maybe-autoscroll-after-msg)

;; 6. UTILITY & CONNECTION CONFIGURATION
;; =========================================================================
(defun tao/erc-disconnect-all ()
  "Disconnect from all IRC networks, close channels, and kill the member sidebar."
  (interactive)
  ;; 1. Cancel the background idle timer so it stops running loops
  (when tao/erc-member-timer
    (cancel-timer tao/erc-member-timer)
    (setq tao/erc-member-timer nil))

  ;; 2. Disconnect cleanly from all servers (handles all channels automatically)
  (when (fboundp 'erc-quit-server)
    (ignore-errors
      (erc-quit-server "Goodbye!"))) ; You can customize your quit message string here

  ;; 3. Kill all ERC network and channel buffers
  (dolist (b (erc-buffer-list))
    (when (buffer-live-p b)
      (kill-buffer b)))

  ;; 4. Explicitly kill your custom member list buffer to free up window space
  (let ((sidebar-buf (get-buffer tao/erc-members-buffer)))
    (when sidebar-buf
      (kill-buffer sidebar-buf))))

;;; Reconnect discipline
;; ERC clears its reconnect bookkeeping as soon as a session registers
;; (`erc-connection-established' resets `erc-server-reconnect-count'), so a
;; connection that registers and then immediately drops is re-established
;; forever.  Every one of those reconnects re-runs `erc-after-connect', which
;; re-JOINs every autojoin channel: to everyone else the client looks like a
;; flapping bouncer.  Every automatic reconnect goes through
;; `erc-schedule-reconnect', so the backoff and the flap limit live there.

(defcustom tao/erc-reconnect-base-delay 15
  "Seconds to wait before the first automatic reconnect.
Libera throttles clients that reconnect within a few seconds, so this must
not be ERC's one-second default."
  :type 'number)

(defcustom tao/erc-reconnect-max-delay 600
  "Upper bound in seconds for the reconnect backoff."
  :type 'number)

(defcustom tao/erc-reconnect-max-flaps 5
  "Give up on auto-reconnecting after this many consecutive failures.
One failure is counted per dropped session and per failed connectivity
probe.  A session that stays up for `tao/erc-reconnect-stable-seconds'
starts counting from zero again."
  :type 'integer)

(defcustom tao/erc-reconnect-stable-seconds 300
  "Seconds a session must survive for its loss to count as a new disconnect."
  :type 'number)

(defvar-local tao/erc--reconnect-failures 0
  "Consecutive automatic reconnect failures for this server buffer.")

(defvar-local tao/erc--reconnect-delay nil
  "Delay in seconds for the next automatic reconnect.
Doubles after each failure, capped at `tao/erc-reconnect-max-delay'.")

(defvar-local tao/erc--registered-at nil
  "Time of this session's last successful registration, as from `float-time'.")

(defvar-local tao/erc--gave-up nil
  "Non-nil once the flap limit stopped automatic reconnection.
The next successful registration clears the counters and rearms it.")

(defun tao/erc--note-connection (_server _nick)
  "Record a successful registration and clear a spent flap limit.
Runs in the server buffer, via `erc-after-connect'."
  (setq tao/erc--registered-at (float-time))
  (when tao/erc--gave-up
    (setq tao/erc--gave-up nil
      tao/erc--reconnect-failures 0
      tao/erc--reconnect-delay nil)))

(defun tao/erc--reconnect-backoff (orig buffer &optional incr)
  "Reconnect BUFFER with exponential backoff, giving up after too many flaps.
ORIG is `erc-schedule-reconnect', the only place ERC arms a reconnect timer.
INCR is ERC's own attempt increment; 0 means a failed connectivity probe
rather than a dropped session."
  (with-current-buffer buffer
    (when (and tao/erc--registered-at
            (>= (- (float-time) tao/erc--registered-at)
              tao/erc-reconnect-stable-seconds))
      ;; The session was healthy, so its loss starts a fresh run.
      (setq tao/erc--reconnect-failures 0
        tao/erc--reconnect-delay nil))
    (setq tao/erc--registered-at nil)
    (setq tao/erc--reconnect-delay
      (min tao/erc-reconnect-max-delay
        (if tao/erc--reconnect-delay
          (* 2 tao/erc--reconnect-delay)
          tao/erc-reconnect-base-delay)))
    (setq tao/erc--reconnect-failures (1+ tao/erc--reconnect-failures))
    (if (> tao/erc--reconnect-failures tao/erc-reconnect-max-flaps)
      (progn
        (setq tao/erc--gave-up t)
        (erc-display-message nil 'error (current-buffer)
          (format "Gave up reconnecting (%d failures in a row); M-x +irc/connect when the network is back."
            tao/erc--reconnect-failures)))
      (let ((erc-server-reconnect-timeout tao/erc--reconnect-delay))
        (funcall orig buffer incr)))))

(after! erc
  (add-hook 'erc-after-connect #'tao/erc--note-connection)
  (advice-add 'erc-schedule-reconnect :around #'tao/erc--reconnect-backoff))

;;; Unread channel notifications
;; `erc-insert-post-hook' runs with the buffer narrowed to the message just
;; inserted, so the parsed response identifies it.  A PRIVMSG that lands in a
;; buffer the focused window does not show stays unread until that buffer is
;; focused; a channel sitting in a background window still notifies.
(defvar tao/erc--unread-buffers nil
  "ERC channel and query buffers with messages received while unfocused.
Ordered most recently unread first.")

(defvar tao/erc--announced-buffers nil
  "Buffers in `tao/erc--unread-buffers' whose notice was already shown.")

(defvar tao/erc--pending-notice nil
  "Names of unread channels whose notice waits for an idle minibuffer.")

(defun tao/erc--unread-names ()
  "Return names of the live buffers in `tao/erc--unread-buffers'."
  (delq nil (mapcar (lambda (buf)
                      (and (buffer-live-p buf) (buffer-name buf)))
                    tao/erc--unread-buffers)))

(defun tao/erc--flush-unread-notice ()
  "Show the pending unread-channel notice in the echo area.
Channels read in the meantime are dropped from it."
  (let ((names (seq-intersection tao/erc--pending-notice
                                 (tao/erc--unread-names))))
    (setq tao/erc--pending-notice nil)
    (when names
      (message "New IRC messages in %s" (string-join names ", ")))))

(defun tao/erc--focused-buffer ()
  "Return the buffer the user is looking at.
While the minibuffer is active, report the window it was invoked from, so
typing a command does not make a visible channel look unread."
  (window-buffer (or (and (active-minibuffer-window)
                          (minibuffer-selected-window))
                     (selected-window))))

(defun tao/erc--buffer-focused-p (buffer)
  "Return non-nil when BUFFER is the one in the focused window."
  (eq buffer (tao/erc--focused-buffer)))

(defun tao/erc--prune-unread ()
  "Forget unread buffers that were killed or moved to the focused window.
Only the focused buffer counts as read: a channel in a background window
still gets notified."
  (setq tao/erc--unread-buffers
        (seq-filter (lambda (buf)
                      (and (buffer-live-p buf)
                           (not (tao/erc--buffer-focused-p buf))))
                    tao/erc--unread-buffers))
  (setq tao/erc--announced-buffers
        (seq-filter (lambda (buf) (memq buf tao/erc--unread-buffers))
                    tao/erc--announced-buffers)))

(defun tao/erc--note-unread ()
  "Mark the current buffer unread and announce it if it was not already.
The notice lists every unread channel, so a burst of messages never hides
an earlier channel behind the echo area; it waits while the minibuffer is
active so an in-progress prompt is not clobbered."
  (let ((buf (current-buffer)))
    (cl-pushnew buf tao/erc--unread-buffers)
    (tao/erc--prune-unread)
    (when (and (memq buf tao/erc--unread-buffers)
               (not (memq buf tao/erc--announced-buffers)))
      (push buf tao/erc--announced-buffers)
      (setq tao/erc--pending-notice (tao/erc--unread-names))
      (unless (active-minibuffer-window)
        (tao/erc--flush-unread-notice)))))

(defun tao/erc--track-unread-message ()
  "Record the message just inserted as unread unless its buffer is focused.
Only PRIVMSGs count as messages; joins, parts, and notices do not."
  (when (and (derived-mode-p 'erc-mode)
             (erc-default-target)
             (not (tao/erc--buffer-focused-p (current-buffer))))
    (when-let* ((pos (erc-find-parsed-property))
                (parsed (erc-get-parsed-vector pos))
                ((equal (erc-response.command parsed) "PRIVMSG")))
      (tao/erc--note-unread))))

(defun tao/erc--prune-unread-on-command ()
  "Recheck unread buffers after each command; those now shown are read."
  (when tao/erc--unread-buffers
    (tao/erc--prune-unread)))

(after! erc
  (require 'seq)
  (require 'subr-x)
  (add-hook 'erc-insert-post-hook #'tao/erc--track-unread-message)
  (add-hook 'post-command-hook #'tao/erc--prune-unread-on-command)
  (add-hook 'minibuffer-exit-hook #'tao/erc--flush-unread-notice))

;;; Session snapshots
(defcustom tao/erc-session-directory "~/Notes/irc-sessions/"
  "Directory where `tao/erc-save-session' writes IRC logs.
Each channel or query gets a file named after its buffer."
  :type 'directory)

(defun tao/erc--chat-message-p (position)
  "Return non-nil if the message starting at POSITION is user chat text.
Joins, parts, quits, notices, mode changes, and server output are
excluded.  Own messages lack `erc--cmd', so they are recognized by the
\"input\" catalog keys ERC stores in `erc--msg'."
  (and (get-text-property position 'erc--spkr)
       (or (eq (get-text-property position 'erc--cmd) 'PRIVMSG)
           (memq (get-text-property position 'erc--msg)
                 '(input-chan-privmsg input-query-privmsg
                   ctcp-action-input)))))

(defun tao/erc--message-body (start end)
  "Return the text of the message at START..END without its timestamp.
Continuation lines are indented so the result fits one Org list item."
  (let ((p start))
    (while (and (< p end)
                (eq (get-text-property p 'field) 'erc-timestamp))
      (setq p (next-single-property-change p 'field nil end)))
    (replace-regexp-in-string
     "\n" "\n  "
     (string-trim (buffer-substring-no-properties p end)))))

(defun tao/erc--session-text ()
  "Return the current buffer's user messages as an Org document.
Only chat text is kept: joins, parts, quits, notices, mode changes, and
server messages are dropped.  Each calendar day becomes a heading and
each message a list item, dated from ERC's `erc--ts' text property.
Everything after `erc-insert-marker' is the prompt and input line, so it
is left out."
  (save-restriction
    (widen)
    (let* ((marker (and (boundp 'erc-insert-marker) erc-insert-marker))
           (end (if (and (markerp marker) (marker-position marker))
                    (marker-position marker)
                  (point-max)))
           (parts (list (format "#+title: %s\n\n" (buffer-name))))
           (day nil)
           (starts nil))
      (save-excursion
        (goto-char (point-min))
        (while (< (point) end)
          (when (get-text-property (point) 'erc--msg)
            (push (point) starts))
          (forward-line 1))
        (setq starts (append (nreverse starts) (list end)))
        (while (cdr starts)
          (let* ((start (car starts))
                 (stop (cadr starts))
                 (ts (get-text-property start 'erc--ts))
                 (msg-day (and ts
                               (ignore-errors
                                 (format-time-string "%F" ts))))
                 (msg-time (and ts
                                (ignore-errors
                                  (format-time-string "%H:%M" ts)))))
            (when (tao/erc--chat-message-p start)
              (when (and msg-day (not (equal msg-day day)))
                (setq day msg-day)
                ;; Leave a blank line between days, but not after the title.
                (when (> (length parts) 1)
                  (push "\n" parts))
                (push (format "* %s\n" day) parts))
              (let ((body (tao/erc--message-body start stop)))
                (unless (string-empty-p body)
                  (push (if msg-time
                            (format "- %s %s\n" msg-time body)
                          (format "- %s\n" body))
                        parts))))
            (setq starts (cdr starts)))))
      (apply #'concat (nreverse parts)))))

(defun tao/erc--archive-session-file (file)
  "Rename FILE beside itself with its modification time and return the name.
The stamp is \"YYYY-MM-DDTHH-MM-SS\" taken from FILE, plus a numeric
suffix if that name is already taken."
  (let* ((time (or (file-attribute-modification-time (file-attributes file))
                   (current-time)))
         (stamp (format-time-string "%Y-%m-%dT%H-%M-%S" time))
         (dir (file-name-directory file))
         (base (file-name-sans-extension (file-name-nondirectory file)))
         (ext (file-name-extension file t))
         (candidate (expand-file-name (concat base "-" stamp ext) dir))
         (n 1))
    (while (file-exists-p candidate)
      (setq candidate (expand-file-name
                       (format "%s-%s-%d%s" base stamp n ext) dir)
            n (1+ n)))
    (rename-file file candidate)
    candidate))

(defun tao/erc-save-session (&optional buffer)
  "Write BUFFER's IRC session to `tao/erc-session-directory' as Org text.
BUFFER defaults to the current buffer and must be an ERC channel or query
buffer.  Only user chat text is saved, grouped under one heading per
calendar day.  The file is named after the buffer with leading hash marks
removed and an .org suffix, e.g. \"emacs.org\" for \"#emacs\".

If the session text has changed since the file was written, the existing
file is first renamed to the same name with its modification timestamp
appended, e.g. \"emacs-2026-11-05T12-34-56.org\"; unchanged text leaves
the file alone.  Returns the main file name."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (unless (and (derived-mode-p 'erc-mode)
                   (erc-default-target))
        (user-error "Not an ERC channel or query buffer: %s"
                    (buffer-name buffer))))
    (with-current-buffer buffer
      (let* ((dir (expand-file-name tao/erc-session-directory))
             (name (concat (replace-regexp-in-string
                            "\\`#+" "" (buffer-name buffer))
                           ".org"))
             (file (expand-file-name name dir))
             (text (tao/erc--session-text))
             (unchanged (and (file-exists-p file)
                             (with-temp-buffer
                               (insert-file-contents file)
                               (string= text (buffer-string))))))
        (make-directory dir t)
        (cond
         (unchanged
          (message "IRC session unchanged; kept %s" file))
         (t
          (let ((archived (and (file-exists-p file)
                               (tao/erc--archive-session-file file))))
            (write-region text nil file nil 'quiet)
            (if archived
                (message "Archived previous session as %s; saved %s"
                         archived file)
              (message "Saved IRC session to %s" file)))))
        file))))
