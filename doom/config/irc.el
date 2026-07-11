;;; irc.el --- ERC config -*- lexical-binding: t; -*-

(defcustom irc-channels '("#emacs" "#linux")
  "Channels to autojoin."
  :type '(repeat string))

(setq auth-sources '("~/.authinfo"))

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
               (sidebar (get-buffer-create tao/erc-members-buffer)))

          (with-current-buffer sidebar
            (tao/erc-members-mode)

            ;; Dynamically set the column headers with the fresh count
            (setq tabulated-list-format
                  (vector '("P" 2 t)
                          (list (format "Nick (%d)" member-count) 24 t)))

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

;; =========================================================================
;; 5. UTILITY & CONNECTION CONFIGURATION
;; =========================================================================

(defun tao/erc-quit-and-cleanup ()
  (interactive)
  (when (fboundp 'erc-cmd-GQUIT) (erc-cmd-GQUIT ""))
  (dolist (b (and (fboundp 'erc-buffer-list) (erc-buffer-list)))
    (when (buffer-live-p b) (kill-buffer b))))

(defun +irc/connect ()
  (interactive)
  (require 'erc)
  (erc-tls :server erc-server :port erc-port :nick erc-nick))

(after! erc
  (when (and (boundp 'irc-nickname)
             (stringp irc-nickname)
             (not (string-empty-p irc-nickname)))
    (add-to-list 'erc-modules 'sasl)
    (erc-update-modules)
    (setq erc-prompt-for-password nil
          erc-server "irc.libera.chat"
          erc-port 6697
          erc-ssl t
          erc-autojoin-channels-alist `(("libera.chat" ,@irc-channels))
          erc-sasl-mechanism 'plain
          erc-sasl-user irc-nickname
          erc-nick irc-nickname
          erc-sasl-password :password)))
