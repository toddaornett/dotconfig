;;; nodoze.el --- Prevent system from sleeping -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Nodoze prevents your system from sleeping for a specified duration using
;; the macOS `caffeinate` utility. Run `M-x nodoze` and enter a duration such
;; as "90m", "1h30m", "5 minutes", "45s", or "1d 2h" (days can be combined
;; with hours). A bare number with no unit (e.g. "1" or "0.0167") is
;; interpreted as hours, matching nodoze's original interface.
;;
;; From `nodoze-afternoon-time' (13:00) through `nodoze-until-time', an empty
;; duration prompt defaults to the remaining time until `nodoze-until-time'.
;;
;; With any prefix argument, `nodoze' starts a kokoni-style schedule instead
;; of prompting: idle-only caffeinate until `nodoze-idle-until-time', then
;; full-awake until `nodoze-until-time'. Starting at or after
;; `nodoze-afternoon-time' (or after `nodoze-idle-until-time') skips the
;; idle-only phase and runs full-awake until `nodoze-until-time'.
;;
;; `M-x nodoze-kill` terminates the caffeinate process currently tracked by
;; nodoze (or reports that none is running).
;;
;; `M-x nodoze-status` reports the start time, duration, and expected end
;; time of the currently tracked caffeinate process.
;;
;; All user-facing messages from `nodoze' and `nodoze-kill' are both shown in
;; the echo area and logged, timestamped, to the `*nodoze*` buffer.
;;
;; The tracked process's PID, start time, and duration are persisted to a
;; per-user file in `temporary-file-directory' so `nodoze-kill' and
;; `nodoze-status' keep working even across Emacs restarts.
;;
;; Requires macOS `caffeinate`.
;;
;;; Code:

(defgroup nodoze nil
  "Customize settings for the running of nodoze to keep the system awake."
  :group 'hardware
  :prefix "nodoze-")

(defcustom nodoze-command-options
  "-d -i -m -s -u -t"
  "The default arguments passed by nodoze to NODOZE_COMMAND_PROGRAM."
  :group 'nodoze
  :type 'string)

(defcustom nodoze-idle-command-options
  "-i -t"
  "Arguments used for the idle-only phase of a prefixed (kokoni-style) run."
  :group 'nodoze
  :type 'string)

(defcustom nodoze-command-program
  "caffeinate"
  "The default program for nodoze to keep system awake."
  :group 'nodoze
  :type 'string)

(defcustom nodoze-idle-until-time
  "12:00"
  "Clock time (HH:MM) until which a prefixed nodoze uses idle-only prevention.
After this time the full-awake phase runs until `nodoze-until-time'."
  :group 'nodoze
  :type 'string)

(defcustom nodoze-afternoon-time
  "13:00"
  "Clock time (HH:MM) at or after which nodoze treats the day as afternoon.
A prefixed run then skips the idle-only phase. An unprefixed run uses the
remaining time until `nodoze-until-time' as the duration-prompt default."
  :group 'nodoze
  :type 'string)

(defcustom nodoze-until-time
  "17:45"
  "Clock time (HH:MM) until which a prefixed nodoze keeps the system fully awake."
  :group 'nodoze
  :type 'string)

(defvar nodoze--state-file
  (expand-file-name (format "nodoze-%s.state" (user-login-name))
    temporary-file-directory)
  "File used to persist info about the caffeinate process nodoze is tracking.
Stores a plist of the form (:pid PID :start START-TIME :seconds SECONDS).")

(defvar nodoze--next-phase-timer nil
  "Timer that starts the full-awake phase after an idle-only phase.")

(defun nodoze--write-state (pid start-time seconds)
  "Persist PID, START-TIME, and SECONDS to `nodoze--state-file'."
  (with-temp-file nodoze--state-file
    (prin1 (list :pid pid :start start-time :seconds seconds)
      (current-buffer))))

(defun nodoze--read-state ()
  "Read and return the persisted state plist, or nil if none/unreadable."
  (when (file-exists-p nodoze--state-file)
    (with-temp-buffer
      (insert-file-contents nodoze--state-file)
      (condition-case nil
        (read (current-buffer))
        (error nil)))))

(defun nodoze--delete-state ()
  "Remove the persisted state file, if it exists."
  (when (file-exists-p nodoze--state-file)
    (delete-file nodoze--state-file)))

(defun nodoze--process-running-p (pid)
  "Return non-nil if PID refers to a currently running process."
  (and pid (process-attributes pid) t))

(defun nodoze--log (buffer fmt &rest args)
  "Insert a timestamped message built from FMT and ARGS into BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "[%s] %s\n"
                (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))
                (apply #'format fmt args))))))

(defun nodoze--log-and-message (buffer fmt &rest args)
  "Build a message from FMT and ARGS, log it (timestamped)
in BUFFER, and also show it via `message'."
  (let ((text (apply #'format fmt args)))
    (nodoze--log buffer "%s" text)
    (message "%s" text)))

(defun nodoze--log-finished (buffer pid)
  "Log in BUFFER that process PID has finished.
Only do so if PID is still the tracked process."
  (let ((state (nodoze--read-state)))
    (when (and state (eq (plist-get state :pid) pid))
      (nodoze--delete-state)
      (nodoze--log-and-message
        buffer "Caffeinate (pid %s) finished - system can now sleep" pid))))

(defun nodoze--cancel-next-phase ()
  "Cancel a pending full-awake phase, if any."
  (when (timerp nodoze--next-phase-timer)
    (cancel-timer nodoze--next-phase-timer))
  (setq nodoze--next-phase-timer nil))

(defun nodoze--kill-tracked ()
  "Terminate the caffeinate process nodoze is currently tracking, if any.
Only ever touches a process that nodoze itself started; never affects other
caffeinate processes on the system. Returns the PID killed, or nil."
  (nodoze--cancel-next-phase)
  (let* ((state (nodoze--read-state))
          (pid (plist-get state :pid)))
    (when (and pid (nodoze--process-running-p pid))
      (signal-process pid 'SIGTERM))
    (nodoze--delete-state)
    pid))

(defconst nodoze--duration-unit-re
  "\\([0-9]*\\.?[0-9]+\\)[ \t]*\\([a-zA-Z]+\\)"
  "Regexp matching one NUMBER+UNIT term within a human duration string.")

(defconst nodoze--clock-time-re
  "\\`\\([0-2]?[0-9]\\):\\([0-5][0-9]\\)\\'"
  "Regexp matching a 24-hour clock time as HH:MM.")

(defun nodoze--duration-unit-seconds (unit)
  "Return the number of seconds in one UNIT, based on its first letter.
UNIT is matched case-insensitively by first letter only, so \"d\"/\"day\"/
\"days\", \"h\"/\"hr\"/\"hrs\"/\"hour\"/\"hours\", \"m\"/\"min\"/\"mins\"/
\"minute\"/\"minutes\", and \"s\"/\"sec\"/\"secs\"/\"second\"/\"seconds\" are
all recognized."
  (pcase (aref (downcase unit) 0)
    (?d 86400.0)
    (?h 3600.0)
    (?m 60.0)
    (?s 1.0)
    (_ (user-error "nodoze: unrecognized duration unit %S" unit))))

(defun nodoze--parse-duration (input)
  "Parse INPUT into a whole number of seconds.
INPUT is a human-friendly duration string.

Units may be written in full, abbreviated, or singular/plural form, and
may be combined in any order and with or without surrounding whitespace,
e.g. \"1h30m\", \"1d 2h\", \"5m 5s\", \"5 minutes\", \"45s\", \"5sec\".

A bare number with no unit at all (e.g. \"1\" or \"0.0167\") is treated as
a number of hours, matching nodoze's original numeric-hours interface."
  (let ((s (string-trim input))
         (seconds 0.0)
         (found-unit nil)
         (start 0))
    (when (zerop (length s))
      (user-error "nodoze: empty duration"))
    (while (string-match nodoze--duration-unit-re s start)
      (let ((num (string-to-number (match-string 1 s)))
             (unit (match-string 2 s)))
        (setq seconds (+ seconds (* num (nodoze--duration-unit-seconds unit))))
        (setq found-unit t)
        (setq start (match-end 0))))
    (unless found-unit
      (if (string-match-p "\\`[0-9]*\\.?[0-9]+\\'" s)
        (setq seconds (* (string-to-number s) 3600.0))
        (user-error "nodoze: could not parse duration %S" input)))
    (round seconds)))

(defun nodoze--format-duration (seconds)
  "Return a compact human-readable string for a duration of SECONDS."
  (let* ((seconds (round seconds))
          (days (/ seconds 86400))
          (rem (% seconds 86400))
          (hours (/ rem 3600))
          (rem (% rem 3600))
          (minutes (/ rem 60))
          (secs (% rem 60))
          (parts (delq nil
                   (list (and (> days 0) (format "%dd" days))
                     (and (> hours 0) (format "%dh" hours))
                     (and (> minutes 0) (format "%dm" minutes))
                     (and (or (> secs 0)
                            (and (= days 0) (= hours 0) (= minutes 0)))
                       (format "%ds" secs))))))
    (string-join parts " ")))

(defun nodoze--parse-clock-time (time-str)
  "Parse TIME-STR (\"HH:MM\") into a list (HOUR MINUTE)."
  (unless (and (stringp time-str)
            (string-match nodoze--clock-time-re time-str))
    (user-error "nodoze: invalid clock time %S (expected HH:MM)" time-str))
  (let ((hour (string-to-number (match-string 1 time-str)))
         (minute (string-to-number (match-string 2 time-str))))
    (unless (and (<= 0 hour 23) (<= 0 minute 59))
      (user-error "nodoze: invalid clock time %S" time-str))
    (list hour minute)))

(defun nodoze--seconds-until (time-str)
  "Seconds from now until TIME-STR today. Negative if TIME-STR has passed."
  (let* ((hm (nodoze--parse-clock-time time-str))
          (target (decode-time)))
    (setf (decoded-time-second target) 0
      (decoded-time-minute target) (nth 1 hm)
      (decoded-time-hour target) (nth 0 hm)
      (decoded-time-dst target) -1)
    (round (float-time (time-subtract (encode-time target) (current-time))))))

(defun nodoze--afternoon-p ()
  "Return non-nil if the current time is at or after `nodoze-afternoon-time'."
  (<= (nodoze--seconds-until nodoze-afternoon-time) 0))

(defun nodoze--default-duration-prompt ()
  "Remaining time until `nodoze-until-time' when it is afternoon, else nil."
  (when (nodoze--afternoon-p)
    (let ((seconds (nodoze--seconds-until nodoze-until-time)))
      (when (> seconds 0)
        (nodoze--format-duration seconds)))))

(defun nodoze--launch (options seconds description)
  "Start `nodoze-command-program' with OPTIONS for SECONDS.
DESCRIPTION is logged with the new PID."
  (let* ((buffer (get-buffer-create "*nodoze*"))
          (start-time (current-time))
          (args (append (split-string options)
                  (list (number-to-string seconds))))
          (proc (apply #'start-process "nodoze-caffeinate" nil
                  nodoze-command-program args))
          (pid (process-id proc)))
    (set-process-query-on-exit-flag proc nil)
    (nodoze--write-state pid start-time seconds)
    (nodoze--log-and-message
      buffer "Starting caffeinate (pid %s) %s (%s)"
      pid description (nodoze--format-duration seconds))
    (run-at-time seconds nil #'nodoze--log-finished buffer pid)
    pid))

(defun nodoze--schedule-plan ()
  "Return the prefixed-run plan as (PHASE SECONDS), or nil if past end of day.
PHASE is `idle' (then a full-awake follow-up) or `full'."
  (let ((until-end (nodoze--seconds-until nodoze-until-time)))
    (cond
      ((<= until-end 0) nil)
      ((or (nodoze--afternoon-p)
         (<= (nodoze--seconds-until nodoze-idle-until-time) 0))
        (list 'full until-end))
      (t (list 'idle (nodoze--seconds-until nodoze-idle-until-time))))))

(defun nodoze--start-full-phase ()
  "Start the full-awake phase of a prefixed schedule, if time remains."
  (setq nodoze--next-phase-timer nil)
  (let ((until-end (nodoze--seconds-until nodoze-until-time))
         (buffer (get-buffer-create "*nodoze*")))
    (if (<= until-end 0)
      (nodoze--log-and-message
        buffer "nodoze: schedule ended (past %s)" nodoze-until-time)
      (nodoze--launch nodoze-command-options until-end
        (format "until %s" nodoze-until-time)))))

(defun nodoze--start-schedule ()
  "Start a kokoni-style schedule through `nodoze-until-time'."
  (let ((plan (nodoze--schedule-plan)))
    (unless plan
      (user-error "nodoze: already past %s" nodoze-until-time))
    (nodoze--kill-tracked)
    (pcase plan
      (`(full ,until-end)
        (nodoze--launch nodoze-command-options until-end
          (format "until %s" nodoze-until-time)))
      (`(idle ,until-idle)
        (nodoze--launch nodoze-idle-command-options until-idle
          (format "idle-only until %s" nodoze-idle-until-time))
        (setq nodoze--next-phase-timer
          (run-at-time until-idle nil #'nodoze--start-full-phase))))))

;;;###autoload
(defun nodoze (duration &optional scheduled)
  "Prevent the system from sleeping for DURATION, logging to *nodoze* buffer.
DURATION is a human-friendly duration string; see `nodoze--parse-duration'
for the accepted formats (e.g. \"1h30m\", \"5 minutes\", \"45s\", \"1d 2h\",
or a bare number of hours).

With any prefix argument, or when SCHEDULED is non-nil, ignore DURATION and
start a kokoni-style schedule: idle-only until `nodoze-idle-until-time',
then full-awake until `nodoze-until-time'. At or after
`nodoze-afternoon-time', skip idle-only and run full-awake until
`nodoze-until-time'."
  (interactive
    (if current-prefix-arg
      (list nil t)
      (let ((default (nodoze--default-duration-prompt)))
        (list (read-string
                (if default
                  (format "Duration to stay active (default %s): " default)
                  "Duration to stay active (e.g. 1h30m, 45s, 2 days, or a bare \
number of hours): ")
                nil nil default)
          nil))))
  (if scheduled
    (nodoze--start-schedule)
    (let ((seconds (nodoze--parse-duration duration)))
      (when (<= seconds 0)
        (user-error "nodoze: duration must be greater than zero"))
      ;; Stop the previous run nodoze itself was tracking, if it's still going,
      ;; so it doesn't keep running orphaned. Never touches other caffeinate
      ;; processes on the system.
      (nodoze--kill-tracked)
      (nodoze--launch nodoze-command-options seconds
        (format "for %s" (nodoze--format-duration seconds))))))

;;;###autoload
(defun nodoze-kill ()
  "Terminate the caffeinate process currently tracked by nodoze.
If no such process is running, report that instead. Only ever affects a
process nodoze itself started. Either way, the result is logged to the
*nodoze* buffer in addition to being shown as a message."
  (interactive)
  (let* ((buffer (get-buffer-create "*nodoze*"))
          (state (nodoze--read-state))
          (prior-pid (plist-get state :pid))
          (was-running (and prior-pid (nodoze--process-running-p prior-pid))))
    (let ((pid (nodoze--kill-tracked)))
      (if was-running
        (nodoze--log-and-message
          buffer "Killed caffeinate (pid %s) via nodoze-kill" pid)
        (nodoze--log-and-message
          buffer "nodoze-kill: no caffeinate process is currently running")))))

;;;###autoload
(defun nodoze-status ()
  "Show the start time, duration, and expected end time of the tracked process.
If nodoze isn't currently tracking a running caffeinate process, report that
instead."
  (interactive)
  (let* ((state (nodoze--read-state))
          (pid (plist-get state :pid))
          (start (plist-get state :start))
          (seconds (plist-get state :seconds)))
    (if (and pid start seconds (nodoze--process-running-p pid))
      (let* ((end-time (time-add start seconds))
              (remaining (max 0 (float-time
                                  (time-subtract end-time (current-time))))))
        (message
          "nodoze: running (pid %s) | started %s | duration %.2f hours \
| ends %s | %.0f min remaining"
          pid
          (format-time-string "%Y-%m-%d %H:%M:%S" start)
          (/ seconds 3600.0)
          (format-time-string "%Y-%m-%d %H:%M:%S" end-time)
          (/ remaining 60.0)))
      (nodoze--delete-state)
      (message "nodoze: no caffeinate process is currently running"))))

(provide 'nodoze)
;;; nodoze.el ends here
