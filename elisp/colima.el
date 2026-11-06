;;; colima.el --- Drop-in replacement of docker desktop -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26.1"))

;;; Commentary:
;;
;; Colima is a wrapper package for Colima (container runtimes on Linux and macOS).
;; This package assumes that the external command "colima" has been installed
;; somehow such as through Homebrew.
;;
;; See https://github.com/abiosoft/colima for the project.
;;
;; Interactive commands:
;;
;;   `colima'                 Run any colima subcommand (prompts for both)
;;   `colima-start'           Start colima (arguments pre-filled from
;;                            `colima-default-start-arguments')
;;   `colima-stop'            Stop colima (C-u for --force)
;;   `colima-restart'         Restart colima
;;   `colima-status'          Show status (C-u for --extended)
;;   `colima-list'            List colima instances
;;   `colima-version'         Show colima version
;;   `colima-update'          Update the container runtime
;;   `colima-ssh-config'      Show the SSH config of the VM
;;   `colima-delete'          Delete the VM (optionally its container data)
;;   `colima-prune'           Prune colima's cached downloaded assets
;;   `colima-kubernetes-start', `colima-kubernetes-stop',
;;   `colima-kubernetes-reset'
;;   `colima-docker-prune-images'   Remove all unused docker images
;;   `colima-docker-prune-volumes'  Remove all unused docker volumes
;;   `colima-docker-prune-all'      Remove stopped containers, networks,
;;                                  unused images and volumes
;;
;; All output goes to the "*colima output*" buffer.
;;
;;; Code:

(require 'subr-x)
(require 'pcase)

(defgroup colima nil
  "Interface to Colima, container runtimes on Linux and macOS."
  :group 'tools)

(defcustom colima-command "colima"
  "The command or full path of the colima executable.
Defaults to \"colima\", assuming it is on the PATH."
  :group 'colima
  :type 'string)

(defcustom colima-docker-command "docker"
  "The command or full path of the docker executable.
Used by the `colima-docker-prune-*' commands."
  :group 'colima
  :type 'string)

(defcustom colima-profile nil
  "Colima profile name to pass via --profile, or nil for the default profile.
Only applied to wrapper commands whose colima subcommand accepts a profile."
  :group 'colima
  :type '(choice (const :tag "Default profile" nil)
           (string :tag "Profile name")))

(defcustom colima-default-start-arguments ""
  "Default arguments to pre-fill when prompted by `colima' for the start command.
Set to a non-empty string to pre-populate the arguments prompt,
for example \"--cpu 4 --memory 8\"."
  :group 'colima
  :type 'string)

(defvar colima--filter-carry ""
  "Carry buffer for incomplete ANSI escape sequences across filter calls.")

(defvar colima--collapse-lines nil
  "Current list of display lines being assembled by the process filter.")

(defvar colima--collapse-index 0
  "Current line index into `colima--collapse-lines' for the process filter.")

(defun colima-strip-control-sequences (string carry)
  "Strip ANSI/VT100 control sequences from STRING using CARRY for partials.
CARRY holds any incomplete escape sequence left over from the previous
call.  Returns a cons cell (RESULT . NEW-CARRY) where RESULT is the
cleaned string and NEW-CARRY is any trailing incomplete escape sequence
to pass to the next call."
  (let ((str (concat carry string))
         (i 0)
         (len (length (concat carry string)))
         (result ""))
    (while (< i len)
      (cond
        ((and (= (aref str i) 27) (= i (1- len)))
          (setq result (concat result (substring str 0 i))
            carry (substring str i)
            i len))
        ((and (= (aref str i) 27) (< (1+ i) len) (= (aref str (1+ i)) ?\[))
          (let ((j (+ i 2)))
            (while (and (< j len)
                     (memq (aref str j)
                       '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?\; ??)))
              (setq j (1+ j)))
            (if (>= j len)
              (setq result (concat result (substring str 0 i))
                carry (substring str i)
                i len)
              (setq i (1+ j)))))
        (t
          (setq result (concat result (string (aref str i)))
            i (1+ i)))))
    (cons result (or carry ""))))

(defun colima--dedupe-consecutive-lines (lines)
  "Remove consecutive duplicate entries from LINES.
Returns a new list with runs of identical lines collapsed to a single entry."
  (let ((out nil)
         (prev nil))
    (dolist (l lines (nreverse out))
      (unless (equal l prev)
        (push l out)
        (setq prev l)))))

(defun colima-strip-and-collapse (string carry lines index)
  "Parse STRING into a virtual terminal line buffer, handling ANSI sequences.
CARRY is any incomplete escape sequence from a previous call.
LINES is the current list of display lines (strings).
INDEX is the current cursor line position within LINES.

Returns a list (NEW-LINES NEW-INDEX NEW-CARRY) where NEW-LINES is the updated
line buffer, NEW-INDEX is the updated cursor position, and NEW-CARRY holds any
trailing incomplete escape sequence."
  (let ((str (concat carry string))
         (i 0)
         (len (length (concat carry string)))
         (lines (copy-sequence (or lines (list ""))))
         (index (or index 0))
         (new-carry ""))
    (while (< i len)
      (cond
        ((and (= (aref str i) 27) (= i (1- len)))
          (setq new-carry (substring str i)
            i len))
        ((and (= (aref str i) 27) (< (1+ i) len) (= (aref str (1+ i)) ?\[))
          (let ((j (+ i 2)))
            (while (and (< j len)
                     (memq (aref str j)
                       '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?\; ??)))
              (setq j (1+ j)))
            (if (>= j len)
              (setq new-carry (substring str i)
                i len)
              (let ((params (substring str (+ i 2) j))
                     (final (aref str j))
                     (current-line (when (< index (length lines))
                                     (nth index lines))))
                (cond
                  ((and (= final ?A)
                     (string-match-p "^\\(1;\\)?1?$\\|^$" params)
                     current-line
                     (string-prefix-p "> " current-line))
                    (setq index (max 0 (1- index))))
                  ((and (= final ?K)
                     (< index (length lines))
                     (string-prefix-p "> " (nth index lines)))
                    (setf (nth index lines) "")))
                (setq i (1+ j))))))
        ((= (aref str i) ?\r)
          (setf (nth index lines) "")
          (setq i (1+ i)))
        ((= (aref str i) ?\n)
          (setq index (1+ index))
          (while (>= index (length lines))
            (setq lines (append lines (list ""))))
          (setq i (1+ i)))
        (t
          (while (>= index (length lines))
            (setq lines (append lines (list ""))))
          (setf (nth index lines)
            (concat (nth index lines)
              (string (aref str i))))
          (setq i (1+ i)))))
    (list lines index new-carry)))

(defun colima--dedupe-redraw (lines)
  "Remove a repeated block of lines at the start of LINES caused by terminal redraws.
Detects the smallest prefix of length K such that the next K lines are identical,
removes the duplicate block, and returns the deduplicated line list."
  (let ((n (length lines))
         (k 1)
         done)
    (while (and (<= (* 2 k) n) (not done))
      (let ((prefix-tail lines)
             (candidate-tail (nthcdr k lines))
             (eq t)
             (i 0))
        (while (and (< i k) eq prefix-tail candidate-tail)
          (unless (equal (car prefix-tail) (car candidate-tail))
            (setq eq nil))
          (setq prefix-tail (cdr prefix-tail)
            candidate-tail (cdr candidate-tail)
            i (1+ i)))
        (when (and eq (= i k))
          (let ((head nil))
            (dotimes (_ k)
              (push (car lines) head)
              (setq lines (cdr lines)))
            (setq lines (append (nreverse head) (nthcdr k lines))
              done t))))
      (setq k (1+ k)))
    lines))

(defun colima--dedupe-global (lines)
  "Remove duplicate entries from LINES, keeping only the last occurrence of each.
Returns a new list in the original order with earlier duplicates removed."
  (let ((seen (make-hash-table :test #'equal))
         (out nil))
    (dolist (l (reverse lines))
      (unless (gethash l seen)
        (puthash l t seen)
        (push l out)))
    out))

(defun colima-process-filter (proc string)
  "Process filter for colima subprocesses.
Receives output STRING from PROC, strips ANSI escape sequences, collapses
terminal cursor-movement redraws, and rewrites the process buffer with the
cleaned, deduplicated lines."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((carry colima--filter-carry)
             (lines colima--collapse-lines)
             (index colima--collapse-index))
        (pcase-let* ((`(,new-lines ,new-index ,new-carry)
                       (colima-strip-and-collapse string carry lines index))
                      (deduped (colima--dedupe-consecutive-lines
                                 (colima--dedupe-global
                                   (colima--dedupe-redraw new-lines)))))
          (setq colima--filter-carry new-carry
            colima--collapse-lines deduped
            colima--collapse-index
            (if (zerop (length deduped)) 0
              (min new-index (1- (length deduped)))))
          (let ((inhibit-modification-hooks t))
            (save-excursion
              (goto-char (process-mark proc))
              (delete-region (point) (point-max))
              (insert (string-join deduped "\n"))
              (set-marker (process-mark proc) (point)))))))))

(defun colima-process-sentinel (proc event)
  "Process sentinel for colima subprocesses.
Appends a line describing EVENT to the buffer of PROC once it has finished."
  (when (and (memq (process-status proc) '(exit signal))
          (buffer-live-p (process-buffer proc)))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
             (inhibit-modification-hooks t))
        (save-excursion
          (goto-char (point-max))
          (insert (format "\n\n[Process %s: %s]\n"
                    (process-name proc)
                    (string-trim event))))))))

;;;; Running commands

(defconst colima--output-buffer-name "*colima output*"
  "Name of the buffer receiving the output of colima commands.")

(defun colima--run-command-line (command-line)
  "Run the shell COMMAND-LINE, showing output in the colima output buffer.
Signals a `user-error' if a previous command is still running."
  (let* ((buffer (get-buffer-create colima--output-buffer-name))
          (old (get-buffer-process buffer)))
    (when (and old (process-live-p old))
      (user-error "A colima command is still running; wait for it to finish"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq colima--filter-carry ""
        colima--collapse-lines (list "")
        colima--collapse-index 0)
      (dolist (v '(colima--filter-carry colima--collapse-lines
                    colima--collapse-index))
        (make-local-variable v)))
    (let ((proc (start-process-shell-command "colima" buffer command-line)))
      (set-process-filter proc #'colima-process-filter)
      (set-process-sentinel proc #'colima-process-sentinel))
    (display-buffer buffer)))

(defun colima--build-command-line (executable command arguments &optional profile)
  "Build a shell command line from EXECUTABLE, COMMAND and ARGUMENTS.
When PROFILE is non-nil and `colima-profile' is set, add a --profile flag.
ARGUMENTS may be nil or an empty string."
  (string-join
    (delq nil
      (list executable
        command
        (when (and profile colima-profile
                (not (string-empty-p colima-profile)))
          (concat "--profile " (shell-quote-argument colima-profile)))
        (unless (or (null arguments) (string-empty-p arguments))
          arguments)))
    " "))

(defun colima--run (command &optional arguments profile)
  "Run colima COMMAND with ARGUMENTS in the colima output buffer.
If PROFILE is non-nil, the --profile flag is added when `colima-profile' is set."
  (colima--run-command-line
    (colima--build-command-line colima-command command arguments profile)))

(defun colima--run-docker (command &optional arguments)
  "Run docker COMMAND with ARGUMENTS in the colima output buffer."
  (colima--run-command-line
    (colima--build-command-line colima-docker-command command arguments)))

;;;; Generic entry point

;;;###autoload
(defun colima (command arguments)
  "Run a colima COMMAND with optional ARGUMENTS in a dedicated buffer.
COMMAND is a colima subcommand such as \"start\", \"stop\", or \"status\".
ARGUMENTS is a string of additional flags/options, or empty string for
none.  When COMMAND is \"start\", the arguments prompt is pre-filled
from `colima-default-start-arguments'.
Output is displayed in the \"*colima output*\" buffer with ANSI sequences
stripped and terminal redraws collapsed."
  (interactive
    (let ((cmd (read-string "Enter colima command: ")))
      (list cmd
        (read-string "Enter optional arguments: "
          (when (equal cmd "start")
            colima-default-start-arguments)))))
  (colima--run command arguments))

;;;; Lifecycle commands

;;;###autoload
(defun colima-start (arguments)
  "Start colima with ARGUMENTS.
The prompt is pre-filled from `colima-default-start-arguments'."
  (interactive
    (list (read-string "Enter optional arguments: "
            colima-default-start-arguments)))
  (colima--run "start" arguments t))

;;;###autoload
(defun colima-stop (&optional force)
  "Stop colima.  With prefix argument FORCE, pass --force."
  (interactive "P")
  (colima--run "stop" (when force "--force") t))

;;;###autoload
(defun colima-restart ()
  "Restart colima."
  (interactive)
  (colima--run "restart" nil t))

;;;###autoload
(defun colima-status (&optional extended)
  "Show colima status.  With prefix argument EXTENDED, pass --extended."
  (interactive "P")
  (colima--run "status" (when extended "--extended") t))

;;;###autoload
(defun colima-list ()
  "List colima instances."
  (interactive)
  (colima--run "list"))

;;;###autoload
(defun colima-version ()
  "Show the colima version."
  (interactive)
  (colima--run "version"))

;;;###autoload
(defun colima-update ()
  "Update the container runtime of the colima VM."
  (interactive)
  (colima--run "update" nil t))

;;;###autoload
(defun colima-ssh-config ()
  "Show the SSH configuration of the colima VM."
  (interactive)
  (colima--run "ssh-config" nil t))

;;;; Destructive commands

;;;###autoload
(defun colima-delete ()
  "Delete the colima VM after confirmation.
You are additionally asked whether to delete persistent container data
\(--data), which wipes all images, containers and volumes stored in it."
  (interactive)
  (when (yes-or-no-p
          (format "Delete the colima VM%s? "
            (if (and colima-profile (not (string-empty-p colima-profile)))
              (format " (profile %s)" colima-profile)
              "")))
    (colima--run "delete"
      (string-join
        (delq nil
          (list "--force"
            (when (y-or-n-p
                    "Also delete persistent container data (images, volumes)? ")
              "--data")))
        " ")
      t)))

;;;###autoload
(defun colima-prune ()
  "Prune colima's cached downloaded assets after confirmation.
You are asked whether to prune all assets (--all) or only outdated ones."
  (interactive)
  (when (yes-or-no-p "Prune colima's cached downloaded assets? ")
    (colima--run "prune"
      (string-join
        (delq nil
          (list "--force"
            (when (y-or-n-p "Prune all cached assets, not just outdated ones? ")
              "--all")))
        " "))))

;;;; Kubernetes

;;;###autoload
(defun colima-kubernetes-start ()
  "Start the colima Kubernetes cluster."
  (interactive)
  (colima--run "kubernetes start" nil t))

;;;###autoload
(defun colima-kubernetes-stop ()
  "Stop the colima Kubernetes cluster."
  (interactive)
  (colima--run "kubernetes stop" nil t))

;;;###autoload
(defun colima-kubernetes-reset ()
  "Reset the colima Kubernetes cluster after confirmation."
  (interactive)
  (when (yes-or-no-p "Reset the Kubernetes cluster (all cluster state is lost)? ")
    (colima--run "kubernetes reset" nil t)))

;;;; Docker cleanup (requires a running colima VM)

;;;###autoload
(defun colima-docker-prune-images ()
  "Remove all unused docker images (`docker image prune --all --force').
Colima must be running."
  (interactive)
  (when (yes-or-no-p "Remove ALL unused docker images? ")
    (colima--run-docker "image prune" "--all --force")))

;;;###autoload
(defun colima-docker-prune-volumes ()
  "Remove all unused docker volumes (`docker volume prune --all --force').
Colima must be running."
  (interactive)
  (when (yes-or-no-p "Remove ALL unused docker volumes (data will be lost)? ")
    (colima--run-docker "volume prune" "--all --force")))

;;;###autoload
(defun colima-docker-prune-all ()
  "Remove stopped containers, unused networks, images and volumes.
Runs `docker system prune --all --volumes --force'.  Colima must be running."
  (interactive)
  (when (yes-or-no-p
          "Remove stopped containers, unused networks, images AND volumes? ")
    (colima--run-docker "system prune" "--all --volumes --force")))

(provide 'colima)
;;; colima.el ends here
