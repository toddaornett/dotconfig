;;; colima.el --- drop-in replacement of docker desktop -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Colima is a wrapper package for Colima (container runtimes on Linux and macOS).
;; This package assumes that the external command "colima" has been installed
;; somehow such as through Homebrew.
;;
;; See https://github.com/abiosoft/colima for the project.
;;
;;; Code:

(defcustom colima-command "colima"
  "The command or full path of the colima executable.
Defaults to \"colima\", assuming it is on the PATH."
  :group 'colima
  :type 'string)

(defcustom colima-default-arguments ""
  "Default arguments to pre-fill when prompted by `colima'.
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

(defun colima (command arguments)
  "Run a colima COMMAND with optional ARGUMENTS in a dedicated buffer.
COMMAND is a colima subcommand such as \"start\", \"stop\", or \"status\".
ARGUMENTS is a string of additional flags/options, or empty string for
none.  The prompt for ARGUMENTS is pre-filled from `colima-default-arguments'.
Output is displayed in the \"*colima output*\" buffer with ANSI sequences
stripped and terminal redraws collapsed."
  (interactive
   (list (read-string "Enter colima command: ")
         (read-string "Enter optional arguments: " colima-default-arguments)))
  (let* ((buffer-name "*colima output*")
         (buffer (get-buffer-create buffer-name))
         (full-command (string-join
                        (if (string-empty-p arguments)
                            (list colima-command command)
                          (list colima-command command arguments))
                        " ")))
    (with-current-buffer buffer
      (setq colima--filter-carry ""
            colima--collapse-lines (list "")
            colima--collapse-index 0)
      (dolist (v '(colima--filter-carry colima--collapse-lines
                   colima--collapse-index))
        (make-local-variable v)))
    (let ((proc (start-process-shell-command "colima" buffer full-command)))
      (set-process-filter proc #'colima-process-filter))
    (display-buffer buffer)))

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

(provide 'colima)
;;; colima.el ends here
