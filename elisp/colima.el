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
  "The command or full path of the colima command."
  :group 'colima
  :type 'string)

(defvar colima--filter-carry "")
(defvar colima--collapse-lines nil)
(defvar colima--collapse-index 0)

(defun colima-strip-control-sequences (string carry)
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
  (let ((out nil)
        (prev nil))
    (dolist (l lines (nreverse out))
      (unless (equal l prev)
        (push l out)
        (setq prev l)))))

(defun colima-strip-and-collapse (string carry lines index)
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
  (interactive "sEnter colima command: \nsEnter optional arguments: ")
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
      (dolist (v '(colima--filter-carry colima--collapse-lines colima--collapse-index))
        (make-local-variable v)))
    (let ((proc (start-process-shell-command "colima" buffer full-command)))
      (set-process-filter proc #'colima-process-filter))
    (display-buffer buffer)))

(defun colima--dedupe-global (lines)
  "Remove duplicate LINES, keep only the last occurrence of each line."
  (let ((seen (make-hash-table :test #'equal))
        (out nil))
    (dolist (l (reverse lines))
      (unless (gethash l seen)
        (puthash l t seen)
        (push l out)))
    out))

(provide 'colima)
;;; colima.el ends here
