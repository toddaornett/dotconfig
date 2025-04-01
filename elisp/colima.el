;;; colema.el --- drop-in replacement of docker desktop -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Colima is a wrapper package for Colima (container runtimes on Linux and macOS).
;; This package assumes that the external command "colima" has been installed
;; somehow such as through Homebrew.
;; 
;; See https://github.com/abiosoft/colima for the project.
;;
;;; Code:

(require 's)

(defcustom colima-command "colima"
  "The command or full path of the colima command."
  :group 'colima
  :type 'string)

(defun colima-format-command (command args)
  "Lower level function for formatting the colima COMMAND and optional ARGS."
  (s-join " " (if (string-empty-p args)
                  `(,(s-trim colima-command) ,(s-trim command))
                `(,(s-trim colima-command) ,(s-trim command) ,(s-trim args)))))

(defun colima (command arguments)
  "Run a colima COMMAND with optional ARGUMENTS."
  (interactive "sEnter colima command: \nsEnter optional arguments: ")
  (let* ((buffer-name "*colima output*")
         (full-command (colima-format-command command arguments))
         (buffer (get-buffer-create buffer-name))
         (start-pos (with-current-buffer buffer
                      (point-max))))
    (start-process-shell-command "colima" buffer full-command)
    (with-current-buffer buffer
      (let ((proc (get-buffer-process buffer)))
        (if proc
            (set-process-sentinel
             proc
             (lambda (process event)
               (when (string-match-p "finished" event)
                 (with-current-buffer (process-buffer process)
                   (goto-char start-pos)
                   (recenter 0)))))
          (goto-char start-pos)
          (recenter 0))))
    (display-buffer buffer)))

(provide 'colima)
;;; colima.el ends here
