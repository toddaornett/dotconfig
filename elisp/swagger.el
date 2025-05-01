;;; swagger.el --- Launch Swagger Editor from Docker -*- lexical-binding: t; -*-

;; Author: Todd Ornett
;; Version: 1.0
;; Keywords: tools, docker, swagger
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; This package provides a simple interface to launch the Swagger Editor
;; using a Docker container. It checks if the swaggerapi/swagger-editor
;; image exists, pulls it if necessary, and runs the container on port 80.
;; Use M-x swagger to start the swagger editor.

;;; Code:

(defgroup swagger nil
  "Customization group for Swagger Editor integration."
  :group 'tools
  :prefix "swagger-")

(defcustom swagger-docker-port "80:8080"
  "Port mapping for the Swagger Editor Docker container."
  :type 'string
  :group 'swagger)

(defvar swagger--message-buffer "*Swagger*"
  "Buffer to store Swagger-related messages.")

(defun swagger--log-message (format-string &rest args)
  "Log a message to the *Swagger* buffer instead of *Messages*."
  (let ((buffer (get-buffer-create swagger--message-buffer)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (apply 'format format-string args))
      (insert "\n"))))

(defun swagger--check-docker-image ()
  "Check if the swaggerapi/swagger-editor Docker image exists."
  (let ((output (shell-command-to-string
                 "docker images | grep swaggerapi/swagger-editor 2>/dev/null")))
    (not (string-empty-p output))))

(defun swagger--pull-image ()
  "Pull the swaggerapi/swagger-editor Docker image."
  (swagger--log-message "Pulling swaggerapi/swagger-editor Docker image...")
  (let ((result (shell-command-to-string "docker pull swaggerapi/swagger-editor 2>&1")))
    (if (string-match-p "Error" result)
        (error "Failed to pull Swagger Editor image: %s" result)
      (swagger--log-message "Successfully pulled swaggerapi/swagger-editor image"))))

(defun swagger--run-container ()
  "Run the Swagger Editor Docker container."
  (swagger--log-message "Starting Swagger Editor container...")
  (let ((command (format "docker run -p %s swaggerapi/swagger-editor"
                         swagger-docker-port))
        (buffer (get-buffer-create swagger--message-buffer)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (async-shell-command command buffer)
      (swagger--log-message "Swagger Editor is running on http://localhost:%s"
                            (car (split-string swagger-docker-port ":"))))))

;;;###autoload
(defun swagger ()
  "Launch the Swagger Editor in a Docker container."
  (interactive)
  (unless (executable-find "docker")
    (error "Docker is not installed or not in PATH"))
  (unless (swagger--check-docker-image)
    (swagger--pull-image))
  (swagger--run-container))

(provide 'swagger)
;;; swagger.el ends here
