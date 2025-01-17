;;; insert-port-number-for-directory-into-buffer.el --- generate port number based on current directory name

;; Copyright (C) 2024
;; Author: Todd Ornett
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; If needed, map to a keybinding of choice for convenience.

;;; Code:
(defun hash-directory-name-to-port (directory-name)
  "Generate a port number from the DIRECTORY-NAME using SHA-256 hashing."
  (let* ((hash (secure-hash 'sha256 directory-name))
         (hash-integer (string-to-number (substring hash 0 8) 16))
         (min-port 8000)
         (max-port 8999)
         (port-number (+ min-port (mod hash-integer (- max-port min-port)))))
    port-number))

(defun insert-port-number-for-directory-into-buffer (&optional directory-name)
  "Insert a generated port number for DIRECTORY-NAME into the current buffer.
If DIRECTORY-NAME is not provided, use the last part of the current
directory path."
  (interactive)
  (let* ((dir (or directory-name default-directory))
         (base-dir-name (file-name-nondirectory (directory-file-name dir)))
         (port-number (hash-directory-name-to-port base-dir-name)))
    (insert (format "%d" port-number))))

(provide 'insert-port-number-for-directory-into-buffer)
;;; insert-port-number-for-directory-into-buffer.el ends here
