;;; play-sound.el --- play sound files on OSX  -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2013  Leo Liu
;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 1.0
;; Updated: 2025-03-31 to not require cl
;; 
;; Keywords: comm, tools, convenience

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

;; Provide a compatibility layer for play-sound on OSX since
;; play-sound-internal is not implemented.

;;; To install:

;; (unless (and (fboundp 'play-sound-internal)
;;              (subrp (symbol-function 'play-sound-internal)))
;;   (require 'play-sound))

;;; Code:

(defun play-sound-internal (sound)
  "Internal function for `play-sound' to play SOUND."
  (unless (and (listp sound) (eq (car sound) 'sound))
    (signal 'wrong-type-argument (list sound)))

  (let* ((plist (cdr sound))
         (file (plist-get plist :file))
         (data (plist-get plist :data))
         (volume (plist-get plist :volume))
         (device (plist-get plist :device))
         (args (list "afplay")))

    (when (or data device)
      (error "DATA and DEVICE arguments are not supported"))

    (when volume
      (setq args (nconc args (list "-v" (format "%s" volume)))))

    (setq args (nconc args (list (expand-file-name file data-directory))))

    (apply #'start-process "afplay" nil args)))

(provide 'play-sound)
;;; play-sound.el ends here
