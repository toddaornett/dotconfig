;;; teams-toolscel --- Teams Tools u -*- lexical-binding: t -*-

;; Author: Todd Ornett
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, teams
;; Homepage: https://github.com/toddaornett/dotconfig

;;; Commentary:

;;; Code:

;;;###autoload
(defun teams-tools-safelink-p (str)
  "Return non-nil if STR looks like a Microsoft Teams/Outlook safelink."
  (and (stringp str)
       (string-match-p "safelinks" str)
       (string-match-p "[?&]url=" str)))

;;;###autoload
(defun teams-tools-unwrap-url (&optional safelink)
  "Extract the real URL from a Teams/Outlook safelinks-wrapped URL,
decode it, and put it on the clipboard (kill-ring).

If SAFELINK is not provided, use the current clipboard contents if
they look like a safelink; otherwise prompt for the URL."
  (interactive)
  (let* ((clip (ignore-errors (current-kill 0)))
         (input (or safelink
                    (and (teams-tools-safelink-p clip) clip)
                    (read-string "Safelink URL: "))))
    (if (string-match "[?&]url=\\([^&]+\\)" input)
        (let ((real-url (url-unhex-string (match-string 1 input))))
          (kill-new real-url)
          (message "Copied: %s" real-url)
          real-url)
      (error "No `url=' parameter found in input"))))

(provide 'teams-tools)
;;; teams-tools.el ends here
