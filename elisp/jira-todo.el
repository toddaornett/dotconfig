;;; jira-todo.el --- Generate org-mode TODO a1d Slack message for JIRA tickets -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Todd Ornett
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;;
;; Created: April 22, 2026
;; Modified: April 22, 2026
;; Version: 0.0.1
;; Keywords: jira, org, tools
;; Homepage: https://github-tao/toddaornett/dotconfig
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Fetches a JIRA ticket and generates an org-mode TODO entry and Slack message.
;; Requires the `request` package and the following variables to be set:
;;   jira-base-url, jira-issue-key-prefix, jira-username, jira-token
;;
;;; Code:

(require 'request)
(require 'json)
(require 'subr-x)
(require 'org)

(defgroup jira-todo nil
  "Generate `org-mode' TODOs from JIRA tickets."
  :group 'tools)

(defcustom jira-todo-base-url
  (or (getenv "JIRA_ISSUE_BASE_URL") "https://atlassian.net")
  "Base URL for JIRA instance."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-issue-key-prefix
  (or (getenv "JIRA_ISSUE_KEY_PREFIX") "JIRA")
  "JIRA issue key prefix, e.g. JIRA."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-username
  (or (getenv "JIRA_USER") "")
  "JIRA username (email)."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-token
  (or (getenv "JIRA_TOKEN") "")
  "JIRA API token."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-pr-reviewers
  (or (getenv "PULL_REQUEST_REVIEWERS") "")
  "GitHub PR reviewers for Slack message."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-pr-messaging-provider
  (or (getenv "MESSAGING_PROVIDER") "slack")
  "Determine the format of message format.
`slack' or `teams' (Microsoft Teams)"
  :type 'string
  :group 'jira-todo)

(defun jira-todo--rest-url (issue-number)
  "Return the JIRA REST API URL for ISSUE-NUMBER."
  (format "%s/rest/api/3/issue/%s-%s"
          jira-todo-base-url
          jira-todo-issue-key-prefix
          issue-number))

(defun jira-todo--browse-url (key)
  "Return the JIRA browse URL for KEY."
  (format "%s/browse/%s" jira-todo-base-url key))

(defun jira-todo--auth-header ()
  "Return the Basic Auth header value."
  (concat "Basic "
          (base64-encode-string
           (concat jira-todo-username ":" jira-todo-token)
           t)))

(defun jira-todo--smart-open-line-above ()
  "Insert an empty line above the current line and indent it.
If in an `org-mode' buffer within a TODO, move point to a new line
immediately above the first sibling TODO under the parent heading."
  (if (derived-mode-p 'org-mode)
      (let ((target nil)
            (current-level (save-excursion
                             (org-back-to-heading t)
                             (org-current-level))))
        (save-excursion
          ;; Go up to parent heading
          (org-back-to-heading t)
          (when (org-up-heading-safe)
            (let ((parent-end (save-excursion (org-end-of-subtree t t))))
              (while (and (not target)
                          (re-search-forward org-heading-regexp parent-end t))
                (when (and (= (org-current-level) current-level)
                           (org-entry-is-todo-p))
                  (setq target (line-beginning-position)))))))
        (if target
            (progn
              (goto-char target)
              (open-line 1))
          ;; Fallback: original behavior
          (move-beginning-of-line nil)
          (newline-and-indent)
          (forward-line -1)
          (indent-according-to-mode)))
    ;; Not in org-mode: original behavior
    (move-beginning-of-line nil)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)))

(defun jira-todo--format-slack-message (summary)
  "Format message with SUMMARY for Slack."
  (concat
   (format "Slack:\n")
   (format "--begin--\n")
   (format ":pull_request: PTAL %s\n" jira-todo-pr-reviewers)
   (format "%s\n" summary)
   (format "TBD\n")
   (format "--end--\n")))

(defun jira-todo--format-teams-message (summary)
  "Format message with SUMMARY for Microsoft Teams."
  (concat
   (format "Teams:\n")
   (format "--begin--\n")
   (format "PTAL %s\n" jira-todo-pr-reviewers)
   (format "TBD\n")
   (format "%s\n" summary)
   (format "--end--\n")))

(defun jira-todo--format-output (data)
  "Format `org-mode' TODO and message from parsed JIRA DATA."
  (let* ((key             (format "%s" (alist-get 'key data)))
         (fields          (alist-get 'fields data))
         (summary         (format "%s" (alist-get 'summary fields)))
         (url             (jira-todo--key-to-browse-url key))
         (clean-summary   (replace-regexp-in-string "\\[[A-Z]+\\][ ]*" "" summary))
         (branch-words    (replace-regexp-in-string "[^A-Za-z0-9]+" "-" clean-summary))
         (branch-compact  (replace-regexp-in-string "-+" "-" branch-words))
         (branch-trimmed  (replace-regexp-in-string "-+$" "" branch-compact))
         (branch-summ     (downcase branch-trimmed))
         (branch          (format "%s_%s" key branch-summ)))
    (concat
     (format "*** TODO CR: %s %s\n" key clean-summary)
     (format "JIRA: [[%s][%s]]\n" url key)
     (format "Branch: %s\n" branch)
     (format "Title: %s: %s\n" key clean-summary)
     (format "PR: TBD\n")
     (format "PR Text:\n")
     (format "--begin--\n")
     (format "## JIRA\n")
     (format "[%s](%s)\n" key url)
     (format "## Description\n")
     (format "%s\n" clean-summary)
     (format "--end--\n")
     (pcase jira-todo-pr-messaging-provider
       ("slack" (jira-todo--format-slack-message clean-summary))
       ("teams" (jira-todo--format-teams-message clean-summary))
       (_ ""))
     (format ":LOGBOOK:\n")
     (format ":END:"))))

(defun jira-todo--insert-output (data)
  "Insert formatted `org-mode' TODO for JIRA DATA at point in the current buffer."
  (let ((output (jira-todo--format-output data))
        (buf (current-buffer)))
    (with-current-buffer buf
      (jira-todo--smart-open-line-above)
      (let ((start (point)))
        (insert output)
        (goto-char start)
        (org-back-to-heading t)))))

(defun jira-todo--parse-input (input)
  "Parse INPUT into a JIRA key.

INPUT can be in any of the following forms:
- Full URL:  https://atlassian.net/browse/JIRA-11111
- Full key:  JIRA-11111
- Number:    11111

Returns the JIRA key as a string, e.g. JIRA-11111.
Signals an error if INPUT cannot be parsed."
  (cond
   ;; Full URL: https://atlassian.net/browse/JIRA-11111
   ((string-match "/browse/\\([A-Z]+-[0-9]+\\)" input)
    (match-string 1 input))
   ;; Full key: JIRA-11111
   ((string-match "^\\([A-Z]+-[0-9]+\\)$" input)
    (match-string 1 input))
   ;; Just a number: 11111
   ((string-match "^[0-9]+$" input)
    (format "%s-%s" jira-todo-issue-key-prefix input))
   (t
    (error "Could not parse JIRA issue from input: %s" input))))

(defun jira-todo--key-to-rest-url (key)
  "Return the JIRA REST API URL for KEY (e.g. JIRA-11111)."
  (format "%s/rest/api/3/issue/%s" jira-todo-base-url key))

(defun jira-todo--key-to-browse-url (key)
  "Return the JIRA browse URL for KEY."
  (format "%s/browse/%s" jira-todo-base-url key))

;;;###autoload
(defun jira-todo-fetch (&optional input)
  "Fetch a JIRA ticket and generate an `org-mode' TODO and Slack message.
INPUT can be a full URL, a key like JIRA-11111, or just an issue number.
If INPUT is not provided, prompt interactively."
  (interactive)
  (let* ((input (or input
                    (and (not (called-interactively-p 'any)) nil)
                    (read-string "JIRA issue (URL, key, or number): ")))
         (key (jira-todo--parse-input input))
         (rest-url (jira-todo--key-to-rest-url key)))
    (request rest-url
      :headers `(("Accept"        . "application/json")
                 ("Authorization" . ,(jira-todo--auth-header)))
      :parser #'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (jira-todo--insert-output data)))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Error fetching JIRA ticket: %S" error-thrown))))))

(provide 'jira-todo)
;;; jira-todo.el ends here
