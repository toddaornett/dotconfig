;;; status.el --- create status messages from org task buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Todd Ornett
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: February 27, 2026
;; Version: 0.0.1
;; Keywords: outlines, org, convenience
;; Homepage: https://github.com/toddaornett/dotconfig
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:
;; Generate Slack-friendly status messages from an Org task buffer.

;;; Code:
(require 'org)
(require 'org-element)
(require 'subr-x)

(defgroup status nil
  "Generate status messages from an Org task file."
  :group 'org
  :prefix "status-")

(defcustom status-org-task-file "~/Notes/tasks.org"
  "Path to the Org file containing dated task logs.

The file is expected to contain headings whose title contains a date
in YYYY-MM-DD format, for example:

  * Tasks
  ** [2026-03-01 Sun]
  *** DONE Emacs: refactor status.el
  *** DOING Dev: weekly report

Tasks must be Org TODO headings located anywhere under the date
heading.  Heading level does not matter."
  :type 'file
  :group 'status)

(defcustom status-category-list '("Emacs" "Dev" "Work" "Shell" "Estate")
  "List of task categories to include in status output."
  :type '(repeat string)
  :group 'status)

(defun status--extract-date (heading)
  "Extract YYYY-MM-DD from HEADING."
  (when (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" heading)
    (match-string 1 heading)))

(defun status--all-date-headings ()
  "Return a list of all date heading strings in the buffer."
  (let (dates)
    (with-current-buffer (find-file-noselect status-org-task-file)
      (org-with-wide-buffer
       (unless (derived-mode-p 'org-mode)
         (org-mode))
       (org-element-cache-reset)
       (org-map-entries
        (lambda ()
          (let ((h (org-get-heading t t t t)))
            (when-let ((d (status--extract-date h)))
              (push d dates)))))))
    (delete-dups dates)))

(defun status--sorted-date-headings ()
  "Return all date headings sorted newest-first."
  (sort (status--all-date-headings) #'string>))

(defun status--most-recent-date ()
  "Return the most recent date heading in the file."
  (car (status--sorted-date-headings)))

(defun status--previous-date (date)
  "Return the date heading immediately before DATE."
  (let* ((dates (status--sorted-date-headings))
         (tail (member date dates)))
    (cadr tail)))

(defun status--goto-date-heading (date)
  "Move point to the Org heading with title containing DATE.
Return non-nil if found."
  (goto-char (point-min))
  (re-search-forward
   (format org-complex-heading-regexp-format
           (format ".*%s.*" (regexp-quote date)))
   nil t))

(defun status--collect-category-tasks (date states &optional mark-ongoing)
  "Collect tasks for DATE whose TODO state is in STATES.
If MARK-ONGOING is non-nil, annotate DOING tasks with \"(ongoing)\",
REVIEW tasks with \"(in review)\", and BLOCKED tasks with the text
before the first colon in the heading (or \"(blocked)\" if absent)."
  (let (results)
    (with-current-buffer (find-file-noselect status-org-task-file)
      (org-with-wide-buffer
       (unless (derived-mode-p 'org-mode)
         (org-mode))
       (org-element-cache-reset)
       (save-excursion
         (when (and date (status--goto-date-heading date))
           (org-narrow-to-subtree)
           (org-map-entries
            (lambda ()
              (let* ((state (org-get-todo-state))
                     (title (org-get-heading t t t t)))
                (when (and state
                           (member state states)
                           (string-match
                            (format "^\\(.*?\\)\\(%s\\):[ \t]+\\(.*\\)$"
                                    (regexp-opt status-category-list))
                            title))
                  (let* ((task (match-string 3 title))
                         (prefix (string-trim (match-string 1 title) nil "[:,\s]+"))
                         (final
                          (cond
                           ((and mark-ongoing (string= state "DOING"))
                            (format "%s (ongoing)" task))
                           ((string= state "REVIEW")
                            (format "%s (in review)" task))
                           ((string= state "BLOCKED")
                            (if (string-empty-p prefix)
                                (format "%s (blocked)" task)
                              (format "%s (awaiting %s)" task prefix)))
                           (t task))))
                    (push final results)))))
            nil 'tree)
           (widen)))))
    (nreverse results)))

(defun status--format-section (title items)
  "Format a status section titled TITLE from ITEMS."
  (if items
      (concat title ":\n"
              (mapconcat (lambda (i) (concat "• " i)) items "\n")
              "\n")
    (concat title ":\n• (none)\n")))

;;;###autoload
(defun status-daily ()
  "Generate a daily status report using the most recent date heading."
  (interactive)
  (let* ((today (status--most-recent-date))
         (yesterday (status--previous-date today))
         (buffer-name (format "Daily Status %s" today))
         (y-items (status--collect-category-tasks
                   yesterday '("DONE" "DOING" "BLOCKED" "REVIEW") t))
         (t-items (status--collect-category-tasks
                   today '("TODO" "DONE" "DOING" "BLOCKED" "REVIEW") nil))
         (output (concat
                  (status--format-section "Yesterday" y-items)
                  "\n"
                  (status--format-section "Today" t-items))))
    (with-current-buffer (generate-new-buffer buffer-name)
      (switch-to-buffer (current-buffer))
      (erase-buffer)
      (insert output)
      (goto-char (point-min)))))

(defun status--date-string (time)
  "Return TIME formatted as a YYYY-MM-DD date string."
  (format-time-string "%Y-%m-%d" time))

(defun status--week-range ()
  "Return a cons cell (START . END) for the reporting week.
Week starts on Sunday."
  (let* ((dow (string-to-number (format-time-string "%u")))
         (today (current-time)))
    (if (= dow 1)
        (cons (time-subtract today (days-to-time 6))
              (time-subtract today (days-to-time 2)))
      (cons (time-subtract today (days-to-time (- dow 1)))
            today))))

;;;###autoload
(defun status-accomplished-since-week-start ()
  "Generate a weekly accomplishments report in a new buffer."
  (interactive)
  (let* ((range (status--week-range))
         (start (status--date-string (car range)))
         (end (status--date-string (cdr range)))
         (buffer-name (format "Weekly Status %s" end))
         (results '()))
    (dolist (date (status--all-date-headings))
      (when (and (not (string< date start))
                 (not (string< end date)))
        (setq results
              (append results
                      (status--collect-category-tasks
                       date '("DONE" "DOING" "BLOCKED") nil)))))
    (let ((output
           (if results
               (concat "Accomplishments:\n"
                       (mapconcat (lambda (i) (concat "• " i))
                                  results "\n"))
             "Accomplishments:\n• (none)\n")))
      (with-current-buffer (generate-new-buffer buffer-name)
        (switch-to-buffer (current-buffer))
        (erase-buffer)
        (insert output)
        (goto-char (point-min))))))

(provide 'status)
;;; status.el ends here:
