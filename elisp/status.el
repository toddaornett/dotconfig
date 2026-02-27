;;; status.el --- create status messages from org task buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Todd Ornett
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: February 27, 2026
;; Version: 0.0.1
;; Keywords: outlines, org, convenience
;; Homepage: https://github.com/toddaornett/dotconfig
;; Package-Requires: ((emacs "24.4") (org "9.0"))

;;; Commentary:
;; Generate Slack-friendly status messages from an Org task buffer.

;;; Code:

(require 'org)
(require 'subr-x)

(defgroup status nil
  "Generate status messages from an Org task file."
  :group 'org
  :prefix "status-")

(defcustom status-org-task-file "~/Notes/tasks.org"
  "Path to Org file containing daily task logs.
The file is expected to contain top-level date headings such as:

  * 2026-02-27
  ** DONE CR: some task

The path is expanded using `expand-file-name' before opening."
  :type 'file
  :group 'status)

(defcustom status-category "CR"
  "Only include tasks whose headline category matches this string.
For example, with the default value \"CR\", only tasks like:

  ** DONE CR: task description

will be included in generated status output."
  :type 'string
  :group 'status)

(defun status--date-string (time)
  "Return TIME formatted as a YYYY-MM-DD date string."
  (format-time-string "%Y-%m-%d" time))

(defun status--previous-workday ()
  "Return the time value of the previous workday.
If today is Monday, this returns the previous Friday.
Otherwise, it returns yesterday."
  (let* ((dow (string-to-number (format-time-string "%u"))) ;; 1 = Monday
         (days (if (= dow 1) 3 1)))
    (time-subtract (current-time) (days-to-time days))))

(defun status--find-day-heading (date)
  "Move point to the Org heading for DATE.
DATE must be a string in YYYY-MM-DD format.
Return non-nil if the heading is found."
  (goto-char (point-min))
  (re-search-forward (concat "^\\* " date) nil t))

(defun status--collect-category-tasks (date states &optional mark-ongoing)
  "Collect tasks for DATE whose TODO state is in STATES.

DATE must be a string in YYYY-MM-DD format.
STATES is a list of strings such as '(\"DONE\" \"DOING\" \"TODO\").

Only tasks whose category matches `status-category' are included.

If MARK-ONGOING is non-nil, DOING tasks are annotated with \"(ongoing)\".

Return a list of task description strings."
  (let ((results '())
        (file (expand-file-name status-org-task-file)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (when (status--find-day-heading date)
          (org-narrow-to-subtree)
          (goto-char (point-min))
          (while (re-search-forward
                  (format "^\\*\\* \\(%s\\) %s: \\(.*\\)$"
                          (regexp-opt states)
                          (regexp-quote status-category))
                  nil t)
            (let* ((state (match-string 1))
                   (task (string-trim (match-string 2)))
                   (final (if (and mark-ongoing (string= state "DOING"))
                              (format "%s (ongoing)" task)
                            task)))
              (push final results)))
          (widen))))
    (nreverse results)))

(defun status--format-section (title items)
  "Format a status section with TITLE and ITEMS.
ITEMS must be a list of task description strings.
Return a formatted string suitable for insertion into a buffer."
  (when items
    (concat title ":\n"
            (mapconcat (lambda (i) (concat "• " i)) items "\n")
            "\n")))

;;;###autoload
(defun status-daily ()
  "Insert a Slack-friendly daily status report into the current buffer.

The report contains two sections:

  Yesterday:
    • DONE and DOING tasks from the previous workday

  Today:
    • TODO and DOING tasks from today

Only tasks matching `status-category' are included.
DOING tasks from yesterday are marked as \"(ongoing)\"."
  (interactive)
  (let* ((today (status--date-string (current-time)))
         (yesterday (status--date-string (status--previous-workday)))
         (y-items (status--collect-category-tasks
                   yesterday '("DONE" "DOING") t))
         (t-items (status--collect-category-tasks
                   today '("TODO" "DOING") nil))
         (output (concat
                  (status--format-section "Yesterday" y-items)
                  "\n"
                  (status--format-section "Today" t-items))))
    (insert output)))

(defun status--week-start ()
  "Return the time value corresponding to the start of the current week.
The week is assumed to start on Monday."
  (let ((dow (string-to-number (format-time-string "%u"))))
    (time-subtract (current-time) (days-to-time (1- dow)))))

;;;###autoload
(defun status-accomplished-since-week-start ()
  "Insert a Slack-friendly accomplishments report into the current buffer.

The report includes all DONE and DOING tasks from the start of the
current week up to today, filtered by `status-category'."
  (interactive)
  (let* ((start (status--date-string (status--week-start)))
         (end (status--date-string (current-time)))
         (file (expand-file-name status-org-task-file))
         (results '()))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\* \\([0-9-]+\\)$" nil t)
          (let ((date (match-string 1)))
            (when (and (string>= date start)
                       (string<= date end))
              (setq results
                    (append results
                            (status--collect-category-tasks
                             date '("DONE" "DOING") nil))))))))
    (let ((output (concat "Accomplishments:\n"
                           (mapconcat (lambda (i) (concat "• " i))
                                      results "\n"))))
      (insert output))))

(provide 'status)
;;; status.el ends here
