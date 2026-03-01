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
  "Path to the Org file containing dated task logs.

The file is expected to contain top-level headings in this form:

  * YYYY-MM-DD

with task entries beneath them such as:

  ** DONE Emacs: some task"
  :type 'file
  :group 'status)

(defcustom status-category-list '("Emacs" "Dev" "Work" "Shell")
  "List of task categories to include in status output.

Each category should appear in task headings like:

  ** DONE Emacs: task description"
  :type '(repeat string)
  :group 'status)

(defun status--date-string (time)
  "Return TIME formatted as a YYYY-MM-DD date string."
  (format-time-string "%Y-%m-%d" time))

(defun status--find-day-heading (date)
  "Move point to the Org heading for DATE.

DATE must be a string in YYYY-MM-DD format.

Return non-nil if the heading is found."
  (goto-char (point-min))
  (re-search-forward (concat "^\\* " date) nil t))

(defun status--most-recent-date-before (date)
  "Return the most recent date string in the task file before DATE.

DATE must be in YYYY-MM-DD format.

If no earlier date exists, return nil."
  (let ((file (expand-file-name status-org-task-file))
        (best nil))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\* \\([0-9-]+\\)$" nil t)
          (let ((d (match-string 1)))
            (when (and (string< d date)
                       (or (null best) (string< best d)))
              (setq best d))))))
    best))

(defun status--collect-category-tasks (date states &optional mark-ongoing)
  "Collect tasks for DATE whose TODO state is in STATES.

DATE must be a string in YYYY-MM-DD format.
STATES is a list of strings such as '(\"DONE\" \"DOING\" \"TODO\").

Only tasks whose category matches `status-category-list' are included.

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
                  (format "^\\*+ +\\(%s\\) +\\(%s\\): +\\(.*\\)$"
                          (regexp-opt states)
                          (regexp-opt status-category-list))
                  nil t)
            (let* ((state (match-string 1))
                   (task (string-trim (match-string 3)))
                   (final (if (and mark-ongoing (string= state "DOING"))
                              (format "%s (ongoing)" task)
                            task)))
              (push final results)))
          (widen))))
    (nreverse results)))

(defun status--format-section (title items)
  "Format a status section titled TITLE from ITEMS.

ITEMS must be a list of task description strings.

Always return a string, inserting \"(none)\" if ITEMS is empty."
  (if (and items (not (null items)))
      (concat title ":\n"
              (mapconcat (lambda (i) (concat "• " i)) items "\n")
              "\n")
    (concat title ":\n• (none)\n")))

;;;###autoload
(defun status-daily ()
  "Generate a daily status report in a new buffer.

The buffer is named \"Daily Status YYYY-MM-DD\".

The report contains two sections:

  Yesterday:
    • DONE and DOING tasks from the most recent prior date

  Today:
    • TODO and DOING tasks from today

Only tasks matching `status-category-list' are included."
  (interactive)
  (let* ((today (status--date-string (current-time)))
         (yesterday (or (status--most-recent-date-before today)
                        today))
         (buffer-name (format "Daily Status %s" today))
         (y-items (status--collect-category-tasks
                   yesterday '("DONE" "DOING") t))
         (t-items (status--collect-category-tasks
                   today '("TODO" "DOING") nil))
         (output (concat
                  (status--format-section "Yesterday" y-items)
                  "\n"
                  (status--format-section "Today" t-items))))
    (with-current-buffer (generate-new-buffer buffer-name)
      (switch-to-buffer (current-buffer))
      (erase-buffer)
      (insert output)
      (goto-char (point-min)))))

(defun status--week-start ()
  "Return the time value corresponding to the start of the current week.

The week is assumed to start on Monday."
  (let ((dow (string-to-number (format-time-string "%u"))))
    (time-subtract (current-time) (days-to-time (1- dow)))))

(defun status--week-range ()
  "Return a cons cell (START . END) for the reporting week.

If today is Monday, the range is the previous Sunday through
previous Saturday. Otherwise, the range is from this week's
Sunday through today."
  (let* ((dow (string-to-number (format-time-string "%u"))) ;; 1=Mon … 7=Sun
         (today (current-time)))
    (if (= dow 1)
        ;; Monday → last week's Sunday..Saturday
        (cons (time-subtract today (days-to-time 6)) ; Sunday
              (time-subtract today (days-to-time 2))) ; Saturday
      ;; Other days → this Sunday..today
      (cons (time-subtract today (days-to-time (- dow 1)))
            today))))

;;;###autoload
(defun status-accomplished-since-week-start ()
  "Generate a weekly accomplishments report in a new buffer.

The buffer is named \"Weekly Status YYYY-MM-DD\".

The report includes all DONE and DOING tasks from the start of
the current week up to today, filtered by `status-category-list'."
  (interactive)
  (let* ((range (status--week-range))
         (start (status--date-string (car range)))
         (end (status--date-string (cdr range)))
         (buffer-name (format "Weekly Status %s" end))
         (file (expand-file-name status-org-task-file))
         (results '()))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\* \\([0-9-]+\\)$" nil t)
          (let ((date (match-string 1)))
            (when (and (not (string< date start))
                       (not (string< end date)))
              (setq results
                    (append results
                            (status--collect-category-tasks
                             date '("DONE" "DOING") nil))))))))
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
;;; status.el ends here
