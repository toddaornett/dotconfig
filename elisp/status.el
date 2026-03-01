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
  "Path to Org file containing daily task logs."
  :type 'file
  :group 'status)

(defcustom status-category-list '("Emacs" "Dev" "Work" "Shell")
  "List of categories to include in status output.

Each category should appear in task headings like:

  ** DONE Emacs: task description"
  :type '(repeat string)
  :group 'status)

(defun status--date-string (time)
  "Return TIME formatted as a YYYY-MM-DD date string."
  (format-time-string "%Y-%m-%d" time))

(defun status--find-day-heading (date)
  "Move point to the Org heading matching DATE.

DATE must be a string in YYYY-MM-DD format.
Return non-nil if the heading is found."
  (goto-char (point-min))
  (re-search-forward (concat "^\\* " date) nil t))

(defun status--most-recent-date-before (date)
  "Return the most recent date string in the task file before DATE.

DATE must be in YYYY-MM-DD format.  The returned value is also a
YYYY-MM-DD string, or nil if no earlier date exists."
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
  "Collect tasks from DATE matching STATES and `status-category-list'.

DATE is a YYYY-MM-DD string identifying the day subtree.
STATES is a list of Org TODO keywords (e.g. \"DONE\", \"DOING\").
If MARK-ONGOING is non-nil, tasks in the \"DOING\" state are
annotated with \"(ongoing)\".

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
  "Format a status section with TITLE and ITEMS.

ITEMS should be a list of strings.  Always return a string.  If
ITEMS is empty or nil, a placeholder \"(none)\" entry is used."
  (if (and items (not (null items)))
      (concat title ":\n"
              (mapconcat (lambda (i) (concat "• " i)) items "\n")
              "\n")
    (concat title ":\n• (none)\n")))

;;;###autoload
(defun status-daily ()
  "Generate a daily status report buffer.

The report contains two sections:
\"Yesterday\" (completed or ongoing tasks from the most recent
prior date) and \"Today\" (TODO or ongoing tasks from today).

The output is shown in a new buffer named
\"Daily Status YYYY-MM-DD\"."
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
  "Return a time value representing the start of the current week.

The week is considered to start on Monday."
  (let ((dow (string-to-number (format-time-string "%u"))))
    (time-subtract (current-time) (days-to-time (1- dow)))))

;;;###autoload
(defun status-accomplished-since-week-start ()
  "Generate a weekly accomplishments report buffer.

Collect all completed or ongoing tasks from the beginning of the
current week up to today.  The output is shown in a new buffer
named \"Weekly Status YYYY-MM-DD\"."
  (interactive)
  (let* ((start (status--date-string (status--week-start)))
         (end (status--date-string (current-time)))
         (buffer-name (format "Weekly Status %s" end))
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
