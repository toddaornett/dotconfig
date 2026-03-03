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

The file is expected to contain headings whose title is a date in
YYYY-MM-DD format, at any heading level, for example:

  * Projects
  ** 2026-03-01
  *** DONE Emacs: refactor status.el
  *** DOING Dev: weekly report

Tasks must be Org TODO headings located anywhere under the date
heading.  Heading level does not matter."
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

(defun status--goto-date-heading (date)
  "Move point to the Org heading whose title is DATE.
Return non-nil if found."
  (goto-char (point-min))
  (let ((re (format org-complex-heading-regexp-format
                    (regexp-quote date))))
    (re-search-forward re nil t)))

(defun status--all-date-headings ()
  "Return a list of all date heading strings in the buffer."
  (let (dates)
    (org-map-entries
     (lambda ()
       (let ((h (org-get-heading t t t t)))
         (when (string-match-p
                "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" h)
           (push h dates)))))
    dates))

(defun status--most-recent-date-before (date)
  "Return the most recent date string before DATE."
  (let ((dates (status--all-date-headings))
        (best nil))
    (dolist (d dates)
      (when (and (string< d date)
                 (or (null best) (string< best d)))
        (setq best d)))
    best))

(defun status--collect-category-tasks (date states &optional mark-ongoing)
  "Collect tasks for DATE whose TODO state is in STATES.
If MARK-ONGOING is non-nil, annotate DOING tasks with \"(ongoing)\"."
  (let (results)
    (with-current-buffer (find-file-noselect status-org-task-file)
      (org-with-wide-buffer
       (unless (derived-mode-p 'org-mode)
         (org-mode))
       (save-excursion
         (when (status--goto-date-heading date)
           (org-narrow-to-subtree)
           (org-map-entries
            (lambda ()
              (let* ((state (org-get-todo-state))
                     (title (org-get-heading t t t t)))
                (when (and state
                           (member state states)
                           (string-match
                            (format "^\\(%s\\):[ \t]+\\(.*\\)$"
                                    (regexp-opt status-category-list))
                            title))
                  (let* ((task (match-string 2 title))
                         (final (if (and mark-ongoing
                                         (string= state "DOING"))
                                    (format "%s (ongoing)" task)
                                  task)))
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
  "Generate a daily status report in a new buffer."
  (interactive)
  (let* ((today (status--date-string (current-time)))
         (yesterday (or (with-current-buffer (find-file-noselect status-org-task-file)
                          (org-with-wide-buffer
                           (unless (derived-mode-p 'org-mode)
                             (org-mode))
                           (status--most-recent-date-before today)))
                        today))
         (buffer-name (format "Daily Status %s" today))
         (y-items (status--collect-category-tasks
                   yesterday '("DONE" "DOING") t))
         (t-items (status--collect-category-tasks
                   today '("TODO" "DONE" "DOING") nil))
         (output (concat
                  (status--format-section "Yesterday" y-items)
                  "\n"
                  (status--format-section "Today" t-items))))
    (with-current-buffer (generate-new-buffer buffer-name)
      (switch-to-buffer (current-buffer))
      (erase-buffer)
      (insert output)
      (goto-char (point-min)))))

(defun status--week-range ()
  "Return a cons cell (START . END) for the reporting week.
Week starts on Sunday."
  (let* ((dow (string-to-number (format-time-string "%u"))) ;; 1=Mon … 7=Sun
         (today (current-time)))
    (if (= dow 1)
        ;; Monday → last week's Sunday..Saturday
        (cons (time-subtract today (days-to-time 6))
              (time-subtract today (days-to-time 2)))
      ;; Other days → this Sunday..today
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
    (with-current-buffer (find-file-noselect status-org-task-file)
      (org-with-wide-buffer
       (unless (derived-mode-p 'org-mode)
         (org-mode))
       (save-excursion
         (dolist (date (status--all-date-headings))
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
