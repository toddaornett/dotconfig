;;; org-todo-tools.el --- Carry org tasks over to today's date section  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Todd Ornett
;;
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: November 13, 2026
;; Modified: November 20, 2026
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: outlines, convenience, tools
;; URL: https://github.com/toddaornett/dotconfig
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Extra functions for org-todo.
;;
;; `org-todo-tools-copy-subtree-to-today' carries the open tasks of the
;; current date section forward: it finds the outermost ancestor heading
;; whose title starts with a date stamp, moves the open subtrees into a
;; sibling section dated today, and leaves the DONE subtrees behind.
;; Both sections are folded afterwards, today's section is re-opened one
;; level, and point lands on its first task.
;;
;;; Code:

(require 'org)

(defconst org-todo-tools-date-regexp
  "\\(\\[\\|<\\)?\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\
\\( [A-Z][a-z][a-z]\\)?\\( [0-9]\\{2\\}:[0-9]\\{2\\}\\)?\
\\(?:-[0-9]\\{2\\}:[0-9]\\{2\\}\\)?\\(\\]\\|>\\)?"
  "Regexp matching a date at the start of a heading title.
Group 1 is the opening `<' or `[', group 2 the ISO date, group 3 the
day name including its leading space, group 4 the time including its
leading space, and group 5 the closing `>' or `]'.")

(defun org-todo-tools--done-keyword-p (keyword)
  "Return non-nil when KEYWORD names a finished TODO state."
  (or (and (member keyword org-done-keywords) t)
    (equal keyword "DONE")))

(defun org-todo-tools--date-title-p (title)
  "Return non-nil when TITLE starts with a date stamp."
  (and title
    (string-match-p (concat "\\`[ \t]*" org-todo-tools-date-regexp)
      title)))

(defun org-todo-tools--today-title (title)
  "Return TITLE with its leading date stamp moved to today."
  (replace-regexp-in-string
    org-todo-tools-date-regexp
    (lambda (match)
      (concat (match-string 1 match)
        (format-time-string "%Y-%m-%d")
        (and (match-string 3 match) (format-time-string " %a"))
        (and (match-string 4 match) (format-time-string " %H:%M"))
        (match-string 5 match)))
    title t t))

(defun org-todo-tools--subtree-end (position level)
  "Return the position where the subtree at POSITION of LEVEL ends.
Trailing blank lines are not included."
  (save-excursion
    (goto-char position)
    (forward-line 1)
    (let ((regexp (concat "^\\*\\{1," (int-to-string level) "\\}\\(?:[ \t]\\|$\\)")))
      (if (re-search-forward regexp nil t)
        (match-beginning 0)
        (point-max)))))

(defun org-todo-tools--parent-position (position)
  "Return the start of the parent heading of the heading at POSITION."
  (save-excursion
    (goto-char position)
    (org-back-to-heading t)
    (org-up-heading-safe)))

(defun org-todo-tools--date-ancestor ()
  "Return the outermost ancestor heading whose title starts with a date."
  (save-excursion
    (unless (org-before-first-heading-p)
      (org-back-to-heading t)
      (let (found)
        (while (progn
                 (when (org-todo-tools--date-title-p (org-get-heading t t t t))
                   (setq found (point)))
                 (if (org-up-heading-safe)
                   t
                   (goto-char (point-min))
                   nil)))
        found))))

(defun org-todo-tools--find-heading (title level parent)
  "Return the position of the heading titled TITLE at LEVEL under PARENT."
  (let (found)
    (save-excursion
      (goto-char (point-min))
      (while (and (not found)
               (re-search-forward org-heading-regexp nil t))
        (let ((position (match-beginning 0)))
          (when (and (equal (save-excursion
                              (goto-char position)
                              (org-outline-level))
                       level)
                  (equal (save-excursion
                           (goto-char position)
                           (org-get-heading t t t t))
                    title)
                  (equal (org-todo-tools--parent-position position) parent))
            (setq found position)))))
    found))

(defun org-todo-tools--task-ranges (begin end done)
  "Return subtrees between BEGIN and END whose TODO state matches DONE.
When DONE is non-nil return DONE subtrees, otherwise return every other
TODO subtree.  Nested subtrees covered by a returned parent are dropped.
Each element is a cons of its start and end positions, ordered from last
to first so that deletion leaves earlier positions valid."
  (let (ranges kept)
    (save-excursion
      (goto-char begin)
      (while (re-search-forward org-heading-regexp end t)
        (let ((position (match-beginning 0))
               level keyword)
          (save-excursion
            (goto-char position)
            (setq level (org-outline-level)
              keyword (org-get-todo-state)))
          (when (and keyword (eq (org-todo-tools--done-keyword-p keyword) done))
            (push (cons position
                    (org-todo-tools--subtree-end position level))
              ranges)))))
    (dolist (range (sort ranges (lambda (a b) (< (car a) (car b)))))
      (unless (and kept (<= (cdr range) (cdr (car kept))))
        (push range kept)))
    (sort kept (lambda (a b) (> (car a) (car b))))))

(defun org-todo-tools--relevel (text level)
  "Return TEXT with its headings shifted so the shallowest one is LEVEL."
  (let* ((first (and (string-match "^\\(\\*+\\) " text)
                  (length (match-string 1 text))))
          (delta (and first (- level first))))
    (if (or (null delta) (zerop delta))
      text
      (replace-regexp-in-string
        "^\\(\\*+\\) "
        (lambda (match)
          (concat (make-string (max 1 (+ (length (match-string 1 match)) delta))
                    ?*)
            " "))
        text t t))))

(defun org-todo-tools--delete-tasks (begin end done)
  "Delete subtrees between BEGIN and END whose TODO state matches DONE.
DONE descendants of a deleted open subtree are kept, shifted so that
they sit directly below the section heading at BEGIN."
  (let ((level (unless done
                 (1+ (save-excursion
                       (goto-char begin)
                       (org-outline-level))))))
    (dolist (range (org-todo-tools--task-ranges begin end done))
      (let ((kept (when (null done)
                    (nreverse
                      (mapcar (lambda (r)
                                (org-todo-tools--relevel
                                  (buffer-substring-no-properties (car r) (cdr r))
                                  level))
                        (org-todo-tools--task-ranges
                          (car range) (cdr range) t))))))
        (delete-region (car range) (cdr range))
        (save-excursion
          (goto-char (car range))
          (dolist (text kept)
            (insert text "\n")))))))

(defun org-todo-tools--delete-section-if-empty (begin end)
  "Delete the section between BEGIN and END when nothing is left in it."
  (when (string-empty-p
          (string-trim
            (buffer-substring-no-properties
              (save-excursion
                (goto-char begin)
                (forward-line 1)
                (point))
              end)))
    (delete-region begin end)))

(defun org-todo-tools--present (begin today stars title)
  "Fold the source and today's sections, then land on today's first ta

BEGIN marks the source section and TODAY marks the section dated today.
STARS and TITLE identify the source heading; the source is skipped when
its section no longer exists.  Point ends on the first heading below
TODAY."
  (when (and (marker-position begin)
          (save-excursion
            (goto-char begin)
            (looking-at-p (concat "^" (regexp-quote stars) " "
                            (regexp-quote title) "[ \t]*$"))))
    (save-excursion
      (goto-char begin)
      (org-fold-hide-subtree)))
  (goto-char today)
  (org-fold-hide-subtree)
  (org-fold-show-subtree)
  (org-next-visible-heading 1))

(defun org-todo-tools-copy-subtree-to-today ()
  "Move open tasks of the enclosing date section to a section dated today.

The source is the outermost ancestor headline whose title starts with a
date stamp.  Its open tasks move into a sibling headline carrying
today's date in the same format, created directly above the source
section when no section for today exists yet; an existing section for
today receives the tasks at its top.  DONE subtrees are dropped from the
copy and every non-DONE TODO subtree is removed from the source, so open
tasks move forward and closed ones stay behind.  The source section is
deleted when that leaves it empty.  Both sections are then folded,
today's section is re-opened one level, and point ends on its first
task."
  (interactive)
  (let ((source (org-todo-tools--date-ancestor)))
    (unless source
      (user-error "No date heading above point"))
    (let* ((source-title (save-excursion
                           (goto-char source)
                           (org-get-heading t t t t)))
            (title (org-todo-tools--today-title source-title))
            (level (save-excursion
                     (goto-char source)
                     (org-outline-level)))
            (source-end (org-todo-tools--subtree-end source level)))
      (cond
        ((equal title source-title)
          (message "Section is already dated today"))
        ((null (org-todo-tools--task-ranges source source-end nil))
          (message "No open tasks to carry over"))
        (t
          (let* ((begin (copy-marker source t))
                  (end (copy-marker source-end t))
                  (body (string-trim-right
                          (buffer-substring-no-properties
                            (save-excursion
                              (goto-char source)
                              (forward-line 1)
                              (point))
                            source-end)))
                  (target (org-todo-tools--find-heading
                            title level (org-todo-tools--parent-position source)))
                  (today (copy-marker (or target source)))
                  (start (if target
                           (save-excursion
                             (goto-char target)
                             (forward-line 1)
                             (point))
                           source))
                  copy-begin copy-end)
            (save-excursion
              (goto-char start)
              (setq copy-begin (point))
              (insert (if target
                        (concat body "\n")
                        (concat (make-string level ?*) " " title "\n" body "\n")))
              (setq copy-end (point)))
            ;; Keep open tasks in the copy, closed tasks in the source.
            (org-todo-tools--delete-tasks copy-begin copy-end t)
            (org-todo-tools--delete-tasks
              (marker-position begin) (marker-position end) nil)
            (org-todo-tools--delete-section-if-empty begin end)
            (org-todo-tools--present begin today
              (make-string level ?*) source-title)
            (message "Carried %s" title)))))))

(provide 'org-todo-tools)
;;; org-todo-tools.el ends here
