;;; datetime.el --- date and time support -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Todd Ornett
;;
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Created: September 4, 2026
;; Modified: September 4, 2026
;; Version: 0.0.1
;; Keywords: calendar, comm, convenience, tools
;; Package-Requires: ((emacs "27.1") (tzc "0.1.0"))
;; Homepage: https://github.com/yourusername/dotconfig
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package basically wraps other packages such as tzc for more convenience.
;;
;;; Code:

(require 'parse-time)
(require 'tzc)

(defvar datetime-zone-aliases
  '(("JST" . "Asia/Tokyo")
    ("JT" . "Asia/Tokyo")
    ("UTC" . "UTC")
    ("GMT" . "UTC")
    ("CT" . "America/Chicago")
    ("CST" . "America/Chicago")
    ("CDT" . "America/Chicago")
    ("PT" . "America/Los_Angeles")
    ("PST" . "America/Los_Angeles")
    ("PDT" . "America/Los_Angeles")
    ("ET" . "America/New_York")
    ("EST" . "America/New_York")
    ("EDT" . "America/New_York")
    ("MT" . "America/Denver")
    ("MST" . "America/Denver")
    ("MDT" . "America/Denver")
    ("AKT" . "America/Anchorage")
    ("AKST" . "America/Anchorage")
    ("AKDT" . "America/Anchorage"))
  "Abbreviation to IANA timezone mapping for freeform conversion.")

(defconst datetime--time-re
  "\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\(?:[[:space:]]*\\([AaPp]\\.?[Mm]\\.?\\)\\)?"
  "Hour, minute, and optional AM/PM.")

(defun datetime--favourite-zones ()
  "Zones offered as conversion targets."
  (if (boundp 'tzc-favourite-time-zones)
      tzc-favourite-time-zones
    '("UTC"
      "Asia/Tokyo"
      "America/Los_Angeles"
      "America/Chicago"
      "America/New_York"
      "Europe/London")))

(defun datetime--source-zone (input)
  "IANA zone from abbreviations or names in INPUT."
  (or (and (string-match "\\([A-Za-z]+/[A-Za-z0-9_+-]+\\)" input)
           (match-string 1 input))
      (let ((case-fold-search t)
            found)
        (dolist (pair datetime-zone-aliases found)
          (unless found
            (when (string-match-p (format "\\<%s\\>" (regexp-quote (car pair))) input)
              (setq found (cdr pair))))))
      "UTC"))

(defun datetime--hour-24 (hour meridiem)
  "Convert 12-hour HOUR plus MERIDIEM to 24-hour."
  (let ((m (and meridiem (upcase (replace-regexp-in-string "\\." "" meridiem)))))
    (cond ((null m) hour)
          ((string= m "AM") (if (= hour 12) 0 hour))
          ((string= m "PM") (if (= hour 12) 12 (+ hour 12)))
          (t hour))))

(defun datetime--parse-times (input)
  "Return a list of (HOUR MINUTE) pairs in 24-hour form from INPUT."
  (if (string-match (concat datetime--time-re
                           "[[:space:]]*[–—-][[:space:]]*"
                           datetime--time-re)
                    input)
      (let* ((h1 (string-to-number (match-string 1 input)))
             (m1 (string-to-number (match-string 2 input)))
             (p1 (match-string 3 input))
             (h2 (string-to-number (match-string 4 input)))
             (m2 (string-to-number (match-string 2 input))) ; corrected indexing fallback match
             (m2 (string-to-number (match-string 5 input)))
             (p2 (match-string 6 input))
             (mer (or p1 p2)))
        (list (list (datetime--hour-24 h1 mer) m1)
              (list (datetime--hour-24 h2 (or p2 mer)) m2)))
    (if (string-match datetime--time-re input)
        (list (list (datetime--hour-24 (string-to-number (match-string 1 input))
                                      (match-string 3 input))
                    (string-to-number (match-string 2 input))))
      (error "Could not parse a time of day from the input"))))

(defun datetime--parse-date (input)
  "Return (YEAR MONTH DAY) from INPUT, defaulting missing parts to today."
  (let* ((today (decode-time))
         (parsed (parse-time-string input)))
    (list (or (decoded-time-year parsed) (decoded-time-year today))
          (or (decoded-time-month parsed) (decoded-time-month today))
          (or (decoded-time-day parsed) (decoded-time-day today)))))

(defun datetime--encode (year month day hour minute zone)
  "Encode local Y-M-D HOUR:MINUTE in ZONE, honoring DST."
  (encode-time (list 0 minute hour day month year nil -1 zone)))

(defun datetime--format-date (time zone)
  "English weekday-month date of TIME in ZONE."
  (let ((system-time-locale "C"))
    (replace-regexp-in-string
     "  +" " "
     (string-trim (format-time-string "%A, %B %e, %Y" time zone)))))

(defun datetime--format-clock (time zone)
  "12-hour clock of TIME in ZONE, no leading zero."
  (let* ((hour (string-to-number (format-time-string "%H" time zone)))
         (minute (format-time-string "%M" time zone))
         (h12 (mod hour 12))
         (h12 (if (zerop h12) 12 h12)))
    (cons (format "%d:%s" h12 minute)
          (if (< hour 12) "AM" "PM"))))

(defun datetime--format-zone (time zone)
  "Abbreviation of ZONE at TIME."
  (let ((system-time-locale "C"))
    (format-time-string "%Z" time zone)))

(defun datetime--format-instants (times zone)
  "Format TIMES (one or two encoded instants) in ZONE."
  (let* ((start (car times))
         (end (cadr times))
         (date1 (datetime--format-date start zone))
         (clock1 (datetime--format-clock start zone))
         (abbrev (datetime--format-zone (or end start) zone)))
    (if (null end)
        (format "%s, %s %s %s" date1 (car clock1) (cdr clock1) abbrev)
      (let ((date2 (datetime--format-date end zone))
            (clock2 (datetime--format-clock end zone)))
        (cond
         ((not (string= date1 date2))
          (format "%s, %s %s – %s, %s %s %s"
                  date1 (car clock1) (cdr clock1)
                  date2 (car clock2) (cdr clock2)
                  abbrev))
         ((string= (cdr clock1) (cdr clock2))
          (format "%s, %s–%s %s %s"
                  date1 (car clock1) (car clock2) (cdr clock2) abbrev))
         (t
          (format "%s, %s %s–%s %s %s"
                  date1 (car clock1) (cdr clock1)
                  (car clock2) (cdr clock2) abbrev)))))))

;;;###autoload
(defun datetime-convert-freeform (input-string &optional target-zone)
  "Parse a messy, freeform INPUT-STRING and convert it to TARGET-ZONE.
TARGET-ZONE defaults to Asia/Tokyo.  If a text region is active, it uses
that text automatically instead of prompting for INPUT-STRING."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Paste freeform date/time text: "))
         (completing-read "Target zone (default Asia/Tokyo): " (datetime--favourite-zones)
                          nil t nil nil "Asia/Tokyo")))
  (when (string-blank-p input-string)
    (error "Input cannot be empty"))
  (let* ((target (if (or (null target-zone) (string-empty-p target-zone)) "Asia/Tokyo" target-zone))
         (source (datetime--source-zone input-string))
         (date (datetime--parse-date input-string))
         (times (datetime--parse-times input-string))
         (year (nth 0 date))
         (month (nth 1 date))
         (day (nth 2 date))
         (start (datetime--encode year month day
                                 (nth 0 (car times)) (nth 1 (car times))
                                 source))
         (end (when (cdr times)
                (let ((encoded (datetime--encode year month day
                                                (nth 0 (cadr times))
                                                (nth 1 (cadr times))
                                                source)))
                  (if (time-less-p encoded start)
                      (time-add encoded (days-to-time 1))
                    encoded))))
         (result (datetime--format-instants (if end (list start end) (list start))
                                           target)))
    (when (called-interactively-p 'any)
      (message "Result: %s" result))
    result))

(provide 'datetime)
;;; datetime.el ends here
