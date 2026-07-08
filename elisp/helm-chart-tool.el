;;; helm-chart-tool.el --- Tools for maintaining Helm Charts -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Todd Ornett
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: June 17, 2026
;; Modified: June 26, 2026
;; Version: 0.0.7
;; Keywords: convenience helm chart
;; Homepage: https://github.com/todd.ornett/dotconfig
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides tools to manage Helm Chart YAML files.
;;
;; Two interactive commands are provided:
;;
;;   `helm-chart-tool-add-envars'
;;     Adds variables that are present in the source env file but missing
;;     from the chart YAML files.  Existing values are never touched.
;;
;;   `helm-chart-tool-update-envars'
;;     Updates variables that already exist in the chart YAML files but
;;     whose values differ from the source env file.  New variables are
;;     not added; use `helm-chart-tool-add-envars' for that.
;;
;;   `helm-chart-tool-sync-envars'
;;     Combines both: adds missing variables AND updates changed ones in
;;     a single pass.
;;
;;; Code:

(require 'cl-lib)

(defgroup helm-chart-tool nil
  "Maintain Helm charts."
  :group 'tools)

(defcustom helm-chart-tool-envars-filename
  (or (getenv "HELM_CHART_TOOL_ENVARS_FILENAME") "env.example")
  "Filename of the source environment variable name/value pairs file."
  :type 'string
  :group 'helm-chart-tool)

(defcustom helm-chart-tool-envars-directory
  (or (getenv "HELM_CHART_TOOL_ENVARS_DIRECTORY") "~/dev/Phoenix/golang/clickhouse-ingester-v2")
  "Directory containing `helm-chart-tool-envars-filename'.
The base-name of this directory is used as the chart name."
  :type 'string
  :group 'helm-chart-tool)

(defcustom helm-chart-tool-charts-directory
  (or (getenv "HELM_CHART_TOOL_CHARTS_DIRECTORY") "~/dev/Phoenix/charts")
  "Root directory that contains all Helm chart subdirectories.
The chart to patch is found at <charts-directory>/<chart-name>/."
  :type 'string
  :group 'helm-chart-tool)

(defcustom helm-chart-tool-umbrella-values
  (or (getenv "HELM_CHART_TOOL_UMBRELLA_VALUES") "")
  "Absolute path to the umbrella chart values.yaml file.
When non-empty, the block keyed by the current chart name within this file
is also patched.  When empty, umbrella patching is skipped."
  :type 'string
  :group 'helm-chart-tool)


;;; ---- Path helpers -----------------------------------------------------------

(defun helm-chart-tool--envars-path ()
  "Return the absolute path to the source env-vars file."
  (expand-file-name
   helm-chart-tool-envars-filename
   (expand-file-name helm-chart-tool-envars-directory)))

(defun helm-chart-tool--chart-name ()
  "Return the chart name derived from the base-name of the envars directory."
  (file-name-nondirectory
   (directory-file-name
    (expand-file-name helm-chart-tool-envars-directory))))

(defun helm-chart-tool--helm-chart-dir ()
  "Return the absolute path to the helm chart directory for the current service."
  (expand-file-name (helm-chart-tool--chart-name)
                    (expand-file-name helm-chart-tool-charts-directory)))


;;; ---- Parsing ----------------------------------------------------------------

(defun helm-chart-tool--parse-envars (path)
  "Parse PATH (a KEY=VALUE file) and return an alist of (NAME . VALUE).
Blank lines and lines beginning with # are ignored.
Surrounding single or double quotes on values are stripped."
  (unless (file-exists-p path)
    (error "helm-chart-tool: env-vars file not found: %s" path))
  (with-temp-buffer
    (insert-file-contents path)
    (let (pairs)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-trim (thing-at-point 'line t))))
          (unless (or (string-empty-p line)
                      (string-prefix-p "#" line)
                      (not (string-match-p "=" line)))
            (let* ((eq-pos (string-search "=" line))
                   (name   (string-trim (substring line 0 eq-pos)))
                   (value  (string-trim (substring line (1+ eq-pos))))
                   (value  (if (and (> (length value) 1)
                                    (or (and (string-prefix-p "\"" value)
                                             (string-suffix-p "\"" value))
                                        (and (string-prefix-p "'" value)
                                             (string-suffix-p "'" value))))
                               (substring value 1 (1- (length value)))
                             value)))
              (push (cons name value) pairs))))
        (forward-line 1))
      (nreverse pairs))))

(defun helm-chart-tool--find-yaml-files (directory)
  "Recursively find *.yaml and *.yml files under DIRECTORY.
Files inside any templates subdirectory are excluded because they contain
Helm template logic that must not be patched."
  (unless (file-directory-p directory)
    (error "helm-chart-tool: helm chart directory not found: %s" directory))
  (cl-remove-if
   (lambda (path)
     (string-match-p (regexp-quote "/templates/") path))
   (directory-files-recursively directory "\\.ya?ml\\'")))


;;; ---- YAML entry building ----------------------------------------------------

(defun helm-chart-tool--build-env-entry (name value env-indent)
  "Return a YAML mapping entry line for NAME and VALUE.
ENV-INDENT is the indentation of the enclosing env: key; the entry is
indented two spaces beyond it, producing:
  <env-indent>  NAME: \"VALUE\""
  (concat env-indent "  " name ": \"" value "\"\n"))


;;; ---- Prefix-aware insertion -------------------------------------------------

(defun helm-chart-tool--longest-common-prefix (a b)
  "Return the longest string that is a common prefix of A and B."
  (let ((len (min (length a) (length b)))
        (i 0))
    (while (and (< i len) (eq (aref a i) (aref b i)))
      (setq i (1+ i)))
    (substring a 0 i)))

(defun helm-chart-tool--best-anchor (new-name existing-entries)
  "Return the insertion buffer position for NEW-NAME given EXISTING-ENTRIES.
EXISTING-ENTRIES is a list of (NAME . END-CHAR-POS).  The function finds
all existing entries sharing the longest common left-substring prefix with
NEW-NAME and returns the END-CHAR-POS of the last such entry, so the new
var is placed after the whole prefix group.  Returns nil when no prefix
is shared, signalling that the caller should append at the block end."
  (let ((best-len 0)
        candidates)
    (dolist (entry existing-entries)
      (let ((plen (length (helm-chart-tool--longest-common-prefix
                           new-name (car entry)))))
        (when (> plen best-len)
          (setq best-len plen))))
    (when (> best-len 0)
      (dolist (entry existing-entries)
        (when (= best-len (length (helm-chart-tool--longest-common-prefix
                                   new-name (car entry))))
          (push entry candidates)))
      (cdr (cl-reduce (lambda (a b) (if (>= (cdr a) (cdr b)) a b))
                      candidates)))))


;;; ---- Block scanning ---------------------------------------------------------

(defun helm-chart-tool--block-end (block-start env-indent)
  "Return the buffer position of the first line that closes the env: block.
BLOCK-START is the position on the line immediately after the env: key line.
ENV-INDENT is the indentation string of the env: key.

The block ends at the first non-blank, non-comment line whose indentation
is less than or equal to ENV-INDENT.  That line is NOT part of the block
and must not be modified.  Returns point-max when the block runs to EOF."
  (let ((entry-indent (concat env-indent "  "))
        end)
    (save-excursion
      (goto-char block-start)
      (while (and (not end) (not (eobp)))
        (let ((line (thing-at-point 'line t)))
          (cond
           ((string-match-p "\\`[ \t]*\\'" line)
            (forward-line 1))
           ((and (string-match-p "\\`[ \t]*#" line)
                 (>= (- (match-end 0) (match-beginning 0) 1)
                     (length entry-indent)))
            (forward-line 1))
           ((and (string-match-p "\\`[ \t]+" line)
                 (>= (- (match-end 0) (match-beginning 0))
                     (length entry-indent)))
            (forward-line 1))
           (t
            (setq end (point))))))
      (or end (point-max)))))

(defun helm-chart-tool--scan-env-block (block-start block-end entry-indent)
  "Scan a KEY: value mapping block and return an alist of (NAME . END-POS).
BLOCK-START and BLOCK-END delimit the region to scan.
ENTRY-INDENT is the expected indentation string of each KEY: line.
END-POS is the buffer position after the entry's last associated line,
including any blank lines or comments that immediately follow it, so that
inserting at END-POS places the new entry after the whole group."
  (let (entries
        (pending-name nil)
        (pending-end  nil))
    (save-excursion
      (goto-char block-start)
      (while (< (point) block-end)
        (let ((line (thing-at-point 'line t)))
          (cond
           ;; Blank line — extends the trailing region of the current entry.
           ((string-match-p "\\`[ \t]*\\'" line)
            (forward-line 1))
           ;; Comment at entry indent — also extends trailing region.
           ((string-match-p (concat "\\`" (regexp-quote entry-indent) "#") line)
            (forward-line 1))
           ;; KEY: value entry — commit the previous pending entry first,
           ;; then start tracking the new one.
           ((string-match
             (concat "\\`" (regexp-quote entry-indent) "\\([A-Za-z_][A-Za-z0-9_]*\\):")
             line)
            (when pending-name
              (push (cons pending-name pending-end) entries))
            (setq pending-name (match-string 1 line))
            (forward-line 1)
            (setq pending-end (point)))
           ;; Anything else — end of block, commit and stop.
           (t
            (goto-char block-end)))))
      ;; Commit the last pending entry.
      (when pending-name
        (push (cons pending-name pending-end) entries)))
    (nreverse entries)))


;;; ---- Buffer patching (add) --------------------------------------------------

(defun helm-chart-tool--patch-env-block (envars env-indent block-start block-end)
  "Insert missing vars from ENVARS into the env: block between BLOCK-START and BLOCK-END.
ENV-INDENT is the indentation string of the env: key.
Returns a list of (NAME . ANCHOR-NAME-OR-NIL) for every var inserted."
  (let* ((entry-indent     (concat env-indent "  "))
         (existing-entries (helm-chart-tool--scan-env-block
                            block-start block-end entry-indent))
         (existing-names   (mapcar #'car existing-entries))
         ;; pos-groups: alist of (INSERT-POS . list-of-(text name anchor))
         ;; keyed by insertion position so multiple vars at the same position
         ;; are emitted together in source order rather than reversed.
         (pos-groups       '())
         (added            '()))
    (dolist (pair envars)
      (let ((name  (car pair))
            (value (cdr pair)))
        (unless (member name existing-names)
          (let* ((anchor-pos  (helm-chart-tool--best-anchor name existing-entries))
                 (insert-pos  (or anchor-pos block-end))
                 (anchor-name (when anchor-pos
                                (car (cl-find anchor-pos existing-entries
                                              :key #'cdr :test #'=))))
                 (text        (helm-chart-tool--build-env-entry
                               name value env-indent))
                 (group       (assoc insert-pos pos-groups)))
            (if group
                (nconc group (list (list text name anchor-name)))
              (push (list insert-pos (list text name anchor-name)) pos-groups))))))
    ;; Apply groups from highest buffer position downwards so earlier positions
    ;; remain valid.  Within each group emit entries in their original order.
    (setq pos-groups (sort pos-groups (lambda (a b) (> (car a) (car b)))))
    (dolist (group pos-groups)
      (let ((pos     (car group))
            (entries (cdr group)))
        (goto-char pos)
        (dolist (entry entries)
          (cl-destructuring-bind (text name anchor) entry
            (insert text)
            (push (cons name anchor) added)))))
    (nreverse added)))

(defun helm-chart-tool--patch-buffer (envars)
  "Patch every env: mapping block in the current buffer using ENVARS.
ENVARS is an alist of (NAME . VALUE).
Returns a list of (NAME . ANCHOR-NAME-OR-NIL) for every var inserted."
  (let (added)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([ \t]*\\)env:[ \t]*$" nil t)
        (let* ((env-indent  (match-string-no-properties 1))
               (block-start (progn (forward-line 1) (point)))
               (block-end   (helm-chart-tool--block-end block-start env-indent)))
          (setq added
                (append added
                        (helm-chart-tool--patch-env-block
                         envars env-indent block-start block-end))))))
    added))

(defun helm-chart-tool--patch-yaml-file (path envars)
  "Patch the YAML file at PATH inserting missing vars from ENVARS.
Returns a list of (NAME . ANCHOR) pairs for vars added, or nil if none."
  (with-temp-buffer
    (insert-file-contents path)
    (let ((added (helm-chart-tool--patch-buffer envars)))
      (when added
        (write-region (point-min) (point-max) path))
      added)))


;;; ---- Buffer updating (change existing values) -------------------------------

(defun helm-chart-tool--update-env-block (envars env-indent block-start block-end)
  "Update vars in ENVARS that already exist in the env: block but have changed values.
Scans the block between BLOCK-START and BLOCK-END for lines of the form:
  <entry-indent>NAME: \"OLD_VALUE\"   or   <entry-indent>NAME: OLD_VALUE
and replaces the value portion with the one from ENVARS when they differ.
ENV-INDENT is the indentation string of the enclosing env: key.
Returns a list of (NAME OLD-VALUE NEW-VALUE) triples for every var updated."
  (let* ((entry-indent (concat env-indent "  "))
         (envars-map   (copy-sequence envars))   ; alist for fast lookup
         updated)
    (save-excursion
      (goto-char block-start)
      (while (< (point) block-end)
        (let* ((line-start (point))
               (line       (thing-at-point 'line t))
               ;; Match:  <entry-indent>NAME: ["']?VALUE["']?  (no leading #)
               (rx         (concat "\\`" (regexp-quote entry-indent)
                                   "\\([A-Za-z_][A-Za-z0-9_]*\\):"
                                   "[ \t]*\\(\"[^\"]*\"\\|'[^']*'\\|[^\n]*\\)"))
               (matched    (and line (string-match rx line))))
          (if (not matched)
              (forward-line 1)
            (let* ((name      (match-string 1 line))
                   (raw-val   (string-trim (match-string 2 line)))
                   ;; Strip surrounding quotes from the on-disk value.
                   (old-value (if (and (> (length raw-val) 1)
                                       (or (and (string-prefix-p "\"" raw-val)
                                                (string-suffix-p "\"" raw-val))
                                           (and (string-prefix-p "'" raw-val)
                                                (string-suffix-p "'" raw-val))))
                                  (substring raw-val 1 (1- (length raw-val)))
                                raw-val))
                   (source    (assoc name envars-map))
                   (new-value (cdr source)))
              (if (and source (not (string= old-value new-value)))
                  (let* ((new-line (helm-chart-tool--build-env-entry
                                    name new-value env-indent))
                         (line-end (save-excursion (end-of-line) (1+ (point)))))
                    ;; Replace the entire line in place; keep buffer position
                    ;; consistent by moving forward after deletion+insert.
                    (delete-region line-start line-end)
                    (insert new-line)
                    ;; point is now at the start of the next line — no forward-line.
                    (push (list name old-value new-value) updated))
                (forward-line 1)))))))
    (nreverse updated)))

(defun helm-chart-tool--update-buffer (envars)
  "Update changed values in every env: block in the current buffer.
ENVARS is an alist of (NAME . VALUE) sourced from env.example.
Returns a list of (NAME OLD-VALUE NEW-VALUE) triples for every var changed."
  (let (updated)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([ \t]*\\)env:[ \t]*$" nil t)
        (let* ((env-indent  (match-string-no-properties 1))
               (block-start (progn (forward-line 1) (point)))
               (block-end   (helm-chart-tool--block-end block-start env-indent)))
          (setq updated
                (append updated
                        (helm-chart-tool--update-env-block
                         envars env-indent block-start block-end))))))
    updated))

(defun helm-chart-tool--update-yaml-file (path envars)
  "Update changed env var values in the YAML file at PATH using ENVARS.
Returns a list of (NAME OLD-VALUE NEW-VALUE) triples for vars changed, or nil."
  (with-temp-buffer
    (insert-file-contents path)
    (let ((updated (helm-chart-tool--update-buffer envars)))
      (when updated
        (write-region (point-min) (point-max) path))
      updated)))


;;; ---- Umbrella chart patching ------------------------------------------------

(defun helm-chart-tool--service-block-region (chart-name)
  "Return (START . END) of the CHART-NAME: top-level block in the current buffer.
START is the position of the first line after the CHART-NAME: key line.
END is the position of the first subsequent non-blank, non-comment line
that is at the same or outer indentation level, i.e. the start of the next
top-level key or EOF.
Returns nil when CHART-NAME: is not found."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (concat "^\\(" (regexp-quote chart-name) "\\):[ \t]*$") nil t)
      (let ((block-start (progn (forward-line 1) (point)))
            end)
        (while (and (not end) (not (eobp)))
          (let ((line (thing-at-point 'line t)))
            (cond
             ((string-match-p "\\`[ \t]*\\'" line)
              (forward-line 1))
             ((string-match-p "\\`[ \t]*#" line)
              (forward-line 1))
             ((string-match-p "\\`[ \t]+" line)
              (forward-line 1))
             (t
              (setq end (point))))))
        (cons block-start (or end (point-max)))))))

(defun helm-chart-tool--patch-umbrella-buffer (envars chart-name)
  "Patch the env: block inside the CHART-NAME service block in the current buffer.
Only the env: block that falls within CHART-NAME's indented region is touched;
env: blocks belonging to other services are not affected.
ENVARS is an alist of (NAME . VALUE).
Returns a list of (NAME . ANCHOR-NAME-OR-NIL) for every var inserted, or nil."
  (let ((region (helm-chart-tool--service-block-region chart-name)))
    (unless region
      (error "helm-chart-tool: service block '%s:' not found in umbrella values"
             chart-name))
    (let ((service-start (car region))
          (service-end   (cdr region))
          added)
      (save-excursion
        (goto-char service-start)
        (when (re-search-forward "^\\([ \t]*\\)env:[ \t]*$" service-end t)
          (let* ((env-indent  (match-string-no-properties 1))
                 (block-start (progn (forward-line 1) (point)))
                 (block-end   (helm-chart-tool--block-end block-start env-indent)))
            (setq added
                  (helm-chart-tool--patch-env-block
                   envars env-indent block-start block-end)))))
      added)))

(defun helm-chart-tool--patch-umbrella-file (path envars chart-name)
  "Patch the umbrella values file at PATH for service CHART-NAME using ENVARS.
Returns a list of (NAME . ANCHOR) pairs for vars added, or nil if none."
  (unless (file-exists-p path)
    (error "helm-chart-tool: umbrella values file not found: %s" path))
  (with-temp-buffer
    (insert-file-contents path)
    (let ((added (helm-chart-tool--patch-umbrella-buffer envars chart-name)))
      (when added
        (write-region (point-min) (point-max) path))
      added)))

(defun helm-chart-tool--update-umbrella-buffer (envars chart-name)
  "Update changed env var values inside the CHART-NAME service block.
Only the env: block within CHART-NAME's indented region is touched.
ENVARS is an alist of (NAME . VALUE).
Returns a list of (NAME OLD-VALUE NEW-VALUE) triples for every var changed."
  (let ((region (helm-chart-tool--service-block-region chart-name)))
    (unless region
      (error "helm-chart-tool: service block '%s:' not found in umbrella values"
             chart-name))
    (let ((service-start (car region))
          (service-end   (cdr region))
          updated)
      (save-excursion
        (goto-char service-start)
        (when (re-search-forward "^\\([ \t]*\\)env:[ \t]*$" service-end t)
          (let* ((env-indent  (match-string-no-properties 1))
                 (block-start (progn (forward-line 1) (point)))
                 (block-end   (helm-chart-tool--block-end block-start env-indent)))
            (setq updated
                  (helm-chart-tool--update-env-block
                   envars env-indent block-start block-end)))))
      updated)))

(defun helm-chart-tool--update-umbrella-file (path envars chart-name)
  "Update changed env var values in the umbrella file at PATH for CHART-NAME.
Returns a list of (NAME OLD-VALUE NEW-VALUE) triples for vars changed, or nil."
  (unless (file-exists-p path)
    (error "helm-chart-tool: umbrella values file not found: %s" path))
  (with-temp-buffer
    (insert-file-contents path)
    (let ((updated (helm-chart-tool--update-umbrella-buffer envars chart-name)))
      (when updated
        (write-region (point-min) (point-max) path))
      updated)))


;;; ---- Reporting --------------------------------------------------------------

(defun helm-chart-tool--report-added (buf added yaml-path)
  "Insert a patch report for ADDED vars in YAML-PATH into BUF."
  (with-current-buffer buf
    (if added
        (progn
          (insert (format "[PATCHED]  %s\n" yaml-path))
          (dolist (pair added)
            (if (cdr pair)
                (insert (format "           + %-40s (after %s)\n"
                                (car pair) (cdr pair)))
              (insert (format "           + %-40s (appended)\n"
                              (car pair))))))
      (insert (format "[OK]       %s\n" yaml-path)))))

(defun helm-chart-tool--report-updated (buf updated yaml-path)
  "Insert an update report for UPDATED vars in YAML-PATH into BUF.
UPDATED is a list of (NAME OLD-VALUE NEW-VALUE) triples."
  (with-current-buffer buf
    (if updated
        (progn
          (insert (format "[UPDATED]  %s\n" yaml-path))
          (dolist (triple updated)
            (insert (format "           ~ %-40s  \"%s\" -> \"%s\"\n"
                            (nth 0 triple)
                            (nth 1 triple)
                            (nth 2 triple)))))
      (insert (format "[OK]       %s\n" yaml-path)))))

(defun helm-chart-tool--report-synced (buf added updated yaml-path)
  "Insert a combined add+update report for YAML-PATH into BUF."
  (with-current-buffer buf
    (if (or added updated)
        (progn
          (insert (format "[SYNCED]   %s\n" yaml-path))
          (dolist (pair added)
            (if (cdr pair)
                (insert (format "           + %-40s (after %s)\n"
                                (car pair) (cdr pair)))
              (insert (format "           + %-40s (appended)\n"
                              (car pair)))))
          (dolist (triple updated)
            (insert (format "           ~ %-40s  \"%s\" -> \"%s\"\n"
                            (nth 0 triple)
                            (nth 1 triple)
                            (nth 2 triple)))))
      (insert (format "[OK]       %s\n" yaml-path)))))


;;; ---- Shared setup -----------------------------------------------------------

(defun helm-chart-tool--resolve-paths ()
  "Return a plist of all resolved paths and parsed envars.
Keys: :envars-path :chart-name :helm-chart-dir :umbrella-path :envars :yaml-files."
  (let* ((envars-path    (helm-chart-tool--envars-path))
         (chart-name     (helm-chart-tool--chart-name))
         (helm-chart-dir (helm-chart-tool--helm-chart-dir))
         (envars         (helm-chart-tool--parse-envars envars-path))
         (yaml-files     (helm-chart-tool--find-yaml-files helm-chart-dir))
         (umbrella-path  (expand-file-name
                          (if (and helm-chart-tool-umbrella-values
                                   (not (string-empty-p
                                         helm-chart-tool-umbrella-values)))
                              helm-chart-tool-umbrella-values
                            (format "%s/umbrella-chart" helm-chart-dir)))))
    (unless envars
      (error "helm-chart-tool: no KEY=VALUE pairs found in %s" envars-path))
    (unless yaml-files
      (error "helm-chart-tool: no YAML files found under %s" helm-chart-dir))
    (list :envars-path    envars-path
          :chart-name     chart-name
          :helm-chart-dir helm-chart-dir
          :umbrella-path  umbrella-path
          :envars         envars
          :yaml-files     yaml-files)))

(defun helm-chart-tool--init-report (buf paths operation)
  "Erase BUF and write the run header for OPERATION using resolved PATHS."
  (with-current-buffer buf
    (erase-buffer)
    (insert (format "helm-chart-tool %s -- %s\n" operation (current-time-string)))
    (insert (format "Env-vars source : %s\n"
                    (plist-get paths :envars-path)))
    (insert (format "Chart name      : %s\n"
                    (plist-get paths :chart-name)))
    (insert (format "Helm chart dir  : %s\n"
                    (plist-get paths :helm-chart-dir)))
    (insert (format "Umbrella values : %s\n"
                    (plist-get paths :umbrella-path)))
    (insert (format "Source vars     : %s\n\n"
                    (mapcar #'car (plist-get paths :envars))))))


;;; ---- Interactive commands ---------------------------------------------------

;;;###autoload
(defun helm-chart-tool-add-envars ()
  "Add missing environment variables to Helm chart YAML files.

Reads NAME=VALUE pairs from the source env file (controlled by
`helm-chart-tool-envars-filename' and `helm-chart-tool-envars-directory'),
then patches two targets:

1. Every *.yaml / *.yml file found under `helm-chart-tool-charts-directory'
   / <chart-name>/, excluding any templates/ subdirectory.

2. The service block inside `helm-chart-tool-umbrella-values' or if that is empty
   <helm-chart-dir>/umbrella-chart (the file must exist).

In both cases missing variables are placed immediately after the last
existing variable sharing the longest common left-substring prefix, or
appended at the end of the env: block when no prefix match exists.

Existing values are never modified.  Use `helm-chart-tool-update-envars'
to update changed values, or `helm-chart-tool-sync-envars' for both.

Results are reported in the *helm-chart-tool* buffer."
  (interactive)
  (let* ((paths          (helm-chart-tool--resolve-paths))
         (envars         (plist-get paths :envars))
         (yaml-files     (plist-get paths :yaml-files))
         (umbrella-path  (plist-get paths :umbrella-path))
         (chart-name     (plist-get paths :chart-name))
         (report-buf     (get-buffer-create "*helm-chart-tool*"))
         (total-patched  0))
    (helm-chart-tool--init-report report-buf paths "add-envars")

    (with-current-buffer report-buf (insert "-- Chart files --\n"))
    (dolist (yaml-path yaml-files)
      (let ((added (helm-chart-tool--patch-yaml-file yaml-path envars)))
        (when added (cl-incf total-patched))
        (helm-chart-tool--report-added report-buf added yaml-path)))

    (when umbrella-path
      (with-current-buffer report-buf (insert "\n-- Umbrella chart --\n"))
      (let ((added (helm-chart-tool--patch-umbrella-file
                    umbrella-path envars chart-name)))
        (when added (cl-incf total-patched))
        (helm-chart-tool--report-added report-buf added umbrella-path)))

    (with-current-buffer report-buf
      (insert (format "\nDone. %d file(s) updated.\n" total-patched)))

    (display-buffer report-buf)
    (message "helm-chart-tool: done -- %d file(s) updated.  See *helm-chart-tool* for details."
             total-patched)))

;;;###autoload
(defun helm-chart-tool-update-envars ()
  "Update environment variables in Helm chart YAML files to match env.example.

Reads NAME=VALUE pairs from the source env file (controlled by
`helm-chart-tool-envars-filename' and `helm-chart-tool-envars-directory'),
then for each env: block in:

1. Every *.yaml / *.yml file under `helm-chart-tool-charts-directory' /
   <chart-name>/ (templates/ excluded), and

2. The service block inside the umbrella values file,

any variable that already exists but whose value differs from the source
file is overwritten with the source value.

Variables that are absent from the chart files are NOT added; use
`helm-chart-tool-add-envars' for that, or `helm-chart-tool-sync-envars'
for both operations in one pass.

Results are reported in the *helm-chart-tool* buffer."
  (interactive)
  (let* ((paths          (helm-chart-tool--resolve-paths))
         (envars         (plist-get paths :envars))
         (yaml-files     (plist-get paths :yaml-files))
         (umbrella-path  (plist-get paths :umbrella-path))
         (chart-name     (plist-get paths :chart-name))
         (report-buf     (get-buffer-create "*helm-chart-tool*"))
         (total-updated  0))
    (helm-chart-tool--init-report report-buf paths "update-envars")

    (with-current-buffer report-buf (insert "-- Chart files --\n"))
    (dolist (yaml-path yaml-files)
      (let ((updated (helm-chart-tool--update-yaml-file yaml-path envars)))
        (when updated (cl-incf total-updated))
        (helm-chart-tool--report-updated report-buf updated yaml-path)))

    (when umbrella-path
      (with-current-buffer report-buf (insert "\n-- Umbrella chart --\n"))
      (let ((updated (helm-chart-tool--update-umbrella-file
                      umbrella-path envars chart-name)))
        (when updated (cl-incf total-updated))
        (helm-chart-tool--report-updated report-buf updated umbrella-path)))

    (with-current-buffer report-buf
      (insert (format "\nDone. %d file(s) updated.\n" total-updated)))

    (display-buffer report-buf)
    (message "helm-chart-tool: done -- %d file(s) updated.  See *helm-chart-tool* for details."
             total-updated)))

;;;###autoload
(defun helm-chart-tool-sync-envars ()
  "Add missing AND update changed environment variables in Helm chart YAML files.

This command combines `helm-chart-tool-add-envars' and
`helm-chart-tool-update-envars' in a single pass:

  • Variables present in env.example but absent from a chart file are inserted
    (using prefix-aware placement).
  • Variables present in both but with differing values are overwritten with
    the env.example value.
  • Variables already present with the correct value are left untouched.

Results are reported in the *helm-chart-tool* buffer."
  (interactive)
  (let* ((paths          (helm-chart-tool--resolve-paths))
         (envars         (plist-get paths :envars))
         (yaml-files     (plist-get paths :yaml-files))
         (umbrella-path  (plist-get paths :umbrella-path))
         (chart-name     (plist-get paths :chart-name))
         (report-buf     (get-buffer-create "*helm-chart-tool*"))
         (total-changed  0))
    (helm-chart-tool--init-report report-buf paths "sync-envars")

    (with-current-buffer report-buf (insert "-- Chart files --\n"))
    (dolist (yaml-path yaml-files)
      ;; Run add then update on the same file.  Because add-envars writes the
      ;; file first, update-envars sees the freshly inserted entries and will
      ;; not attempt to update them (they already have the correct value).
      (let* ((added   (helm-chart-tool--patch-yaml-file  yaml-path envars))
             (updated (helm-chart-tool--update-yaml-file yaml-path envars)))
        (when (or added updated) (cl-incf total-changed))
        (helm-chart-tool--report-synced report-buf added updated yaml-path)))

    (when umbrella-path
      (with-current-buffer report-buf (insert "\n-- Umbrella chart --\n"))
      (let* ((added   (helm-chart-tool--patch-umbrella-file
                       umbrella-path envars chart-name))
             (updated (helm-chart-tool--update-umbrella-file
                       umbrella-path envars chart-name)))
        (when (or added updated) (cl-incf total-changed))
        (helm-chart-tool--report-synced report-buf added updated umbrella-path)))

    (with-current-buffer report-buf
      (insert (format "\nDone. %d file(s) updated.\n" total-changed)))

    (display-buffer report-buf)
    (message "helm-chart-tool: done -- %d file(s) updated.  See *helm-chart-tool* for details."
             total-changed)))

(provide 'helm-chart-tool)
;;; helm-chart-tool.el ends here
