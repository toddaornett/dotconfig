;;; helm-chart-tool.el --- Tools for maintaining Helm Charts -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Todd Ornett
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: June 17, 2026
;; Modified: June 17, 2026
;; Version: 0.0.6
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
  (or (getenv "HELM_CHART_TOOL_ENVARS_DIRECTORY") "~/Projects/service")
  "Directory containing `helm-chart-tool-envars-filename'.
The base-name of this directory is used as the chart name."
  :type 'string
  :group 'helm-chart-tool)

(defcustom helm-chart-tool-charts-directory
  (or (getenv "HELM_CHART_TOOL_CHARTS_DIRECTORY") "~/Projects/charts/service")
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


;;; ---- Buffer patching --------------------------------------------------------

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

(defun helm-chart-tool--report-added (buf added yaml-path)
  "Insert a patch report for ADDED vars in YAML-PATH into BUF."
  (with-current-buffer buf
    (if added
        (progn
          (insert (format "[PATCHED] %s\n" yaml-path))
          (dolist (pair added)
            (if (cdr pair)
                (insert (format "          + %-40s (after %s)\n"
                                (car pair) (cdr pair)))
              (insert (format "          + %-40s (appended)\n"
                              (car pair))))))
      (insert (format "[OK]      %s\n" yaml-path)))))


;;;###autoload
(defun helm-chart-tool-add-envars ()
  "Add missing environment variables to Helm chart YAML files.

Reads NAME=VALUE pairs from the source env file (controlled by
`helm-chart-tool-envars-filename' and `helm-chart-tool-envars-directory'),
then patches two targets:

1. Every *.yaml / *.yml file found under `helm-chart-tool-charts-directory'
   / <chart-name>/, excluding any templates/ subdirectory.

2. The service block inside `helm-chart-tool-umbrella-values' or if that is empty
   `helm-chart-dir'/umbrella-charts and that path must exist.

In both cases missing variables are placed immediately after the last
existing variable sharing the longest common left-substring prefix, or
appended at the end of the env: block when no prefix match exists.

Results are reported in the *helm-chart-tool* buffer."
  (interactive)
  (let* ((envars-path     (helm-chart-tool--envars-path))
         (chart-name      (helm-chart-tool--chart-name))
         (helm-chart-dir  (helm-chart-tool--helm-chart-dir))
         (envars          (helm-chart-tool--parse-envars envars-path))
         (yaml-files      (helm-chart-tool--find-yaml-files helm-chart-dir))
         (umbrella-path   (expand-file-name
                           (if (and helm-chart-tool-umbrella-values
                                    (not (string-empty-p
                                          helm-chart-tool-umbrella-values)))
                               helm-chart-tool-umbrella-values
                             (format "%s/umbrella-chart" helm-chart-dir))))
         (report-buf      (get-buffer-create "*helm-chart-tool*"))
         (total-patched   0))
    (unless envars
      (error "helm-chart-tool: no KEY=VALUE pairs found in %s" envars-path))
    (unless yaml-files
      (error "helm-chart-tool: no YAML files found under %s" helm-chart-dir))

    (with-current-buffer report-buf
      (erase-buffer)
      (insert (format "helm-chart-tool run -- %s\n" (current-time-string)))
      (insert (format "Env-vars source : %s\n" envars-path))
      (insert (format "Chart name      : %s\n" chart-name))
      (insert (format "Helm chart dir  : %s\n" helm-chart-dir))
      (when umbrella-path
        (insert (format "Umbrella values : %s\n" umbrella-path)))
      (insert (format "Source vars     : %s\n\n" (mapcar #'car envars))))

    ;; 1. Patch individual chart yaml files.
    (with-current-buffer report-buf
      (insert "-- Chart files --\n"))
    (dolist (yaml-path yaml-files)
      (let ((added (helm-chart-tool--patch-yaml-file yaml-path envars)))
        (when added (cl-incf total-patched))
        (helm-chart-tool--report-added report-buf added yaml-path)))

    ;; 2. Patch the umbrella chart values file.
    (when umbrella-path
      (with-current-buffer report-buf
        (insert "\n-- Umbrella chart --\n"))
      (let ((added (helm-chart-tool--patch-umbrella-file
                    umbrella-path envars chart-name)))
        (when added (cl-incf total-patched))
        (helm-chart-tool--report-added report-buf added umbrella-path)))

    (with-current-buffer report-buf
      (insert (format "\nDone. %d file(s) updated.\n" total-patched)))

    (display-buffer report-buf)
    (message "helm-chart-tool: done -- %d file(s) updated.  See *helm-chart-tool* for details."
             total-patched)))

(provide 'helm-chart-tool)
;;; helm-chart-tool.el ends here
