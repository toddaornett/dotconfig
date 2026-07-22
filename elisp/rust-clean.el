;;; rust-clean.el --- Remove Rust generated build directories and files -*- lexical-binding: t -*-

;; Author: Todd Ornet
;; Version: 1.2
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/toddaornett/dotconfig
;; Keywords: tools, rust, convenience

;;; Commentary:

;; Interactively find and delete all Rust `target' build directories
;; found recursively under a configurable root directory (defaults to
;; `rust-clean-directory', which itself defaults to
;; "~/dev/Phoenix").  Useful for reclaiming disk space across many
;; Cargo projects at once.
;;
;; Usage:
;;   M-x rust-clean-directories
;;
;; You will be prompted for a root directory (pre-filled with the
;; configured default, which you can accept or override) unless overridden by
;; `rust-clean-confirm-before-delete'. To change
;; this, or invoke with a prefix argument, e.g. `C-u M-x
;; rust-clean-directories', to require confirmation just for
;; that run).  Afterwards, a temporary report buffer is displayed
;; summarizing how much disk space was freed (in human-readable
;; units, e.g. Kb/Mb/Gb), sorted largest-first, per Rust project.
;;
;; Customize `rust-clean-directory' to change the default root,
;; e.g.:
;;
;;   (setq rust-clean-directory "~/code")
;;
;; Two guards protect against operating on anything that isn't
;; genuine Cargo build output:
;;
;; 1. `rust-clean-verify-cargo-project' (default t) -- only a
;;    `target' directory whose parent also contains a Cargo.toml or
;;    Cargo.lock file is considered.
;;
;; 2. `rust-clean-verify-build-artifacts' (default t) -- the
;;    `target' directory must actually contain at least one Cargo
;;    profile output directory (e.g. "debug", "release", or a custom
;;    profile recognized by its own "deps" subdirectory).  A `target'
;;    directory that is empty or doesn't look like real Cargo output
;;    is skipped.
;;
;; Disabling either guard is strongly discouraged.

;;; Code:

(require 'cl-lib)

(defgroup rust-clean nil
  "Remove Rust `target' build directories."
  :group 'tools
  :prefix "rust-clean-")

(defcustom rust-clean-directory "~/dev/Phoenix"
  "Default root directory under which to search for Rust `target' directories.
This is used by `rust-clean-directories' as the default value,
but can be overridden interactively on each invocation."
  :type 'directory
  :group 'rust-clean)

(defcustom rust-clean-confirm-before-delete t
  "If non-nil, ask for confirmation before deleting anything.
When nil (the default), `rust-clean-directories' deletes all
matching `target' directories immediately with no prompt.  Invoking
the command with a prefix argument always asks for confirmation for
that run, regardless of this setting."
  :type 'boolean
  :group 'rust-clean)

(defcustom rust-clean-verify-cargo-project t
  "Non-nil means only remove \target' directories from real Cargo projects.
This is one of the guards against deleting directories that merely
happen to be named `target' but do not belong to a Rust project.
Disabling this is strongly discouraged."
  :type 'boolean
  :group 'rust-clean)

(defcustom rust-clean-verify-build-artifacts t
  "Non-nil means only remove \target' directories with real build output.
This guards against deleting an empty or otherwise unrelated `target' directory.
Disabling this is strongly discouraged."
  :type 'boolean
  :group 'rust-clean)

(defcustom rust-clean-report-buffer-name "*Rust Cleanup Report*"
  "Name of the temporary buffer used to report freed disk space."
  :type 'string
  :group 'rust-clean)

(defun rust-clean--project-root (target-dir)
  "Return the Rust project directory (parent of TARGET-DIR)."
  (file-name-directory (directory-file-name target-dir)))

(defun rust-clean--cargo-project-p (target-dir)
  "Return non-nil if TARGET-DIR's parent matches pattern of a Cargo project root."
  (let ((parent (rust-clean--project-root target-dir)))
    (or (file-exists-p (expand-file-name "Cargo.toml" parent))
      (file-exists-p (expand-file-name "Cargo.lock" parent)))))

(defun rust-clean--has-build-artifacts-p (target-dir)
  "Return non-nil if TARGET-DIR has actual Cargo build output.

Cargo creates one or more profile subdirectories (\"debug\",
\"release\", or a custom profile) under `target', each holding
further build artifacts.  A custom profile directory is recognized by
containing its own \"deps\" subdirectory.  A `target' directory with
no such profile subdirectory (e.g., empty, or containing only
unrelated files) does not count as real build output."
  (cl-some (lambda (entry)
             (and (file-directory-p entry)
               (let ((name (file-name-nondirectory (directory-file-name entry))))
                 (or (member name '("debug" "release"))
                   (file-directory-p (expand-file-name "deps" entry))))))
    (directory-files target-dir t directory-files-no-dot-files-regexp)))

(defun rust-clean--eligible-p (target-dir)
  "Return non-nil if TARGET-DIR passes all configured safety guards."
  (and (or (not rust-clean-verify-cargo-project)
         (rust-clean--cargo-project-p target-dir))
    (or (not rust-clean-verify-build-artifacts)
      (rust-clean--has-build-artifacts-p target-dir))))

(defun rust-clean--find-directories (root)
  "Return a list of `target' directories found recursively under ROOT.
Only directories that pass `rust-clean--eligible-p' are
returned; this is the combined guard that keeps non-Rust directories
and empty/incomplete `target' directories from ever being processed."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (unless (file-directory-p root)
      (user-error "Directory does not exist: %s" root))
    (let ((found (directory-files-recursively
                   root
                   "\\`target\\'"
                   t
                   (lambda (dir)
                     ;; Don't descend into a `target' dir once found;
                     ;; nothing useful lives beneath it for our purposes.
                     (not (string-equal
                            (file-name-nondirectory (directory-file-name dir))
                            "target"))))))
      (cl-remove-if-not #'rust-clean--eligible-p found))))

(defun rust-clean--directory-size-bytes (dir)
  "Return the on-disk size of DIR in bytes, or nil if it cannot be determined."
  (when (executable-find "du")
    (let* ((output (shell-command-to-string
                     (format "du -sk %s 2>/dev/null" (shell-quote-argument dir))))
            (kb (car (split-string output "[ \t]+" t))))
      (when kb
        (* 1024 (string-to-number kb))))))

(defun rust-clean--human-size (bytes)
  "Format BYTES as a human-readable string (B/Kb/Mb/Gb/...)."
  (if (and bytes (> bytes 0))
    (file-size-human-readable bytes)
    "0B"))

;;;###autoload
(defun rust-clean-directories (&optional directory force-confirm)
  "Find and remove all Rust `target' directories under DIRECTORY.

Prompts for DIRECTORY, pre-filled with `rust-clean-directory'
so the default can be accepted as-is or overridden for this run.

Only directories that pass both configured guards are considered:
belonging to a genuine Cargo project (`rust-clean-verify-cargo-project')
and actually containing build output such as a \"debug\" or
\"release\" subdirectory (`rust-clean-verify-build-artifacts').

By default, matching directories are deleted immediately without
confirmation.  Call with a prefix argument (FORCE-CONFIRM), or set
`rust-clean-confirm-before-delete' to non-nil, to require
confirmation first.

A temporary report buffer is then shown listing the disk space freed
for each Rust project (largest first), plus the overall total."
  (interactive
    (list (read-directory-name "Search for Rust target directories in: "
            rust-clean-directory
            rust-clean-directory t)
      current-prefix-arg))
  (let* ((root (expand-file-name (or directory rust-clean-directory)))
          (targets (rust-clean--find-directories root)))
    (cond
      ((null targets)
        (message "No eligible Rust `target' directories found under %s" root))
      ((and (or rust-clean-confirm-before-delete force-confirm)
         (not (rust-clean--confirm targets root)))
        (message "Aborted. No directories were deleted."))
      (t
        (rust-clean--delete-and-report targets root)))))

(defun rust-clean--confirm (targets root)
  "Show TARGETS found under ROOT and ask the user to confirm deletion."
  (let ((buf (get-buffer-create "*Rust Target Directories*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Found %d `target' director%s under %s:\n\n"
                (length targets)
                (if (= (length targets) 1) "y" "ies")
                root))
      (dolist (dir targets)
        (insert (format "  %s\n" dir)))
      (goto-char (point-min))
      (display-buffer buf))
    (yes-or-no-p (format "Delete these %d `target' director%s? "
                   (length targets)
                   (if (= (length targets) 1) "y" "ies")))))

(defun rust-clean--delete-and-report (targets root)
  "Delete TARGETS (found under ROOT) and display a summary report."
  (let ((freed nil)   ; alist of (project-root . bytes-freed)
         (failed nil)  ; alist of (target-dir . error)
         (total-bytes 0))
    (dolist (dir targets)
      (let ((project (rust-clean--project-root dir))
             (size (rust-clean--directory-size-bytes dir)))
        (condition-case err
          (progn
            (delete-directory dir t t)
            (let ((bytes (or size 0)))
              (push (cons project bytes) freed)
              (cl-incf total-bytes bytes)))
          (error (push (cons dir err) failed)))))
    (rust-clean--show-report root (nreverse freed) (nreverse failed) total-bytes)))

(defun rust-clean--show-report (root freed failed total-bytes)
  "Display a temporary report buffer summarizing cleanup results.
ROOT is the searched directory, FREED is an alist of
\(project-root . bytes-freed), FAILED is an alist of
\(target-dir . error), and TOTAL-BYTES is the overall total freed."
  (let ((buf (get-buffer-create rust-clean-report-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Rust `target' Cleanup Report\n")
        (insert (make-string 28 ?=) "\n\n")
        (insert (format "Searched under: %s\n" root))
        (insert (format "Projects cleaned: %d\n" (length freed)))
        (insert (format "Total space freed: %s\n\n"
                  (rust-clean--human-size total-bytes)))
        (if freed
          (progn
            (insert "Freed per project (largest first):\n\n")
            (dolist (entry (sort (copy-sequence freed)
                             (lambda (a b) (> (cdr a) (cdr b)))))
              (insert (format "  %-8s  %s\n"
                        (rust-clean--human-size (cdr entry))
                        (directory-file-name (car entry))))))
          (insert "No projects were successfully cleaned.\n"))
        (when failed
          (insert (format "\nFailed to delete (%d):\n\n" (length failed)))
          (dolist (f failed)
            (insert (format "  %s -- %s\n"
                      (car f) (error-message-string (cdr f))))))
        (goto-char (point-min))
        (special-mode)
        (setq buffer-read-only t)))
    (display-buffer buf)
    (message "Freed %s across %d Rust project%s.%s"
      (rust-clean--human-size total-bytes)
      (length freed)
      (if (= (length freed) 1) "" "s")
      (if failed (format " %d failed, see %s." (length failed) rust-clean-report-buffer-name) ""))))

(provide 'rust-clean)

;;; rust-clean.el ends here
