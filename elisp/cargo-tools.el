;;; cargo-tools.el --- Some cargo tools and workflows -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Todd Ornett
;;
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: August 24, 2026
;; Modified: September 3, 2026
;; Version: 0.0.1
;; Keywords: rust cargo tools convenience files
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/toddaornett/dotconfig
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package provides some cargo tools and workflows.
;;
;;; Code:

(defun cargo-tools--find-cargo-toml (&optional start-dir)
  "Find the nearest Cargo.toml relevant to START-DIR.
START-DIR defaults to `default-directory'.  Search order:

1. START-DIR/Cargo.toml
2. START-DIR/rust/Cargo.toml
3. Walking upward from START-DIR via `locate-dominating-file'.

Returns the full path to the Cargo.toml file, or signals an error
if none is found by any of the above."
  (let* ((start (file-name-as-directory (or start-dir default-directory)))
         (direct (expand-file-name "Cargo.toml" start))
         (rust-subdir (expand-file-name "Cargo.toml" (expand-file-name "rust" start)))
         (dominating-dir (locate-dominating-file start "Cargo.toml")))
    (cond
     ((file-exists-p direct) direct)
     ((file-exists-p rust-subdir) rust-subdir)
     (dominating-dir (expand-file-name "Cargo.toml" dominating-dir))
     (t (error "cargo-tools: could not find Cargo.toml in, under rust/, or above %s" start)))))

;;;###autoload
(defun cargo-tools-update-tag-version (package-name version)
  "Update the version for PACKAGE-NAME in Cargo.toml, setting it to VERSION.
Matches any dependency key that is exactly PACKAGE-NAME or starts with
PACKAGE-NAME followed by more name characters (e.g. `foo-macros` when
PACKAGE-NAME is `foo`).
For each matching line, if it contains a `tag = \"...\"` field (e.g.
`foo = { git = \"...\", tag = \"v1.2.3\" }`), the version inside that
tag string is replaced with VERSION.  Otherwise, if the dependency is
declared in the simple form `foo = \"1.2.3\"`, that version string is
replaced with VERSION instead.

The Cargo.toml operated on is found by `cargo-tools--find-cargo-toml',
which checks `default-directory' first, then a `rust' subdirectory of
it, then falls back to searching upward through ancestor directories."
  (interactive "sUpdate package name: \nsVersion for tag: ")
  (let ((cargo-file-path (cargo-tools--find-cargo-toml))
        (name-re (concat "^\\(" (regexp-quote package-name)
                          "[A-Za-z0-9_-]*\\)[ \t]*=[ \t]*"))
        (updated 0))
    (with-current-buffer (find-file-noselect cargo-file-path)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward name-re nil t)
          (let ((line-end (line-end-position)))
            (save-restriction
              (narrow-to-region (point) line-end)
              (goto-char (point-min))
              (cond
               ;; Preferred: a `tag = "..."` field somewhere on this line.
               ((re-search-forward
                 "\\btag[ \t]*=[ \t]*\"\\([^\"]*\\)\"" nil t)
                (replace-match version t t nil 1)
                (setq updated (1+ updated)))
               ;; Fallback: simple `name = "version"` form.
               ((looking-at "[ \t]*\"\\([^\"]*\\)\"")
                (replace-match version t t nil 1)
                (setq updated (1+ updated))))))
          (goto-char (line-end-position))))
      (when (> updated 0)
        (save-buffer)))
    (message "cargo-tools: updated %d matching line(s) in %s"
             updated cargo-file-path)))

(provide 'cargo-tools)
;;; cargo-tools.el ends here
