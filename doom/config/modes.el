;;; $DOOMDIR/config/modes.el --- various modes -*- lexical-binding: t; -*-
;; Created: August 1, 2026
;; Modified: August 1, 2026
(define-derived-mode lfm-mode fundamental-mode "LFM"
  "Mode for editing Lazarus .lfm form files, with outline folding."
  (setq-local outline-regexp "[ \t]*object\\_>")
  (setq-local outline-level (lambda () (current-indentation)))
  (outline-minor-mode 1))

(add-to-list 'auto-mode-alist '("\\.lfm\\'" . lfm-mode))
