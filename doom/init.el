;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       ;;bidi
       ;;chinese
       ;;japanese
       ;;layout

       :completion
       company             ; Required for Rust auto-completion
       vertico             ; Keep for efficient search

       :ui
       doom                ; Core Doom UI
       doom-dashboard      ; Optional, minimal impact
       hl-todo             ; Useful for code annotations
       indent-guides       ; Helpful for Rust code
       (ligatures +monaco) ; Remove +extra to reduce font processing
       modeline            ; Keep for status display
       ophints             ; Lightweight, keep
       (popup +defaults)   ; Manage temporary windows
       treemacs            ; Optional, disable if unused
       (vc-gutter +pretty) ; Keep for version control
       vi-tilde-fringe     ; Lightweight, keep
       workspaces          ; Keep for project isolation
       ;; Removed: tabs, window-select, zen (optional, add back if needed)

       :editor
       (evil +everywhere)  ; Core editing experience
       file-templates      ; Lightweight, keep
       fold                ; Useful for code navigation
       (format +format-with-lsp +apheleia) ; Keep for LSP-based formatting
       snippets            ; Keep for productivity
       ;; Removed: multiple-cursors, parinfer (optional, add back if needed)

       :emacs
       dired               ; Keep for file management
       electric            ; Keep for indent
       ibuffer             ; Keep for buffer management
       undo                ; Keep for undo history
       vc                  ; Keep for version control

       :term
       (vterm +toggle)     ; Keep for terminal needs

       :checkers
       syntax              ; Keep for error checking
       (spell +flyspell)   ; Optional, disable if unused
       ;;grammar           ; Disable to reduce overhead

       :tools
       (tree-sitter +lsp)
       projectile          ; Essential for Rust projects
       direnv              ; Keep for environment management
       editorconfig        ; Keep for consistent coding styles
       (eval +overlay)     ; Useful for code execution
       lookup              ; Useful for code navigation
       (magit +forge)      ; Keep for Git integration
       make                ; Keep for build tasks
       rgb                 ; Lightweight, keep
       (ripgrep +extra)    ; Keep for fast search
       lsp                 ; Keep for LSP support

       :os
       (:if IS-MAC macos)  ; Keep for macOS compatibility

       :lang
       emacs-lisp          ; Keep for Elisp development
       json                ; Lightweight, keep
       (javascript +lsp +tree-sitter)
       (typescript +lsp +tree-sitter)
       (web +lsp)
       markdown            ; Lightweight, keep
       (org +pomodoro)     ; Keep for note-taking
       (rust +eglot)       ; Use eglot only, remove +lsp and +cargo
       sh                  ; Lightweight, keep
       yaml                ; Lightweight, keep

       :email
       ;;mu4e
       ;;notmuch
       ;;wanderlust

       :app
       ;;calendar
       ;;emms
       ;;irc
       ;;rss
       ;;twitter

       :config
       (default +bindings +smartparens))
