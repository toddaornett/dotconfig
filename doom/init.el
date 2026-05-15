;;; init.el -*- lexical-binding: t; -*-

(doom!
 :input

 :completion
 company
 vertico

 :ui
 doom
 doom-dashboard
 hl-todo
 indent-guides
 (ligatures +monaco)
 modeline
 ophints
 (popup +defaults)
 treemacs
 (vc-gutter +pretty)
 vi-tilde-fringe

 :editor
 (meow +qwerty)

 :emacs
 dired
 electric
 ibuffer
 undo
 vc

 :term
 (vterm +toggle)

 :checkers
 syntax
 (spell +flyspell)

 :tools
 direnv
 editorconfig
 (eval +overlay)
 lookup
 (magit +forge)
 make
 rgb
 ripgrep
 tree-sitter

 :os
 (:if IS-MAC macos)

 :lang
 emacs-lisp
 json
 (javascript +tree-sitter)
 (typescript +tree-sitter)
 web
 markdown
 (org +pomodoro)
 (rust +eglot)
 sh
 yaml

 :config
 (default +smartparens +bindings))
