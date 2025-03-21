;;; init.el --- My initialization file -*- lexical-binding: t -*-
;;
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Version: 1.0
;;
;;; Commentary:
;; This is my init file.
;;
;;; Code:
;;
(setq custom-file "~/.cache/emacs/custom.el")
(setq inhibit-startup-screen t)
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(setq user-emacs-directory "~/.cache/emacs")
(setq make-backup-files nil)
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
(scroll-bar-mode -1); Disable visible scrollbar
(tool-bar-mode -1); Disable the toolbar
(tooltip-mode -1); Disable tooltips
(set-fringe-mode 10); Give some breathing room
(setq create-lockfiles nil)
(setq disabled-command-function 'ignore)
(setq confirm-kill-emacs 'y-or-n-p)
(setq-default require-final-newline t)

(electric-pair-mode 1)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; add local site packages
(add-to-list 'load-path (expand-file-name "~/.config/emacs/site-lisp/"))
(let ((default-directory (expand-file-name "~/.config/emacs/site-lisp/")))
  (normal-top-level-add-subdirs-to-load-path))
(eval-after-load 'flycheck
  '(add-to-list 'flycheck-emacs-lisp-load-path (expand-file-name "~/.config/emacs/site-lisp/")))

(require 'insert-random-uuid-into-buffer)
(require 'insert-port-number-for-directory-into-buffer)

;; Set up modifier key, macport/railwaycat specifics for macOS
(when (boundp 'mac-carbon-version-string)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super))

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(when (< emacs-major-version 27)
  (package-initialize))

;; Refresh package contents if needed
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'counsel)
  (package-refresh-contents)
  (package-install 'counsel))

;; Install use-package if it's not already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Configure use-package
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer nil)
(setq use-package-verbose t)
(setq use-package-compute-statistics t)

;; Configure auto-package-update
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-interval 7)
  (setq auto-package-update-prompt-before-update t)
  (auto-package-update-maybe))

;; Use no-littering to keep .emacs.d clean
(use-package no-littering)

(use-package alert
  :ensure t
  :config
  (setq alert-default-style 'osx-notifier))

;; Set package user directory
(setq package-user-dir "~/.cache/emacs/packages")
(use-package exec-path-from-shell
  :ensure t
  :config
  (dolist (var '("SSH_AUTH_SOCK" "LANG" "LANGUAGE" "LC_CTYPE" "LC_TIME" "PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (add-to-list 'exec-path "/opt/homebrew/bin" t)
  (when (daemonp)
    (exec-path-from-shell-initialize))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
(add-to-list 'exec-path "/opt/homebrew/bin" t)

;; Set up the visible bell
(setq visible-bell t)
(global-auto-revert-mode)

;; Terminal
(use-package vterm
  :diminish
  :defer t
  :config
  (bind-key "C-c C-v" 'vterm-yank))

;; Line number display for most modes (not org nor terminals)
(add-hook 'prog-mode-hook (lambda ()
			    (display-line-numbers-mode 1)
			    (column-number-mode)))

;; Appearance
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(unless (package-installed-p 'doom-themes)
  (package-refresh-contents)
  (package-install 'doom-themes)
  (load-theme 'doom-one t))

(use-package all-the-icons
  :init
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(load-theme 'doom-tomorrow-night t)
(set-face-attribute 'default nil :font "Fira Code Retina-16")
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina-16")
(set-face-attribute 'variable-pitch nil :font "Cantarell-16" :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package command-log-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . "counsel-minibuffer-history"))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
;; backslash for jis style keyboard8
(defun insert-backslash ()
  "Insert back-slash."
  (interactive)
  (insert "\\"))

;; strip trailing whitespace on save in certain modes
(defun my-delete-trailing-whitespace ()
  "Delete trailing whitepace in programing modes only."
  (when (or (derived-mode-p 'prog-mode)
	    (derived-mode-p 'sql-mode))
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my-delete-trailing-whitespace)

(global-set-key (kbd "M-¥") 'insert-backslash)

(load-file "~/.local/config/emacs/config.el")

;; copy/paste stuff - keep killring separate from system pasteboardA
(use-package simpleclip)
(simpleclip-mode 1)

;; Use rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(setq evil-want-keybinding nil)

(defun tao/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  vterm-mode
		  term-mode
		  git-rebase-mode
		  erc-mode
		  circe-server-mode
		  circe-chat-mode
		  circe-query-mode
		  sauron-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)

  ;; overrides universal argument in favor of C-u/C-v for scrolling
  (setq evil-want-C-u-scroll t)

  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-keybinding nil)
  :hook (evil-mode . tao/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; help will just be from normal mode
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package eradio
  :ensure t
  :init
  (setq eradio-player '("mpv" "--no-video" "--no-terminal"))
  :config
  (setq eradio-channels '(("Totally 80s FM" . "https://stream.zeno.fm/4r73usts108uv")
                           ("NDR DE" . "https://www.ndr.de/resources/metadaten/audio/aac/ndrblue.m3u")
                           ("JPOP" . "https://cast1.torontocast.com:2170/")
                           ("J1Extra" . "https://www.internet-radio.com/servers/tools/playlistgenerator/?u=http://jenny.torontocast.com:8058/listen.pls?sid=1&t=.m3u"))))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(setq magit-define-global-key-bindings 'recommended)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy)
	   (projectile-globally-ignored-directories
	    '("^\\.idea$"
	      "^\\.vscode$"
	      "^\\.ensime_cache$"
	      "^\\.eunit$"
	      "^\\.git$"
	      "^\\.hg$"
	      "^\\.fslckout$"
	      "^_FOSSIL_$"
	      "^\\.bzr$"
	      "^_darcs$"
	      "^\\.pijul$"
	      "^\\.tox$"
	      "^\\.svn$"
	      "^\\.stack-work$"
	      "^\\.ccls-cache$"
	      "^\\.cache$"
	      "^\\.clangd$"
	      "^\\.sl$"
	      "^\\.jj$"
	      "^target$")
	    ))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects" "~/OpenProjects" "~/.config")))
  (setq
    projectile-enable-caching t
    projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package general
  :config
  (general-create-definer tao/leader-keys
    :keymaps '(normal)
    :prefix "SPC"
    :global-prefix "C-SPC"
    :override t)
  (general-def universal-argument-map
    "SPC u" 'universal-argument-more)
  (tao/leader-keys
    "cc" 'comment-or-uncomment-region
    "bd" 'kill-this-buffer
    "bi" 'ibuffer-list-buffers
    "fd" '(init-file :which-key "init file")
    "ff" '(find-file :which-key "find file")
    "fs" 'save-buffer
    "k"  'switch-to-buffer
    "ps" 'org-pomodoro
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "xh" 'mark-whole-buffer
    ;; magit
    "g"   '(:ignore t :which-key "git")
    "gs"  'magit-status
    "gd"  'magit-diff-untaged
    "gc"  'magit-branch-or-checkout
    "gl"  '(:ignore t :which-key "log")
    "glc" 'magit-log-current
    "glf" 'magit-log-buffer-file
    "gb"  'magit-branch
    "gP"  'magit-push-current
    "gp"  'magit-pull-branch
    "gf"  'magit-fetch
    "gF"  'magit-fetch-all
    "gr"  'magit-rebase
    ;; projectile
    "pd" 'counsel-projectile-find-dir
    "pf" 'counsel-projectile-find-file
    "pp" 'counsel-projectile-switch-project))

(general-define-key
 "C-s" 'counsel-grep-or-swiper
 "C-M-j" 'counsel-switch-buffer)

(use-package hydra)

(defhydra hydra-radio (:timeout 4)
  "eradio"
   ("p" eradio-play "Eradio play" :exit t)
   ("s" eradio-stop "Eradio stop" :exit t)
   ("t" eradio-toggle "Eradio toggle" :exit t))

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(tao/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text")
  "r" '(hydra-radio/body :which-key "eradio"))

(defun tao/org-font-setup ()
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Make sure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun tao/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?' ".")
  (setq org-clock-sound (getenv "POMODORO_BREAK_SOUNDFILE"))
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . tao/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t
	org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages 'org-babel-load-languages
    '(
        (shell . t)
    ))
  (tao/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1)(match-end 1) "○"))))))

(defun tao/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . tao/org-mode-visual-fill))

(use-package org-pomodoro
  :after org)
;;;; load local version which includes display of pomodoro count
(require 'org-pomodoro)

(use-package pomidor
  :bind (("<f12>" . pomidor))
  :config (setq pomidor-sound-tick nil
                pomidor-sound-tack nil)
  :hook (pomidor-mode . (lambda ()
                          (display-line-numbers-mode -1) ; Emacs 26.1+
                          (setq left-fringe-width 0 right-fringe-width 0)
                          (setq left-margin-width 2 right-margin-width 0)
                          (setq pomidor-play-sound-file
                            (lambda (file)
                              (start-process "my-pomidor-play-sound"
                                nil
                                "afplay"
                                file)))
                          ;; force fringe update
                          (set-window-buffer nil (current-buffer)))))

;; LSP Mode for both Rust and TypeScript
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  ;; Rust-specific settings
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  ;; General settings
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-inlay-hint-enable t)
  :config
  (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'
  (lsp-enable-which-key-integration t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; LSP UI for enhanced UI features
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom))

;; TypeScript mode with LSP support
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; LSP Treemacs for project management
(use-package lsp-treemacs
  :after lsp)

;; DAP Mode for debugging support
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-ui-mode)
  (dap-tooltip-mode)
  (require 'dap-node) ;; For Node.js debugging
  (dap-node-setup))

;; Company mode for auto-completion
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("TAB" . company-complete-selection))
  (:map lsp-mode-map
        ("TAB" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; Optional: JS2 Mode for JavaScript support
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :hook (js2-mode . lsp-deferred)
  :config
  (setq js2-basic-offset 2))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

(use-package lsp-treemacs
  :after (treemacs lsp))

(use-package rustic
  :bind (:map rustic-mode-map
	      ("<f6>" . rustic-format-buffer)
	      ("M-j" . lsp-ui-imenu)
	      ("M-?" . lsp-find-references)
	      ("C-c C-c l" . flycheck-list-errors)
	      ("C-c C-c a" . lsp-execute-code-action)
	      ("C-c C-c r" . lsp-rename)
	      ("C-c C-c q" . lsp-workspace-restart)
	      ("C-c C-c Q" . lsp-workspace-shutdown)
	      ("C-c C-c s" . lsp-rust-analyzer-status))
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  (rustic-rustfmt-config-alist '((edition . "2021")))
  :config
  (setq rustic-format-on-save t)
  (setq lsp-rust-analyzer-completion-add-call-parenthesis nil))

(use-package cargo)
(use-package flycheck
  :init (global-flycheck-mode))
(use-package flycheck-rust)

(use-package yasnippet
  :defer 1
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.config/emacs/snippets")))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package evil-string-inflection :ensure t)

(use-package org-autolist
  :hook (org-mode . org-autolist-mode))

(use-package shfmt
  :hook (sh-mode . shfmt-on-save-mode))

(use-package browse-url-dwim)

(use-package expand-region
  :config
  (bind-key "C-=" 'er/expand-region))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package session
  :config (add-hook 'after-init-hook 'session-initialize))

(use-package yaml-mode
  :ensure t
  :mode ("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (setq yaml-indent-offset 2)
              (setq indent-tabs-mode nil)
              (setq-local fill-column 100)
              (setq-local whitespace-line-column 100)
              (setq-local whitespace-style '(face lines-tail))
              (whitespace-mode 1))))

(use-package sqlformat
  :ensure t
  :config
  (setq sqlformat-command 'pgformatter)
  ;; Optionally, set the path to pg_format if it's not in your PATH
  ;; (setq sqlformat-pgformatter-command "/path/to/pg_format")
  :hook (sql-mode . (lambda ()
                      (local-set-key (kbd "C-c C-f") 'sqlformat))))

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))

(custom-set-variables
  '(markdown-command (substring (shell-command-to-string "which pandoc") 0 -1)))

(provide 'init)
;;; init.el ends here
