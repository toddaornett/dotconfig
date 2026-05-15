;;; $DOOMDIR/config.el -*- lexical-binding: t; no-byte-compile: t; -*-
(set-language-environment "UTF-8")

(add-to-list 'load-path "~/.config/elisp")

(setq epg-pinentry-mode 'loopback)
(load "~/.emacs_private.el" t)

(require 'project)

;;; Fonts
(when (display-graphic-p)
  (dolist (font '("Noto Emoji" "Noto Sans Symbols 2"))
    (when (member font (font-family-list))
      (set-fontset-font t 'symbol (font-spec :family font) nil 'append))))

(defun tao/install-nerd-font ()
  "Install Fira Code Nerd Font using system package manager."
  (cond
   ((executable-find "brew")
    (start-process "brew-font" "*font-install*"
                   "brew" "install" "--cask" "font-fira-code-nerd-font"))
   ((executable-find "apt")
    (start-process "apt-font" "*font-install*"
                   "sudo" "apt" "install" "-y" "fonts-firacode"))
   ((executable-find "pacman")
    (start-process "pacman-font" "*font-install*"
                   "sudo" "pacman" "-S" "--noconfirm" "ttf-fira-code"))
   (t
    (message "No supported package manager found for Nerd Font install"))))

(defun tao/font-installed-p (font-name)
  "Return t if FONT-NAME is installed."
  (find-font (font-spec :family font-name)))

(defun tao/ensure-doom-fonts ()
  "Ensure Doom-required fonts are installed."
  (when (display-graphic-p)
    (unless (tao/font-installed-p "FiraCode Nerd Font")
      (message "Installing Fira Code Nerd Font...")
      (tao/install-nerd-font))
    (when (featurep 'nerd-icons)
      (unless (tao/font-installed-p "FiraCode Nerd Font")
        (message "Nerd icons use Fira Code Nerd Font; install it if icons look wrong."))
      (when (and (require 'nerd-icons nil t)
                 (fboundp 'nerd-icons-install-fonts)
                 (not (tao/font-installed-p "Symbols Nerd Font")))
        (nerd-icons-install-fonts t)))))

(add-hook 'doom-after-init-hook #'tao/ensure-doom-fonts)

(setq doom-symbol-font
      (font-spec :family "Symbols Nerd Font Mono")
      doom-font
      (font-spec :family "FiraCode Nerd Font" :size 16 :weight 'medium)
      doom-variable-pitch-font
      (font-spec :family "Fira Sans" :size 16)
      doom-big-font
      (font-spec :family "Fira Sans" :size 24))

(setq nerd-icons-font-family "Symbols Nerd Font Mono"
      nerd-icons-scale-factor 1.15)

(when (display-graphic-p)
  (require 'nerd-icons nil t))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic)
  '(doom-dashboard-menu-title :height 1.2 :slant normal :inherit default))

(use-package! nerd-icons
  :config
  (when (facep 'nerd-icons-default-face)
    (set-face-attribute 'nerd-icons-default-face nil
                        :family "FiraCode Nerd Font"
                        :height 1.2
                        :slant 'normal
                        :weight 'regular
                        :width 'normal
                        :inherit nil)))

(after! doom-dashboard
  (add-hook '+doom-dashboard-mode-hook
            (defun tao/doom-dashboard-line-spacing ()
              (setq-local line-spacing 0.35))
            nil t))

;;; Theme
(setq doom-theme 'doom-palenight)

;;; Templates
(set-file-template! "/\\.config/elisp/.*\\.el$"
  :trigger "__package.el"
  :mode 'emacs-lisp-mode)

;;; UI
(setq display-line-numbers-type t
      doom-modeline-vcs-max-length 50)

(use-package! highlight-indent-guides
  :diminish
  :commands highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-display-first t)
  (line-spacing 0.1))

;;; Core settings
(setq org-directory "~/Notes/"
      gc-cons-threshold (* 50 1000 1000)
      delete-by-moving-to-trash t
      trash-directory "~/.Trash"
      select-enable-clipboard nil)

(global-set-key (kbd "s-c") #'clipboard-kill-ring-save)
(global-set-key (kbd "s-v") #'clipboard-yank)
(define-key minibuffer-local-map (kbd "s-v") #'clipboard-yank)

;;; Keybindings
(defun insert-backslash ()
  "Insert backslash."
  (interactive)
  (insert "\\"))

(global-set-key (kbd "M-¥") #'insert-backslash)

(defun insert-blank-line-after-comment ()
  "Insert a blank line after the current line without continuing a comment."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "C-<return>") #'insert-blank-line-after-comment)

;;; URL encoding/decoding
(defun url-decode-region (start end)
  "Replace region with URL-decoded contents."
  (interactive "r")
  (let ((text (decode-coding-string
               (url-unhex-string (buffer-substring start end) t)
               'utf-8)))
    (delete-region start end)
    (insert text)))

(defun url-encode-region (start end)
  "Replace region with URL-encoded contents."
  (interactive "r")
  (let ((text (url-hexify-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

;;; Dired
(defun tao/dired-open-all-files-in-directory ()
  "Open all regular files in the current Dired directory into buffers."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (error "This command must be run from a Dired buffer"))
  (let ((files (directory-files (dired-current-directory) t "^[^.]" t)))
    (dolist (file files)
      (when (and (file-regular-p file)
                 (not (file-symlink-p file)))
        (find-file-noselect file)))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "o")
              #'tao/dired-open-all-files-in-directory))

;;; Company
(use-package! company
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-tooltip-limit 10
        company-dabbrev-ignore-buffers
        (lambda (buffer)
          (string-match-p "^#" (buffer-name buffer))))

  (define-key company-mode-map (kbd "C-<tab>")
              #'company-complete)

  (define-key company-active-map (kbd "M-n")
              #'company-select-next)

  (define-key company-active-map (kbd "M-p")
              #'company-select-previous)

  (define-key company-active-map (kbd "down")
              #'company-select-next)

  (define-key company-active-map (kbd "up")
              #'company-select-previous)

  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "<tab>") nil))

;;; Meow
(defun tao/meow-setup ()
  "Meow QWERTY layout."
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore)
   '(" " . ignore))

  (meow-leader-define-key
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("<escape>" . buffer-menu)
   '("g g" . magit-status)
   '("g /" . magit-dispatch))

  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '(" " . ignore)))

(defun tao/magit-meow-motion-hook ()
  "Use Meow motion state in Magit."
  (when (bound-and-true-p meow-mode)
    (meow-motion-mode)))

(use-package! meow
  :config
  (tao/meow-setup)

  (meow-global-mode 1)

  (when (fboundp 'meow-setup-indicator)
    (meow-setup-indicator))

  ;; Safe Magit motion setup (fixes meow-mode-state-list error)
  (dolist (hook '(magit-mode-hook
                  magit-status-mode-hook
                  magit-diff-mode-hook
                  magit-log-mode-hook
                  magit-process-mode-hook
                  magit-revision-mode-hook
                  magit-stash-mode-hook))
    (add-hook hook #'meow-motion-mode))

  (add-hook 'meow-insert-enter-hook #'company-mode))

;;; Rust / rustic
(use-package! rustic
  :config
  (setq rustic-lsp-client 'eglot
        rustic-format-on-save t)

  (add-hook 'rustic-mode-hook #'eglot-ensure)

  (add-hook 'rustic-mode-hook
            (lambda ()
              (yas-minor-mode 1)
              (setq-local company-backends
                          '((company-capf company-yasnippet))))))

;;; Eglot
(use-package! eglot
  :config
  (setq eglot-sync-connect 0
        eglot-autoshutdown t
        eglot-events-buffer-size 1000000))

;;; Yasnippet
(use-package! yasnippet
  :config
  (yas-global-mode 1)

  (setq yas-snippet-dirs nil)

  (add-to-list 'yas-snippet-dirs
               (expand-file-name "snippets/" doom-user-dir))

  (add-to-list 'yas-snippet-dirs
               "~/.config/yasnippets/")

  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (local-set-key (kbd "TAB") #'yas-expand)
              (local-set-key (kbd "<tab>") #'yas-expand))))

;;; Which-key
(use-package! which-key
  :config
  (setq which-key-use-C-h-commands t
        which-key-show-transient-maps t
        which-key-max-display-columns nil
        which-key-side-window-max-height 0.5))

;;; Magit
(use-package! magit
  :config
  (setq ediff-diff-options ""
        ediff-custom-diff-options "-u"
        magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)

  (add-hook 'magit-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)))

  (add-hook 'magit-mode-hook
            #'tao/magit-meow-motion-hook))

;;; Vterm
(use-package! vterm
  :init
  (setq vterm-always-compile-module nil)
  :config
  (define-key vterm-mode-map (kbd "<tab>")
              #'vterm-send-tab))

;;; ws-butler
(use-package! ws-butler
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (add-hook 'text-mode-hook #'ws-butler-mode))

;;; Local packages
(use-package! port-number)
(use-package! nodoze)
(use-package! colima)
(use-package! git-tools)
(use-package! pg-tools)
(use-package! status)
(use-package! markdown-tools)

(use-package! jira-todo
  :after request)

(use-package! slackcount
  :if (slackcount-available-p)
  :config
  (setq slackcount-alert-sound
        "/System/Library/Sounds/Funk.aiff")
  (slackcount-mode 1))

;;; Custom file
(setq custom-file
      (expand-file-name "custom.el" doom-private-dir))

(when (file-exists-p custom-file)
  (load custom-file))
