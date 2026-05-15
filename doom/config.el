;;; $DOOMDIR/config.el -*- lexical-binding: t; no-byte-compile: t; -*-

(set-language-environment "UTF-8")

(add-to-list 'load-path "~/.config/elisp")

(setq epg-pinentry-mode 'loopback)

(load "~/.emacs_private.el" t)

(require 'project)
(require 'cl-lib)

;; Unicode fallback fonts (modern replacement for Symbola)
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
     (message "⚠️ No supported package manager found for Nerd Font install"))))

(defun tao/font-installed-p (font-name)
  "Return t if FONT-NAME is installed."
  (find-font (font-spec :family font-name)))

(defun tao/ensure-doom-fonts ()
  "Ensure Doom-required fonts are installed."
  (when (display-graphic-p)
    ;; Main Doom font (Fira Code Nerd Font - must match doom-font and nerd-icons)
    (unless (tao/font-installed-p "FiraCode Nerd Font")
      (message "🔤 Installing Fira Code Nerd Font…")
      (tao/install-nerd-font))

    ;; Icon fonts (nerd-icons for Doom v3)
    (when (featurep 'nerd-icons)
      (unless (tao/font-installed-p "FiraCode Nerd Font")
        (message "🎨 Nerd icons use Fira Code Nerd Font; install it if icons look wrong."))
      (when (and (require 'nerd-icons nil t)
              (fboundp 'nerd-icons-install-fonts)
              (not (tao/font-installed-p "Symbols Nerd Font")))
        (nerd-icons-install-fonts t)))))

(add-hook 'doom-after-init-hook #'tao/ensure-doom-fonts)

(setq doom-symbol-font (font-spec :family "Symbols Nerd Font Mono"))

;; Set nerd-icons vars *before* nerd-icons is ever used so the default face is valid
(setq nerd-icons-font-family "Symbols Nerd Font Mono")
;; Slightly > 1.0 gives icons room so they don’t clip or look squashed
(setq nerd-icons-scale-factor 1.15)
(setq doom-modeline-vcs-max-length 50)

;; Load nerd-icons early so nerd-icons-default-face exists before dashboard/modeline run
(when (display-graphic-p)
  (require 'nerd-icons nil t))

(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 16 :weight 'medium)
  doom-variable-pitch-font (font-spec :family "Fira Sans" :size 16)
  doom-big-font (font-spec :family "Fira Sans" :size 24))

(after! doom-themes
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic)
  ;; Dashboard menu icons use this face; give height and no slant to avoid clipping
  '(doom-dashboard-menu-title :height 1.2 :slant normal :inherit default))

;; Customize nerd-icons face: no slant, enough height/width to avoid clipping
(after! nerd-icons
  (when (facep 'nerd-icons-default-face)
    (set-face-attribute 'nerd-icons-default-face nil
      :family "FiraCode Nerd Font"
      :height 1.2
      :slant 'normal
      :weight 'regular
      :width 'normal
      :inherit nil)))

;; Dashboard: extra line spacing so icon lines don’t get clipped by the next line
(after! doom-dashboard
  (add-hook '+doom-dashboard-mode-hook
    (defun tao/doom-dashboard-line-spacing ()
      (setq-local line-spacing 0.35))
    nil t))

;; Theme
(setq doom-theme 'doom-palenight)

(setq display-line-numbers-type 'relative)

(setq-default
  delete-by-moving-to-trash t
  tab-width 2
  window-combination-resize t
  x-stretch-cursor t)

(global-subword-mode 1)

(after! projectile
  (setq projectile-mode-line nil)
  (projectile-mode -1))

(use-package! exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package! ws-butler
  :hook ((text-mode . ws-butler-mode)
          (prog-mode . ws-butler-mode)))

(use-package! apheleia
  :config
  (apheleia-global-mode +1))

(use-package! meow
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
    meow-use-clipboard t)

  (meow-motion-overwrite-define-key
    '("j" . meow-next)
    '("k" . meow-prev)
    '("<escape>" . ignore))

  (meow-leader-define-key
    '("/" . consult-ripgrep)
    '("?" . helpful-at-point)
    '("b" . switch-to-buffer)
    '("f" . find-file)
    '("p" . project-find-file)
    '("g" . magit-status)
    '("k" . kill-current-buffer))

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
    '("a" . meow-append)
    '("b" . meow-back-word)
    '("c" . meow-change)
    '("d" . meow-delete)
    '("e" . meow-next-word)
    '("f" . meow-find)
    '("g" . meow-cancel-selection)
    '("h" . meow-left)
    '("i" . meow-insert)
    '("j" . meow-next)
    '("k" . meow-prev)
    '("l" . meow-right)
    '("m" . meow-join)
    '("n" . meow-search)
    '("o" . meow-block)
    '("p" . meow-yank)
    '("q" . meow-quit)
    '("r" . meow-replace)
    '("s" . meow-kill)
    '("t" . meow-till)
    '("u" . undo-fu-only-undo)
    '("U" . undo-fu-only-redo)
    '("v" . meow-visit)
    '("w" . meow-mark-word)
    '("x" . meow-line)
    '("y" . meow-save)
    '("z" . meow-pop-selection))

  (meow-global-mode 1))

(defun tao/meow-disable-in-special-buffers ()
  (when (derived-mode-p
          'magit-mode
          'magit-status-mode
          'term-mode
          'vterm-mode
          'shell-mode
          'eshell-mode
          'help-mode
          'special-mode)
    (meow-insert-mode 1)
    (setq-local cursor-type 'bar)))

(add-hook 'after-change-major-mode-hook
  #'tao/meow-disable-in-special-buffers)

(after! magit
  (setq magit-display-buffer-function
    #'magit-display-buffer-same-window-except-diff-v1)

  (remove-hook 'magit-mode-hook #'meow-normal-mode)

  (add-hook 'magit-mode-hook
    (lambda ()
      (meow-insert-mode 1)
      (setq-local cursor-type 'bar))))

(after! org
  (setq org-directory "~/org/"
    org-hide-emphasis-markers t
    org-ellipsis " ▼ "
    org-log-done 'time
    org-startup-indented t
    org-adapt-indentation nil
    org-src-fontify-natively t
    org-src-tab-acts-natively t)

  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'variable-pitch-mode))

(use-package! org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package! treesit-auto
  :config
  (global-treesit-auto-mode))

(setq backup-directory-alist
  `(("." . ,(expand-file-name "backups/" doom-cache-dir))))

(setq auto-save-default t)

(setq confirm-kill-emacs nil)
