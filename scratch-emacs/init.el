(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(which-key-mode 1)
(which-key-setup-side-window-right-bottom)

(menu-bar-mode -1)          ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 140)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmodek.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package command-log-mode)

(use-package ivy
  :demand t
  :diminish ivy-mode
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
  :config (ivy-mode 1))

;; NOTE: The first time you load your configurations on a new machine, run
;; the following command interactively if the automatic function
;; 'check-install-all-the-icons-fonts' fails somehow (like on Windows):
;;
;; M-x all-the-icons-install-fonts
;;
;; This is needed so that mode line icons display correctly.
(defun check-install-all-the-icons-fonts ()
  "Install all-the-icons fonts if they are not already installed."
  (let* ((font-dir (if (eq system-type 'darwin)
                       (concat (getenv "HOME") "/Library/Fonts/")
                     (concat (getenv "HOME") "/.local/share/fonts/")))
         (font-file (concat font-dir "all-the-icons.ttf")))
    (unless (file-exists-p font-file)
      (when (y-or-n-p "All-the-icons fonts are missing. Install them now? ")
        (all-the-icons-install-fonts t)))))

(use-package all-the-icons
  :if (display-graphic-p)
  :init
  (check-install-all-the-icons-fonts))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (nerd-icons must be installed)
					;(doom-themes-neotree-config)

  ;; or for treemacs users
					;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  ;(doom-themes-org-config)
  )
  
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))  ;; Don't start searches within ^

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap desribe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(alert all-the-icons auto-package-update command-log-mode counsel
	   doom-modeline doom-themes flycheck helpful ivy-rich
	   no-littering rainbow-delimiters)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
