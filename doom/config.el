;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(set-language-environment "UTF-8")

(add-to-list 'load-path "~/.config/elisp")

(setq epg-pinentry-mode 'loopback)

(load "~/.emacs_private.el" t)

(add-hook! prog-mode #'turn-on-font-lock)

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
    (unless (tao/font-installed-p "FiraCode Nerd Font")
      (message "🔤 Installing Fira Code Nerd Font…")
      (tao/install-nerd-font))

    (when (featurep 'nerd-icons)
      (unless (tao/font-installed-p "FiraCode Nerd Font")
        (message "🎨 Nerd icons use Fira Code Nerd Font; install it if icons look wrong."))
      (when (and (require 'nerd-icons nil t)
              (fboundp 'nerd-icons-install-fonts)
              (not (tao/font-installed-p "Symbols Nerd Font")))
        (nerd-icons-install-fonts t)))))

(add-hook 'doom-after-init-hook #'tao/ensure-doom-fonts)

(setq doom-symbol-font (font-spec :family "Symbols Nerd Font Mono"))

(setq nerd-icons-font-family "Symbols Nerd Font Mono")
(setq nerd-icons-scale-factor 1.15)
(setq doom-modeline-vcs-max-length 50)

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
  '(doom-dashboard-menu-title :height 1.2 :slant normal :inherit default))

(after! nerd-icons
  (when (facep 'nerd-icons-default-face)
    (set-face-attribute 'nerd-icons-default-face nil
      :family "FiraCode Nerd Font"
      :height 1.2
      :slant 'normal
      :weight 'regular
      :width 'normal
      :inherit nil)))

(setq doom-theme 'doom-palenight)

(set-file-template! "/\\.config/elisp/.*\\.el$" :trigger "__package.el" :mode 'emacs-lisp-mode)

(use-package! highlight-indent-guides
  :diminish
  :commands highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-display-first t)
  (line-spacing 0.1))

(setq display-line-numbers-type t)

(setq gc-cons-threshold (* 50 1000 1000))
(after! gcmh
  (setq gcmh-high-cons-threshold 67108864))

(setq delete-by-moving-to-trash t
  trash-directory "~/.Trash")

(defun tao/dired-open-all-files-in-directory ()
  "Open all regular files in the current Dired directory into buffers.
Only works when called from a Dired buffer."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (error "This command must be run from a Dired buffer"))
  (let ((files (directory-files (dired-current-directory) t "^[^.]" t)))
    (dolist (file files)
      (when (and (file-regular-p file)
              (not (file-symlink-p file)))
        (find-file-noselect file)))))
(map! :map dired-mode-map
  :n "o" #'tao/dired-open-all-files-in-directory)

(after! company
  (define-key company-mode-map (kbd "C-<tab>") 'company-complete)

  (setq company-idle-delay 1)

  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-limit 10)
  (setq company-dabbrev-ignore-buffers (lambda (buffer)
                                         (string-match-p "^#" (buffer-name buffer))))

  (define-key company-mode-map (kbd "C-<tab>") 'company-complete)

  (define-key company-active-map (kbd "M-n") 'company-select-next)
  (define-key company-active-map (kbd "M-p") 'company-select-previous)
  (define-key company-active-map (kbd "down") 'company-select-next)
  (define-key company-active-map (kbd "up") 'company-select-previous)

  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "<tab>") nil))

(after! rustic
  (setq rustic-format-on-save nil)
  (add-hook 'rustic-mode-hook
    (lambda ()
      (yas-minor-mode 1)
      (setq-local company-backends '((company-capf company-yasnippet))))))

(after! eglot
  (setq eglot-sync-connect 0)
  (setq eglot-autoshutdown t)
  (setq eglot-events-buffer-config '(:size 1000000 :format full))
  (add-to-list 'eglot-server-programs
    '(rustic-mode . ("rust-analyzer"
                      :initializationOptions
                      (:procMacro (:enable t)
                        :diagnostics (:enable nil)
                        :cargo (:watch (:enable nil))
                        :completion (:autoimport (:enable t)))))))

(after! yasnippet
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.config/yasnippets/"))
  (defun tao/snippet-keywords-from-description (desc)
    "Derive org-package keyword tags from DESC string."
    (let* ((keyword-map
             '(("org"        . "outlines")
                ("task"       . "outlines")
                ("outline"    . "outlines")
                ("slack"      . "convenience")
                ("status"     . "convenience")
                ("message"    . "convenience")
                ("report"     . "convenience")
                ("git"        . "tools")
                ("github"     . "tools")
                ("process"    . "tools")
                ("shell"      . "tools")
                ("script"     . "tools")
                ("generate"   . "convenience")
                ("parse"      . "lisp")
                ("macro"      . "lisp")
                ("elisp"      . "lisp")
                ("emacs"      . "convenience")))
            (desc-lower (downcase (or desc "")))
            (matched
              (delete-dups
                (delq nil
                  (mapcar (lambda (pair)
                            (when (string-match-p (car pair) desc-lower)
                              (cdr pair)))
                    keyword-map)))))
      (if matched
        (mapconcat #'identity matched " ")
        "tools")))
  (add-to-list 'yas-snippet-dirs "~/.config/yasnippets/")
  (add-hook 'yas-minor-mode-hook
    (lambda ()
      (local-set-key (kbd "TAB") 'yas-expand)
      (local-set-key (kbd "<tab>") 'yas-expand))))

(after! vue
  (add-hook 'vue-mode-hook #'lsp!))

(defvar tao/treesit-grammars
  '((css        . ("https://github.com/tree-sitter/tree-sitter-css" "v0.25.0"))
     (bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
     (html       . ("https://github.com/tree-sitter/tree-sitter-html" "v0.23.2"))
     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.25.0" "src"))
     (json       . ("https://github.com/tree-sitter/tree-sitter-json" "v0.24.8"))
     (python     . ("https://github.com/tree-sitter/tree-sitter-python" "v0.25.0"))
     (go         . ("https://github.com/tree-sitter/tree-sitter-go" "v0.25.0"))
     (markdown   . ("https://github.com/ikatyang/tree-sitter-markdown"))
     (make       . ("https://github.com/alemuller/tree-sitter-make"))
     (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
     (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
     (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
     (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
     (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
     (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
     (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
     (prisma     . ("https://github.com/victorhqc/tree-sitter-prisma")))
  "Tree-sitter grammars to install. Bump a version string to trigger reinstall.")

(defvar tao/treesit-grammars-hash-file
  (expand-file-name "treesit-grammars.hash" doom-cache-dir)
  "File storing the hash of the last installed grammar list.")

(defun tao/treesit-grammars-hash ()
  "Return a hash string of the current grammar list."
  (md5 (format "%S" tao/treesit-grammars)))

(defun tao/treesit-grammars-changed-p ()
  "Return t if the grammar list has changed since last install."
  (let ((current-hash (tao/treesit-grammars-hash))
         (stored-hash
           (when (file-exists-p tao/treesit-grammars-hash-file)
             (with-temp-buffer
               (insert-file-contents tao/treesit-grammars-hash-file)
               (string-trim (buffer-string))))))
    (not (string= current-hash stored-hash))))

(defun tao/treesit-save-grammars-hash ()
  "Save the current grammar list hash to disk."
  (with-temp-file tao/treesit-grammars-hash-file
    (insert (tao/treesit-grammars-hash))))

(defun tao/setup-install-grammars ()
  "Install or reinstall Tree-sitter grammars.
Grammars are (re)installed when:
  - the compiled library is missing, or
  - the grammar list has changed since the last install (e.g. a version bump).
Call interactively to force reinstall of all grammars."
  (interactive)
  (let ((changed (or (called-interactively-p 'any)
                   (tao/treesit-grammars-changed-p))))
    (dolist (grammar tao/treesit-grammars)
      (add-to-list 'treesit-language-source-alist grammar)
      (when (or changed
              (not (treesit-language-available-p (car grammar))))
        (message "treesit: installing grammar for %s" (car grammar))
        (treesit-install-language-grammar (car grammar))))
    (when changed
      (tao/treesit-save-grammars-hash)
      (message "treesit: grammars updated"))))

(after! treesit
  (dolist (mapping
            '((python-mode     . python-ts-mode)
               (css-mode        . css-ts-mode)
               (typescript-mode . typescript-ts-mode)
               (js-mode         . typescript-ts-mode)
               (js2-mode        . typescript-ts-mode)
               (c-mode          . c-ts-mode)
               (c++-mode        . c++-ts-mode)
               (c-or-c++-mode   . c-or-c++-ts-mode)
               (bash-mode       . bash-ts-mode)
               (json-mode       . json-ts-mode)
               (js-json-mode    . json-ts-mode)
               (sh-mode         . bash-ts-mode)
               (sh-base-mode    . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  (tao/setup-install-grammars))

(use-package! expand-region
  :commands er/expand-region)

(use-package! expreg
  :init
  (add-to-list 'load-path
    (expand-file-name ".local/straight/repos/expreg" user-emacs-directory))
  :commands (expreg-expand expreg-contract))

(defun tao/expand-region ()
  (interactive)
  (if (treesit-parser-list)
    (expreg-expand)
    (er/expand-region 1)))

(defun tao/contract-region ()
  (interactive)
  (if (treesit-parser-list)
    (expreg-contract)
    (er/contract-region 1)))

(after! hydra
  (defhydra tao/hydra-expand-region (:hint nil)
    "
^Expand Region^
^──────────────^
_+_: expand
_-_: contract
_q_: quit
"
    ("+" #'tao/expand-region)
    ("-" #'tao/contract-region)
    ("q" nil :exit t))

  (defun tao/expand-region-hydra ()
    (interactive)
    (tao/expand-region)
    (tao/hydra-expand-region/body))

  (map! :n "C-c e" #'tao/expand-region
    :n "C-c E" #'tao/contract-region
    :leader
    :n "+" #'tao/expand-region-hydra
    :n "-" #'tao/hydra-expand-region/body))

(use-package! lsp-mode
  :diminish "LSP"
  :hook ((lsp-mode . lsp-diagnostics-mode)
          (lsp-mode . lsp-enable-which-key-integration)
          ((tsx-ts-mode
             typescript-ts-mode
             js-ts-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :none)
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.5)
  (lsp-enable-xref t)
  (lsp-auto-configure t)
  (lsp-eldoc-enable-hover t)
  (lsp-enable-dap-auto-configure t)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)
  (lsp-enable-links nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-suggest-server-download t)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-text-document-color nil)

  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-diagnostic-max-lines 20)
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t)
  (lsp-enable-snippet t)
  (lsp-completion-show-kind t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-signature-doc-lines 1)
  (lsp-ui-doc-use-childframe t)
  (lsp-eldoc-render-all nil)
  (lsp-lens-enable nil)
  (lsp-semantic-tokens-enable nil)

  :init
  (setq lsp-use-plists t))

(use-package! lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode)))

(use-package! lsp-ui
  :commands
  (lsp-ui-doc-show
    lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
          ("C-c C-d" . 'lsp-ui-doc-glance))
  :after (lsp-mode evil)
  :config (setq lsp-ui-doc-enable t
            evil-lookup-func #'lsp-ui-doc-glance
            lsp-ui-doc-show-with-cursor nil
            lsp-ui-doc-include-signature t
            lsp-ui-doc-position 'at-point))

(use-package! typescript-ts-mode
  :hook
  ((typescript-ts-mode . lsp)
    (tsx-ts-mode . lsp)))

(cl-defmethod project-root ((project (head eglot-project)))
  (cdr project))

(after! eglot
  (defun tao/project-try-tsconfig-json (dir)
    (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
      (cons 'eglot-project found)))
  (add-hook 'project-find-functions 'tao/project-try-tsconfig-json nil nil)
  (add-to-list 'eglot-server-programs
    '((typescript-mode) "typescript-language-server" "--stdio")))

(defun insert-backslash ()
  "Insert backslash"
  (interactive)
  (insert "\\"))
(global-set-key (kbd "M-¥") 'insert-backslash)

(defun insert-blank-line-after-comment ()
  "Insert a blank line after the current line without continuing a comment."
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "C-<return>") 'insert-blank-line-after-comment)

(defun url-decode-region (start end)
  "Replace a region with the same contents, only URL decoded."
  (interactive "r")
  (let ((text (decode-coding-string (url-unhex-string (buffer-substring start end) t) 'utf-8)))
    (delete-region start end)
    (insert text)))

(defun url-encode-region (start end)
  "Replace a region with the same contents, only URL encoded."
  (interactive "r")
  (let ((text (url-hexify-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

(after! evil
  (evil-set-initial-state 'tetris-mode 'emacs)
  (add-hook 'evil-insert-state-entry-hook #'company-mode))

(after! project
  (add-to-list 'project-vc-extra-root-markers ".git"))

(after! projectile
  (let* ((projects-path "~/Projects")
          (open-projects-path (getenv "OPENPROJECTS_PATH"))
          (paths (delq nil (list projects-path
                             (unless (string= projects-path open-projects-path)
                               open-projects-path)
                             (when (file-directory-p "~/dev") "~/dev")))))
    (dolist (path paths)
      (let ((entry (cons path 2)))
        (unless (assoc path projectile-project-search-path)
          (add-to-list 'projectile-project-search-path entry)))))
  (add-to-list 'projectile-project-search-path (cons "~/.config" 1)))

(use-package! exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-arguments '("-l"))
    (setq exec-path-from-shell-variables '("PATH"
                                            "MISE_SHELL"
                                            "DEFAULT_GIT_COMMIT_MESSAGE"
                                            "GITHUB_PULL_REQUEST_REVIEWERS"
                                            "JIRA_USER"
                                            "JIRA_TOKEN"
                                            "JIRA_ISSUE_BASE_URL"
                                            "JIRA_ISSUE_KEY_PREFIX"))
    (exec-path-from-shell-initialize)
    (dolist (var exec-path-from-shell-variables)
      (ignore-errors (exec-path-from-shell-copy-env var)))))

(defun tao/get-relative-base-directory (file-path)
  "Determine the base directory for FILE-PATH based on custom fallback rules."
  (cond
    ((and (fboundp 'git-tools--project-root)
       (git-tools--project-root)))
    ((and (boundp 'org-directory)
       org-directory
       (string-prefix-p (expand-file-name org-directory)
         (expand-file-name file-path)))
      (expand-file-name org-directory))
    (t default-directory)))

(defun tao/dired-copy-relative-path ()
  "Copy the current Dired file path relative to a calculated project/org base.
If the point is not on a file, copies the relative path of the current directory."
  (interactive)
  (let* ((filename (or (dired-get-filename nil t)
                     (dired-current-directory)))
          (base (tao/get-relative-base-directory filename))
          (rel-path (file-relative-name filename base)))
    (kill-new rel-path)
    (message "Copied relative path: %s (Base: %s)" rel-path base)))

(defun tao/dired-copy-full-path ()
  "Copy the full path of the current Dired file.
If the point is not on a file, copies the full path of the current directory."
  (interactive)
  (let ((filename (or (dired-get-filename nil t)
                    (dired-current-directory))))
    (kill-new filename)
    (message "Copied full path: %s" filename)))

(defun tao/copy-buffer-relative-path ()
  "Copy the current buffer file path relative to a calculated project/org base."
  (interactive)
  (if-let ((file (buffer-file-name)))
    (let* ((base (tao/get-relative-base-directory file))
            (rel-path (file-relative-name file base)))
      (kill-new rel-path)
      (message "Copied relative path: %s (Base: %s)" rel-path base))
    (user-error "Current buffer is not visiting a file.")))

(defun tao/copy-buffer-full-path ()
  "Copy the current buffer file full path."
  (interactive)
  (if (buffer-file-name)
    (progn
      (kill-new (buffer-file-name))
      (message "Copied full path: %s" (current-kill 0)))
    (user-error "Current buffer is not visiting a file.")))

(after! general
  (general-define-key
    :states 'normal
    :keymaps 'override
    :prefix doom-leader-key
    "<escape>" '(buffer-menu :which-key "buffer menu"))
  (general-define-key
    :states 'normal
    :keymaps 'override
    :prefix doom-leader-key
    :which-key "string inflection"
    "z" '(:which-key "string inflection")
    "z a" '(string-inflection-all-cycle :which-key "all cases")
    "z c" '(string-inflection-camelcase :which-key "camelCase")
    "z k" '(string-inflection-kebab-case :which-key "kebab-case")
    "z l" '(string-inflection-lower-camelcase :which-key "lowerCamelCase")
    "z p" '(string-inflection-upper-camelcase :which-key "UpperCamelCase")
    "z s" '(string-inflection-underscore :which-key "snake_case")
    "z u" '(string-inflection-upcase :which-key "UPCASE"))
  (general-define-key
    :states 'normal
    :keymaps 'override
    :prefix doom-leader-key
    "m i u" '(markdown-tools-insert-human-url :which-key "insert human URL"))
  (general-define-key
    :keymaps 'magit-status-mode-map
    :states 'normal
    "z l" '(+magit-toggle-local-branches-section :which-key "toggle local branches")))

(after! which-key
  (setq which-key-use-C-h-commands t)
  (setq which-key-show-transient-maps t)
  (setq which-key-max-display-columns nil)
  (setq which-key-side-window-max-height 0.5))

(after! apheleia
  (setf (alist-get 'emacs-lisp-mode apheleia-mode-alist) 'lisp-indent)
  (setf (alist-get 'black apheleia-formatters) '("black" "-"))
  (setf (alist-get 'python-mode apheleia-mode-alist) 'black)
  (setf (alist-get 'go-mode apheleia-mode-alist) 'gofumpt)
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'gofumpt))

(add-hook! 'prog-mode #'tao/conditionally-enable-apheleia)

(load! "config/actions")
(load! "config/version-control")
(load! "config/ai")
(load! "config/keybindings")
(load! "config/irc")
(load! "config/clipboard")
(load! "config/notes")
(load! "config/hooks")

(use-package! ws-butler
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (add-hook 'text-mode-hook #'ws-butler-mode)

  (add-hook 'makefile-mode-hook (lambda () (ws-butler-mode -1)))
  (add-hook 'markdown-mode-hook (lambda () (ws-butler-mode -1))))

(use-package! vterm
  :config
  (setq vterm-always-compile-module t)
  (define-key vterm-mode-map (kbd "<tab>") 'vterm-send-tab))

(use-package! autoinsert
  :config
  (setq auto-insert-query nil)
  (auto-insert-mode 1)
  (setq auto-insert-alist
    (assoc-delete-all '(org-mode . "Org file skeleton") auto-insert-alist))
  (defun tao/org-file-title ()
    "Convert buffer filename to a clean title-cased string."
    (let* ((base (file-name-base (buffer-file-name)))
            (spaced (replace-regexp-in-string "[[:punct:]]+" " " base))
            (trimmed (string-trim spaced))
            (words (split-string trimmed " " t)))
      (mapconcat #'capitalize words " ")))
  (define-auto-insert
    '(org-mode . "Org file skeleton")
    '(""
       "#+TITLE: " (tao/org-file-title) "\n"
       "#+CREATED: " (format-time-string "[%Y-%m-%d %a %H:%M]") "\n"
       "#+STARTUP: overview\n\n"
       (concat "* " (tao/org-file-title) "\n** Introduction\n"))))

(use-package! port-number)

(use-package! nodoze)

(use-package! colima)

(use-package! git-tools)

(use-package! yak)

(use-package! rust-clean)

(use-package! pg-tools)

(use-package! status)

(use-package! jira-todo
  :after request)

(use-package! markdown-tools)

(use-package! retimestamp)

(use-package! org-pomodoro)

(use-package! slackcount
  :if (slackcount-available-p)
  :config
  (setq slackcount-alert-sound "/System/Library/Sounds/Funk.aiff")
  (slackcount-mode 1))

(use-package! teamscount
  :if (teamscount-available-p)
  :config
  (setq teamscount-alert-sound "/System/Library/Sounds/Funk.aiff"
    teamscount-icon-fg-color "#7B83EB")
  (teamscount-mode 1))

;;; Flycheck
;; Doom config uses macros (after!, use-package!, …) that flycheck's standalone
;; emacs-lisp byte-compiler cannot evaluate. Use `doom sync` for real syntax checks.
(defun tao/flycheck-disable-in-doom-dir ()
  (when (and buffer-file-name
          (file-in-directory-p (expand-file-name doom-user-dir)
            (expand-file-name buffer-file-name)))
    (setq-local flycheck-disabled-checkers
      (append flycheck-disabled-checkers
        '(emacs-lisp emacs-lisp-checkdoc)))))

(add-hook! emacs-lisp-mode
  #'tao/flycheck-disable-in-doom-dir -90)

(add-hook! markdown-mode
  #'markdown-toggle-markup-hiding
  #'valign-mode)

(setq custom-file (expand-file-name "custom.el" doom-user-dir))
(when (file-exists-p custom-file)
  (load custom-file))
