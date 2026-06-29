;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(set-language-environment "UTF-8")

(add-to-list 'load-path "~/.config/elisp")

(setq epg-pinentry-mode 'loopback)
(load "~/.emacs_private.el" t)

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

(setq org-directory "~/Notes/")

(setq gc-cons-threshold (* 50 1000 1000))
(after! gcmh
  (setq gcmh-high-cons-threshold 67108864))

(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash")

(setq select-enable-clipboard nil)
(map! :nvi
      "s-c" #'clipboard-kill-ring-save
      "s-v" #'clipboard-yank)
(define-key minibuffer-local-map (kbd "s-v") #'clipboard-yank)

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
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook #'eglot-ensure)
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

(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "DOING(p)"
           "REVIEW(r)"
           "BLOCKED(b)"
           "|" "DONE(d)" "CANCELED(c)"))
        org-log-done 'time
        org-todo-keyword-faces
        '(("TODO" . (:foreground "#008080" :weight bold))
          ("DOING" . (:foreground "#00ff00" :weight bold))
          ("BLOCKED" . (:foreground "#ff0000" :weight bold))
          ("REVIEW" . (:foreground "#00ffff" :weight bold))
          ("DONE" . (:foreground "#708090" :weight bold)))
        org-use-fast-todo-selection 'auto)

  (defun my/org-time-stamp-with-time ()
    "Insert an Org timestamp including time."
    (interactive)
    (org-time-stamp '(4)))

  (map! :map org-mode-map
        :localleader
        :desc "Set TODO state" "t" #'org-todo
        :desc "Insert timestamp with time" "T" #'my/org-time-stamp-with-time)

  (defun tao/org-prettify-symbols ()
    "Set up prettify symbols for Org buffers."
    (setq-local prettify-symbols-alist
                '(("[ ]" . "☐")
                  ("[X]" . "☑")
                  ("[-]" . "❍")))
    (prettify-symbols-mode 1))
  (add-hook 'org-mode-hook #'tao/org-prettify-symbols)

  (defface org-task-with-clock
    '((t :foreground "Cyan"))
    "Face for Org tasks with clock entries."
    :group 'org)

  (defun tao/org-has-clock-entries-p ()
    "Return non-nil if the current headline has clock entries."
    (save-excursion
      (org-back-to-heading t)
      (let ((end (org-entry-end-position)))
        (re-search-forward "^[ \t]*CLOCK:" end t))))

  (defun tao/org-fontify-clock-tasks ()
    "Fontify Org tasks with clock entries."
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (let* ((beg (match-beginning 0))
               (end (match-end 0))
               (text-beg (progn
                           (goto-char beg)
                           (skip-chars-forward "*[:space:]")
                           (when (looking-at org-todo-regexp)
                             (goto-char (match-end 0))
                             (skip-chars-forward "[:space:]"))
                           (point))))
          (when (tao/org-has-clock-entries-p)
            (add-text-properties text-beg end '(font-lock-face org-task-with-clock)))))))
  (add-hook 'org-mode-hook #'tao/org-fontify-clock-tasks)
  (add-hook 'org-agenda-finalize-hook #'tao/org-fontify-clock-tasks)

  (defun tao/org-pomodoro-start-or-finished-hook ()
    "Hook to run when org-pomodoro starts or finishes."
    (tao/org-fontify-clock-tasks))
  (add-hook 'org-pomodoro-started-hook #'tao/org-pomodoro-start-or-finished-hook)
  (add-hook 'org-pomodoro-finished-hook #'tao/org-pomodoro-start-or-finished-hook)

  (defun tao/org-update-last-timestamp ()
    "Update or insert the #+UPDATED: keyword with the current timestamp in
     Org mode files, placing it after #+CREATED: if it exists, or display
the last modified time for other files."
    (interactive)
    (if (buffer-file-name)
        (let ((timestamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (if (derived-mode-p 'org-mode)
              (save-excursion
                (goto-char (point-min))
                (if (re-search-forward "^#\\+UPDATED:.*$" nil t)
                    (replace-match (concat "#+UPDATED: " timestamp))
                  (goto-char (point-min))
                  (if (re-search-forward "^#\\+CREATED:.*$" nil t)
                      (progn
                        (end-of-line)
                        (insert "\n#+UPDATED: " timestamp))
                    (goto-char (point-min))
                    (if (re-search-forward "^#\\+.*$" nil t)
                        (progn
                          (end-of-line)
                          (insert "\n#+UPDATED: " timestamp))
                      (insert "#+UPDATED: " timestamp "\n")))))
            (message "Last modified: %s" timestamp)))
      (message "Buffer is not associated with a file")))
  (add-hook 'before-save-hook #'tao/org-update-last-timestamp)

  (defun tao/org-todo-state-is-terminal-p (state)
    "Return non-nil if STATE is a terminal keyword (DONE or CANCELED)."
    (member state '("DONE" "CANCELED")))

  (defun tao/org-sink-done-heading ()
    "Move the current heading to after the last non-terminal sibling at the
same level.  The entire subtree (body text + children) travels with it.
Runs via `org-after-todo-state-change-hook'."
    (when (tao/org-todo-state-is-terminal-p org-state)
      (save-excursion
        (org-back-to-heading t)
        (let* ((level        (org-current-level))
               (stars        (make-string level ?*))
               (subtree-beg  (point))
               (subtree-end  (save-excursion (org-end-of-subtree t t) (point)))
               (subtree-text (buffer-substring subtree-beg subtree-end))
               (insert-after  nil))

          (save-excursion
            (if (org-up-heading-safe)
                (org-goto-first-child)
              (goto-char (point-min))
              (unless (looking-at (concat "^" stars "[^*]"))
                (re-search-forward (concat "^" stars "[^*]") nil t)
                (beginning-of-line)))
            (while (and (looking-at org-heading-regexp)
                        (= (org-current-level) level))
              (let ((kw (org-get-todo-state)))
                (unless (tao/org-todo-state-is-terminal-p kw)
                  (setq insert-after
                        (save-excursion (org-end-of-subtree t t) (point)))))
              (unless (org-get-next-sibling)
                (goto-char (point-max)))))

          (when insert-after
            (unless (= subtree-beg insert-after)
              (let ((adjusted-insert
                     (if (< subtree-beg insert-after)
                         (- insert-after (- subtree-end subtree-beg))
                       insert-after)))
                (delete-region subtree-beg subtree-end)
                (goto-char adjusted-insert)
                (unless (bolp) (insert "\n"))
                (insert subtree-text)
                (goto-char adjusted-insert)
                (beginning-of-line))))))))

  (add-hook 'org-after-todo-state-change-hook #'tao/org-sink-done-heading))

(use-package! org-superstar
  :defer t
  :hook org-mode
  :config
  (setq org-superstar-headline-bullets-list '("✿" "✸" "⬢" "☯" "○" "◆" "▲" "■" "♦" "♢" "▫"))
  (setq org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?➤) (?- . ?–))))

(after! org-pomodoro
  (defcustom org-pomodoro-display-count-p t
    "When non-nil, display the total number of pomodoros in the modeline."
    :group 'org-pomodoro
    :type 'boolean)
  (defcustom org-pomodoro-count-format "[%s] "
    "The format of the total pomodoro count if enabled."
    :group 'org-pomodoro
    :type 'string)
  (defun org-pomodoro-format-count ()
    "Format the total number of pomodoros or empty string if not shown."
    (if (and org-pomodoro-display-count-p (> org-pomodoro-count 0))
        (format org-pomodoro-count-format org-pomodoro-count)
      ""))
  (defun org-pomodoro-update-mode-line ()
    "Set the modeline accordingly to the current state."
    (let ((s (cl-case org-pomodoro-state
               (:pomodoro
                (propertize org-pomodoro-format 'face 'org-pomodoro-mode-line))
               (:overtime
                (propertize org-pomodoro-overtime-format
                            'face 'org-pomodoro-mode-line-overtime))
               (:short-break
                (propertize org-pomodoro-short-break-format
                            'face 'org-pomodoro-mode-line-break))
               (:long-break
                (propertize org-pomodoro-long-break-format
                            'face 'org-pomodoro-mode-line-break)))))
      (setq org-pomodoro-mode-line
            (when (and (org-pomodoro-active-p) (> (length s) 0))
              (list "[" (format s (org-pomodoro-format-seconds)) "] " (org-pomodoro-format-count))))
      (force-mode-line-update t)))
  (defun tao/org-pomodoro-start-or-finished-hook ()
    "Hook to run when org-pomodoro starts or finishes."
    (tao/org-fontify-clock-tasks))
  (add-hook 'org-pomodoro-started-hook #'tao/org-pomodoro-start-or-finished-hook)
  (add-hook 'org-pomodoro-finished-hook #'tao/org-pomodoro-start-or-finished-hook))

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
  (setf (alist-get 'emacs-lisp-mode apheleia-formatters)
        '("emacs" "--eval" "(progn
                              (require 'emacs-lisp)
                              (indent-region (point-min) (point-max))
                              (untabify (point-min) (point-max))
                              (buffer-string))"))
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist)
        'prettier)
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist)
        'prettier)
  (setf (alist-get 'js-mode apheleia-mode-alist)
        'prettier)
  (setf (alist-get 'black apheleia-formatters)
        '("black" "-"))
  (setf (alist-get 'python-mode apheleia-mode-alist)
        'black))

(defun tao/conditionally-enable-apheleia ()
  (when (and (derived-mode-p 'prog-mode)
             (not (or (bound-and-true-p lsp-mode)
                      (bound-and-true-p eglot--managed-mode)
                      (member major-mode '(rust-mode rust-ts-mode
                                           yaml-mode yaml-ts-mode)))))
    (apheleia-mode-maybe)))

(add-hook 'prog-mode-hook #'tao/conditionally-enable-apheleia)

(load! "config/version-control")

(load! "config/ai")

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

(use-package! pg-tools)

(use-package! status)

(use-package! jira-todo
  :after request)

(use-package! markdown-tools)

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

(add-hook 'emacs-lisp-mode-hook #'tao/flycheck-disable-in-doom-dir -90)

(setq custom-file (expand-file-name "custom.el" doom-user-dir))
(when (file-exists-p custom-file)
  (load custom-file))
