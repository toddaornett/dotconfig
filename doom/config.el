;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(set-language-environment "UTF-8")

(add-to-list 'load-path "~/.config/elisp")

(setq epg-pinentry-mode 'loopback)
(load "~/.emacs_private.el" t)

;;; ── Fonts ────────────────────────────────────────────────────────────────────

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

(setq doom-symbol-font        (font-spec :family "Symbols Nerd Font Mono")
      doom-font               (font-spec :family "FiraCode Nerd Font" :size 16 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 16)
      doom-big-font            (font-spec :family "Fira Sans" :size 24))

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

;;; ── Theme ────────────────────────────────────────────────────────────────────

(setq doom-theme 'doom-palenight)

;;; ── Templates ────────────────────────────────────────────────────────────────

(set-file-template! "/\\.config/elisp/.*\\.el$"
  :trigger "__package.el"
  :mode 'emacs-lisp-mode)

;;; ── UI ───────────────────────────────────────────────────────────────────────

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

;;; ── Core settings ────────────────────────────────────────────────────────────

(setq org-directory "~/Notes/"
      gc-cons-threshold (* 50 1000 1000)
      delete-by-moving-to-trash t
      trash-directory "~/.Trash"
      select-enable-clipboard nil)

(map! :nvi
      "s-c" #'clipboard-kill-ring-save
      "s-v" #'clipboard-yank)
(define-key minibuffer-local-map (kbd "s-v") #'clipboard-yank)

;;; ── Keybindings ──────────────────────────────────────────────────────────────

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

;;; ── URL encoding/decoding ────────────────────────────────────────────────────

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

;;; ── Dired ────────────────────────────────────────────────────────────────────

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

(map! :map dired-mode-map
      :n "o" #'tao/dired-open-all-files-in-directory)

;;; ── Company ──────────────────────────────────────────────────────────────────

(use-package! company
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-tooltip-limit 10
        company-dabbrev-ignore-buffers
        (lambda (buffer)
          (string-match-p "^#" (buffer-name buffer))))

  (define-key company-mode-map   (kbd "C-<tab>")  #'company-complete)
  (define-key company-active-map (kbd "M-n")       #'company-select-next)
  (define-key company-active-map (kbd "M-p")       #'company-select-previous)
  (define-key company-active-map (kbd "down")      #'company-select-next)
  (define-key company-active-map (kbd "up")        #'company-select-previous)
  (define-key company-active-map (kbd "TAB")       nil)
  (define-key company-active-map (kbd "<tab>")     nil))

(use-package! evil
  :config
  (add-hook 'evil-insert-state-entry-hook #'company-mode))

;;; ── Rust / rustic ────────────────────────────────────────────────────────────

(use-package! rustic
  :config
  (setq rustic-lsp-client 'eglot
        rustic-format-on-save t)
  (add-hook 'rustic-mode-hook #'eglot-ensure)
  (add-hook 'rustic-mode-hook
            (lambda ()
              (yas-minor-mode 1)
              (setq-local company-backends '((company-capf company-yasnippet))))))

;;; ── Eglot ────────────────────────────────────────────────────────────────────

(use-package! eglot
  :config
  (setq eglot-sync-connect 0
        eglot-autoshutdown t
        eglot-events-buffer-size 1000000)

  (add-to-list 'eglot-server-programs
               '(rustic-mode . ("rust-analyzer"
                                :initializationOptions
                                (:procMacro (:enable t)
                                 :diagnostics (:enable nil)
                                 :cargo (:watch (:enable nil))
                                 :completion (:autoimport (:enable t))))))

  (defun tao/project-try-tsconfig-json (dir)
    (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
      (cons 'eglot-project found)))

  (add-hook 'project-find-functions #'tao/project-try-tsconfig-json nil nil)
  (add-to-list 'eglot-server-programs
               '((typescript-mode) "typescript-language-server" "--stdio")))

;; Project root method for eglot-project type
(cl-defmethod project-root ((project (head eglot-project)))
  (cdr project))

;;; ── YASnippet ────────────────────────────────────────────────────────────────

(use-package! yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.config/yasnippets/"))
  (add-to-list 'yas-snippet-dirs "~/.config/yasnippets/")

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

  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (local-set-key (kbd "TAB")   #'yas-expand)
              (local-set-key (kbd "<tab>") #'yas-expand))))

;;; ── Vue ──────────────────────────────────────────────────────────────────────

(use-package! vue-mode
  :hook (vue-mode . lsp!))

;;; ── Tree-sitter ──────────────────────────────────────────────────────────────

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
  "Install or reinstall Tree-sitter grammars when missing or the list changed."
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

(use-package! treesit
  :mode (("\\.tsx\\'"      . tsx-ts-mode)
         ("\\.js\\'"       . typescript-ts-mode)
         ("\\.mjs\\'"      . typescript-ts-mode)
         ("\\.mts\\'"      . typescript-ts-mode)
         ("\\.cjs\\'"      . typescript-ts-mode)
         ("\\.ts\\'"       . typescript-ts-mode)
         ("\\.jsx\\'"      . tsx-ts-mode)
         ("\\.json\\'"     . json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'"   . prisma-ts-mode))
  :init
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
  :config
  (tao/setup-install-grammars))

;;; ── LSP mode ─────────────────────────────────────────────────────────────────

(use-package! lsp-mode
  :diminish "LSP"
  :hook ((lsp-mode          . lsp-diagnostics-mode)
         (lsp-mode          . lsp-enable-which-key-integration)
         (tsx-ts-mode       . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (js-ts-mode        . lsp-deferred))
  :init
  (setq lsp-use-plists t)
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
  (lsp-semantic-tokens-enable nil))

(use-package! lsp-completion
  :no-require
  :hook (lsp-mode . lsp-completion-mode))

(use-package! lsp-ui
  :commands (lsp-ui-doc-show lsp-ui-doc-glance)
  :after (lsp-mode evil)
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-ui-doc-glance))
  :config
  (setq lsp-ui-doc-enable t
        evil-lookup-func #'lsp-ui-doc-glance
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point))

(use-package! typescript-ts-mode
  :hook ((typescript-ts-mode . lsp)
         (tsx-ts-mode        . lsp)))

;;; ── Project / Projectile ─────────────────────────────────────────────────────

(use-package! project
  :config
  (add-to-list 'project-vc-extra-root-markers ".git"))

(use-package! projectile
  :config
  (let* ((projects-path      "~/Projects")
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

;;; ── exec-path-from-shell ─────────────────────────────────────────────────────

(use-package! exec-path-from-shell
  :when (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-arguments '("-l")
        exec-path-from-shell-variables '("PATH"
                                         "MISE_SHELL"
                                         "DEFAULT_GIT_COMMIT_MESSAGE"
                                         "GITHUB_PULL_REQUEST_REVIEWERS"
                                         "JIRA_USER"
                                         "JIRA_TOKEN"
                                         "JIRA_ISSUE_BASE_URL"
                                         "JIRA_ISSUE_KEY_PREFIX"))
  :config
  (exec-path-from-shell-initialize)
  (dolist (var exec-path-from-shell-variables)
    (ignore-errors (exec-path-from-shell-copy-env var))))

;;; ── Org ──────────────────────────────────────────────────────────────────────

(use-package! org
  :config
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "DOING(p)"
           "REVIEW(r)"
           "BLOCKED(b)"
           "|" "DONE(d)" "CANCELED(c)"))
        org-log-done 'time
        org-todo-keyword-faces
        '(("TODO"     . (:foreground "#008080" :weight bold))
          ("DOING"    . (:foreground "#00ff00" :weight bold))
          ("BLOCKED"  . (:foreground "#ff0000" :weight bold))
          ("REVIEW"   . (:foreground "#00ffff" :weight bold))
          ("DONE"     . (:foreground "#708090" :weight bold)))
        org-use-fast-todo-selection 'auto)

  (defun my/org-time-stamp-with-time ()
    "Insert an Org timestamp including time."
    (interactive)
    (org-time-stamp '(4)))

  (map! :map org-mode-map
        :localleader
        :desc "Set TODO state"          "t" #'org-todo
        :desc "Insert timestamp w/time" "T" #'my/org-time-stamp-with-time)

  (defun tao/org-prettify-symbols ()
    "Set up prettify symbols for Org buffers."
    (setq-local prettify-symbols-alist
                '(("[ ]" . "☐")
                  ("[X]" . "☑")
                  ("[-]" . "❍")))
    (prettify-symbols-mode 1))
  (add-hook 'org-mode-hook #'tao/org-prettify-symbols)

  (defface org-task-with-clock
    '((t :foreground "Cyan")
      :group 'org)
    "Face for Org tasks with clock entries.")

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

  (add-hook 'org-mode-hook          #'tao/org-fontify-clock-tasks)
  (add-hook 'org-agenda-finalize-hook #'tao/org-fontify-clock-tasks)

  (defun tao/org-update-last-timestamp ()
    "Update or insert #+UPDATED: with the current timestamp."
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
                      (progn (end-of-line)
                             (insert "\n#+UPDATED: " timestamp))
                    (goto-char (point-min))
                    (if (re-search-forward "^#\\+.*$" nil t)
                        (progn (end-of-line)
                               (insert "\n#+UPDATED: " timestamp))
                      (insert "#+UPDATED: " timestamp "\n")))))
            (message "Last modified: %s" timestamp)))
      (message "Buffer is not associated with a file")))

  (add-hook 'before-save-hook #'tao/org-update-last-timestamp)

  ;; ── Auto-sink DONE/CANCELED headings to bottom of their sibling list ──

  (defun tao/org-todo-state-is-terminal-p (state)
    "Return non-nil if STATE is DONE or CANCELED."
    (member state '("DONE" "CANCELED")))

  (defun tao/org-sink-done-heading ()
    "Move the current heading after the last non-terminal sibling."
    (when (tao/org-todo-state-is-terminal-p org-state)
      (save-excursion
        (org-back-to-heading t)
        (let* ((level        (org-current-level))
               (stars        (make-string level ?*))
               (subtree-beg  (point))
               (subtree-end  (save-excursion (org-end-of-subtree t t) (point)))
               (subtree-text (buffer-substring subtree-beg subtree-end))
               (insert-after nil))
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

;;; ── org-superstar ────────────────────────────────────────────────────────────

(use-package! org-superstar
  :defer t
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list
        '("✿" "✸" "⬢" "☯" "○" "◆" "▲" "■" "♦" "♢" "▫")
        org-superstar-item-bullet-alist
        '((?* . ?•) (?+ . ?➤) (?- . ?–))))

;;; ── org-pomodoro ─────────────────────────────────────────────────────────────

(use-package! org-pomodoro
  :config
  (defcustom org-pomodoro-display-count-p t
    "When non-nil, display the total number of pomodoros in the modeline."
    :group 'org-pomodoro
    :type 'boolean)

  (defcustom org-pomodoro-count-format "[%s] "
    "Format string for the total pomodoro count."
    :group 'org-pomodoro
    :type 'string)

  (defun org-pomodoro-format-count ()
    "Format the total number of pomodoros, or empty string if not shown."
    (if (and org-pomodoro-display-count-p (> org-pomodoro-count 0))
        (format org-pomodoro-count-format org-pomodoro-count)
      ""))

  (defun org-pomodoro-update-mode-line ()
    "Set the modeline according to the current pomodoro state."
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
              (list "[" (format s (org-pomodoro-format-seconds)) "] "
                    (org-pomodoro-format-count))))
      (force-mode-line-update t)))

  (defun tao/org-pomodoro-refresh-hook ()
    "Refresh clock-task fontification when a pomodoro starts or finishes."
    (tao/org-fontify-clock-tasks))

  (add-hook 'org-pomodoro-started-hook  #'tao/org-pomodoro-refresh-hook)
  (add-hook 'org-pomodoro-finished-hook #'tao/org-pomodoro-refresh-hook))

;;; ── which-key ────────────────────────────────────────────────────────────────

(use-package! which-key
  :config
  (setq which-key-use-C-h-commands t
        which-key-show-transient-maps t
        which-key-max-display-columns nil
        which-key-side-window-max-height 0.5))

;;; ── apheleia ─────────────────────────────────────────────────────────────────

(use-package! apheleia
  :config
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier
        (alist-get 'tsx-ts-mode        apheleia-mode-alist) 'prettier
        (alist-get 'js-mode            apheleia-mode-alist) 'prettier
        (alist-get 'python-mode        apheleia-mode-alist) 'black
        (alist-get 'black              apheleia-formatters) '("black" "-"))

  (defun tao/conditionally-enable-apheleia ()
    (when (and (derived-mode-p 'prog-mode)
               (not (or (bound-and-true-p lsp-mode)
                        (bound-and-true-p eglot--managed-mode)
                        (member major-mode '(rust-mode rust-ts-mode
                                             yaml-mode yaml-ts-mode)))))
      (apheleia-mode-maybe)))

  (add-hook 'prog-mode-hook #'tao/conditionally-enable-apheleia))

;;; ── Magit / Forge ────────────────────────────────────────────────────────────

(use-package! magit
  :config
  (setq ediff-diff-options ""
        ediff-custom-diff-options "-u"
        ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-vertically
        magit-ediff-dwim-show-on-hunks t
        magit-uniqfy-buffer-names t
        magit-save-repository-buffers 'dontask
        magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1
        forge-topic-list-limit '((pullreq . 50) (issue . 0)))

  (add-hook 'magit-mode-hook (lambda () (display-line-numbers-mode -1)))

  (remove-hook 'magit-status-sections-hook #'forge-insert-pullreqs)

  ;; ── Open PRs section ──

  (defun +forge-insert-open-prs ()
    (when (forge-get-repository nil t)
      (magit-insert-section (forge-pullreqs)
        (magit-insert-heading "Open Pull Requests")
        (dolist (pr (forge-sql
                     [:select [number title state author login]
                      :from pullreq
                      :where (and (= repository $s1) (= state "open"))]
                     (forge-get-repository)))
          (insert (format "#%s  %s\n" (aref pr 0) (aref pr 1)))))))

  (add-hook 'magit-status-sections-hook #'+forge-insert-open-prs)

  ;; ── Filtered local branches section ──

  (defun +magit-insert-filtered-local-branches ()
    (let ((branches
           (seq-filter
            (lambda (branch)
              (not (member branch '("main" "master" "develop"))))
            (magit-list-local-branch-names))))
      (when branches
        (magit-insert-section (local-branches)
          (magit-insert-heading "Local Branches")
          (dolist (branch branches)
            (magit-insert-section (branch branch)
              (insert (format "%s\n" branch))))))))

  (add-hook 'magit-status-sections-hook #'+magit-insert-filtered-local-branches 20)
  (add-to-list 'magit-section-initial-visibility-alist '(local-branches . hide))

  ;; ── Jump to first uncommitted change on status open ──

  (defun +magit-move-to-first-uncommitted-change ()
    (when (eq major-mode 'magit-status-mode)
      (run-at-time
       0.1 nil
       (lambda ()
         (goto-char (point-min))
         (when (or (re-search-forward "^Unstaged changes" nil t)
                   (re-search-forward "^Staged changes" nil t))
           (goto-char (match-beginning 0))
           (forward-line 1))))))

  (add-hook 'magit-status-mode-hook #'+magit-move-to-first-uncommitted-change)

  ;; ── Worktrees ──

  (defun tao/magit-worktree-flat-branch-name (branch)
    (and branch (string-replace "/" "-" branch)))

  (defun tao/magit-read-worktree-directory (prompt branch)
    (let* ((root       (magit-toplevel))
           (parent     (file-name-directory (directory-file-name root)))
           (proj       (file-name-nondirectory (directory-file-name root)))
           (base       (expand-file-name (concat proj "-worktrees/") parent))
           (default-name (or (tao/magit-worktree-flat-branch-name branch) "worktree")))
      (unless (file-directory-p base)
        (make-directory base t))
      (read-directory-name prompt base nil nil default-name)))

  (setq magit-read-worktree-directory-function #'tao/magit-read-worktree-directory)

  (defun +magit-worktree-dirty-p (path)
    (not (string-empty-p
          (shell-command-to-string
           (format "git -C %s status --porcelain"
                   (shell-quote-argument path))))))

  (defun +magit-worktree-ahead-behind (path)
    (let* ((cmd (format
                 "git -C %s rev-list --left-right --count HEAD...@{upstream} 2>/dev/null"
                 (shell-quote-argument path)))
           (out (string-trim (shell-command-to-string cmd))))
      (when (string-match "\\([0-9]+\\)[ \t]+\\([0-9]+\\)" out)
        (format "↑%s ↓%s" (match-string 1 out) (match-string 2 out)))))

  (defun +magit-insert-worktrees ()
    (when-let ((worktrees (magit-list-worktrees)))
      (magit-insert-section (worktrees)
        (magit-insert-heading "Worktrees")
        (let ((current (magit-toplevel)))
          (dolist (wt worktrees)
            (pcase-let ((`(,path ,branch ,_head ,_locked) wt))
              (let* ((is-current   (string= (file-truename path)
                                            (file-truename current)))
                     (dot          (if is-current "●" " "))
                     (dirty        (+magit-worktree-dirty-p path))
                     (status       (if dirty "✗" "✓"))
                     (status-face  (if dirty 'error 'success))
                     (ahead-behind (+magit-worktree-ahead-behind path))
                     (branch-name  (if (and branch
                                            (not (string-match-p "^[0-9a-f]\\{7,\\}$" branch)))
                                       branch
                                     "(detached)")))
                (magit-insert-section (worktree path)
                  (insert
                   (format "%s %-40s %-20s %s %s\n"
                           (propertize dot 'face 'magit-branch-local)
                           (abbreviate-file-name path)
                           branch-name
                           (or ahead-behind "")
                           (propertize status 'face status-face)))))))))))

  (add-hook 'magit-status-sections-hook #'+magit-insert-worktrees 5)
  (add-to-list 'magit-section-initial-visibility-alist '(worktrees . show))

  (defun +magit-visit-worktree ()
    (when-let* ((section (magit-current-section))
                (path    (oref section value)))
      (dired path)))

  (define-key magit-status-mode-map (kbd "RET") #'+magit-visit-worktree)

  (defun tao/magit-switch-worktree ()
    (let* ((worktrees (magit-list-worktrees))
           (paths     (mapcar #'car worktrees))
           (choice    (completing-read "Worktree: " paths nil t)))
      (dired choice)))

  (defun tao/magit-create-worktree-from-branch ()
    (interactive)
    (let* ((branch (magit-read-branch "Branch"))
           (dir    (tao/magit-read-worktree-directory "Worktree directory: " branch)))
      (magit-run-git "worktree" "add" dir branch)
      (magit-refresh)))

  (map! :leader
        :desc "Switch git worktree"        "g w" #'tao/magit-switch-worktree
        :desc "Create worktree from branch" "g W" #'tao/magit-create-worktree-from-branch)

  (map! :map magit-status-mode-map
        :n "n" #'magit-section-forward
        :n "p" #'magit-section-backward)

  (defun +magit-toggle-local-branches-section ()
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^Local Branches$" nil t)
        (let ((section (magit-current-section)))
          (when (magit-section-p section)
            (magit-section-toggle section))))))

  ;; ── General keybindings ──

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
     "z"   '(:which-key "string inflection")
     "z a" '(string-inflection-all-cycle       :which-key "all cases")
     "z c" '(string-inflection-camelcase       :which-key "camelCase")
     "z k" '(string-inflection-kebab-case      :which-key "kebab-case")
     "z l" '(string-inflection-lower-camelcase :which-key "lowerCamelCase")
     "z p" '(string-inflection-upper-camelcase :which-key "UpperCamelCase")
     "z s" '(string-inflection-underscore      :which-key "snake_case")
     "z u" '(string-inflection-upcase          :which-key "UPCASE"))

    (general-define-key
     :states 'normal
     :keymaps 'override
     :prefix doom-leader-key
     "m i u" '(markdown-tools-insert-human-url :which-key "insert human URL"))

    (general-define-key
     :states 'normal
     :keymaps 'override
     :prefix doom-leader-key
     "m g c" '(git-tools-open-all-conflict-files :which-key "git open conflict files"))

    (general-define-key
     :keymaps 'magit-status-mode-map
     :states 'normal
     "z l" '(+magit-toggle-local-branches-section :which-key "toggle local branches"))))

;;; ── vterm ────────────────────────────────────────────────────────────────────

(use-package! vterm
  :config
  (setq vterm-always-compile-module t)
  (define-key vterm-mode-map (kbd "<tab>") #'vterm-send-tab))

;;; ── ws-butler ────────────────────────────────────────────────────────────────

(use-package! ws-butler
  :config
  (add-hook 'prog-mode-hook     #'ws-butler-mode)
  (add-hook 'text-mode-hook     #'ws-butler-mode)
  (add-hook 'makefile-mode-hook (lambda () (ws-butler-mode -1)))
  (add-hook 'markdown-mode-hook (lambda () (ws-butler-mode -1))))

;;; ── Local packages (from ~/.config/elisp) ────────────────────────────────────

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
  (setq slackcount-alert-sound "/System/Library/Sounds/Funk.aiff")
  (slackcount-mode 1))

;;; ── Custom file ──────────────────────────────────────────────────────────────

(setq custom-file (expand-file-name "custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
