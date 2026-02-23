;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(set-language-environment "UTF-8")

(add-to-list 'load-path "~/.config/elisp")

(setq epa-pinentry-mode 'loopback)
(load "~/.emacs_private.el" t)

;; Fonts
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 16 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 16)
      doom-big-font (font-spec :family "Fira Sans" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; Theme
(setq doom-theme 'doom-palenight)

;; Configure highlight-indent-guides to avoid indentation line bleeding into tops of characters
(use-package! highlight-indent-guides
  :diminish
  :commands highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-display-first t)
  (line-spacing 0.1))

;; Line numbers
(setq display-line-numbers-type t)

;; Org directory
(setq org-directory "~/Notes/")

;; Delay garbage collection for performance
(setq gc-cons-threshold (* 50 1000 1000))

;; Make deleted files go to the trash can
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash")

;; disable automatic linking of system clipboard to emacs
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
  ;; Use C-<tab> for company completion if TAB is busy
  (define-key company-mode-map (kbd "C-<tab>") 'company-complete)

  (setq company-idle-delay 0.2) ; Auto-popup with delay
  (setq company-minimum-prefix-length 1) ; Trigger after 1 character
  (setq company-tooltip-limit 10)
  (setq company-dabbrev-ignore-buffers (lambda (buffer)
                                         (string-match-p "^#" (buffer-name buffer))))

  ;; Use C-<tab> for explicit company completion
  (define-key company-mode-map (kbd "C-<tab>") 'company-complete)

  ;; previous and next bindings for completion box
  (define-key company-active-map (kbd "M-n") 'company-select-next)
  (define-key company-active-map (kbd "M-p") 'company-select-previous)
  (define-key company-active-map (kbd "down") 'company-select-next)
  (define-key company-active-map (kbd "up") 'company-select-previous)

  ;; Ensure that TAB does not interfere with completion
  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "<tab>") nil))

;; Rust with Eglot
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
  (setq eglot-events-buffer-size 1000000)
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
  (add-to-list 'yas-snippet-dirs "~/.config/yasnippets/")
  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (local-set-key (kbd "TAB") 'yas-expand)
              (local-set-key (kbd "<tab>") 'yas-expand))))

(after! vue
  (add-hook 'vue-mode-hook #'lsp!))

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ;; More modes defined here...
         )
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))

(use-package lsp-mode
  :diminish "LSP"
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)                   ; Use xref to find references
  (lsp-auto-configure t)                ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)          ; I use prettier
  (lsp-enable-links nil)                ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

  :init
  (setq lsp-use-plists t))

(use-package lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode)))

(use-package lsp-ui
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :after (lsp-mode evil)
  :config (setq lsp-ui-doc-enable t
                evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))



(use-package typescript-ts-mode
  :mode ("\\.js\\'" "\\.ts\\'" "\\.tsx\\'")
  :hook
  ((typescript-ts-mode . lsp)
   (tsx-ts-mode . lsp)))

;; Project root for Eglot
(cl-defmethod project-root ((project (head eglot-project)))
  (cdr project))

(after! eglot
  (defun tao/project-try-tsconfig-json (dir)
    (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
      (cons 'eglot-project found)))
  (add-hook 'project-find-functions 'tao/project-try-tsconfig-json nil nil)
  (add-to-list 'eglot-server-programs
               '((typescript-mode) "typescript-language-server" "--stdio")))

;; Keybindings
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

;; URL encoding/decoding
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

;; Enable company in insert mode
(after! evil
  (add-hook 'evil-insert-state-entry-hook #'company-mode))

;; Projectile
(after! projectile
  (let* ((projects-path "~/Projects")
         (open-projects-path (getenv "OPENPROJECTS_PATH"))
         (paths (delq nil (list projects-path
                                (unless (string= projects-path open-projects-path)
                                  open-projects-path)))))
    (dolist (path paths)
      (let ((entry (cons path 2)))
        (unless (assoc path projectile-project-search-path)
          (add-to-list 'projectile-project-search-path entry)))))
  (add-to-list 'projectile-project-search-path (cons "~/.config" 1)))

;; Exec-path-from-shell
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-arguments nil)
    (setq exec-path-from-shell-variables '("PATH" "DEFAULT_GIT_COMMIT_MESSAGE"))
    (exec-path-from-shell-initialize)))

;; Org
(after! org
  ;; Set custom TODO keywords and faces
  (setq org-todo-keywords '((sequence "TODO" "DOING" "REVIEW" "BLOCKED" "|" "DONE"))
        org-log-done 'time
        org-todo-keyword-faces
        '(("TODO" . (:foreground "#008080" :weight bold))
          ("DOING" . (:foreground "#00ff00" :weight bold))
          ("BLOCKED" . (:foreground "#ff0000" :weight bold))
          ("REVIEW" . (:foreground "#00ffff" :weight bold))
          ("DONE" . (:foreground "#708090" :weight bold)))
        org-use-fast-todo-selection t)

  ;; Keybinding for toggling TODO states
  (map! :map org-mode-map :n "t" #'org-todo)

  ;; Prettify symbols in org-mode
  (defun tao/org-prettify-symbols ()
    "Set up prettify symbols for Org buffers."
    (setq-local prettify-symbols-alist
                '(("[ ]" . "☐")
                  ("[X]" . "☑")
                  ("[-]" . "❍")))
    (prettify-symbols-mode 1))
  (add-hook 'org-mode-hook #'tao/org-prettify-symbols)

  ;; Highlight tasks with clock entries
  (defface org-task-with-clock
    '((t :foreground "Cyan"))
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
  (add-hook 'org-mode-hook #'tao/org-fontify-clock-tasks)
  (add-hook 'org-agenda-finalize-hook #'tao/org-fontify-clock-tasks)

  ;; Pomodoro hooks for fontifying clock tasks
  (defun tao/org-pomodoro-start-or-finished-hook ()
    "Hook to run when org-pomodoro starts or finishes."
    (tao/org-fontify-clock-tasks))
  (add-hook 'org-pomodoro-started-hook #'tao/org-pomodoro-start-or-finished-hook)
  (add-hook 'org-pomodoro-finished-hook #'tao/org-pomodoro-start-or-finished-hook)

  (defun tao/org-update-last-timestamp ()
    "Update or insert the #+UPDATED: keyword with the current timestamp in Org mode files,
placing it after #+CREATED: if it exists, or display the last modified time for other files."
    (interactive)
    (if (buffer-file-name)
        (let ((timestamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (if (derived-mode-p 'org-mode)
              (save-excursion
                (goto-char (point-min))
                (if (re-search-forward "^#\\+UPDATED:.*$" nil t)
                    (replace-match (concat "#+UPDATED: " timestamp))
                  ;; Check for #+CREATED: and insert after it
                  (goto-char (point-min))
                  (if (re-search-forward "^#\\+CREATED:.*$" nil t)
                      (progn
                        (end-of-line)
                        (insert "\n#+UPDATED: " timestamp))
                    ;; Fallback: insert after first Org keyword or at start
                    (goto-char (point-min))
                    (if (re-search-forward "^#\\+.*$" nil t)
                        (progn
                          (end-of-line)
                          (insert "\n#+UPDATED: " timestamp))
                      (insert "#+UPDATED: " timestamp "\n")))))
            (message "Last modified: %s" timestamp)))
      (message "Buffer is not associated with a file")))
  (add-hook 'before-save-hook #'tao/org-update-last-timestamp))

;; org-superstar
(use-package org-superstar
  :defer t
  :hook org-mode
  :config
  (setq org-superstar-headline-bullets-list '("✿" "✸" "⬢" "☯" "○" "◆" "▲" "■" "♦" "♢" "▫"))
  (setq org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?➤) (?- . ?–))))

;; Org-pomodoro
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

;; Custom keybindings via General
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
   :keymaps 'magit-status-mode-map
   :states 'normal
   "z l" '(+magit-toggle-local-branches-section :which-key "toggle local branches")))

;; control which-key popup and pagination
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

(after! magit
  (setq ediff-diff-options "")
  (setq ediff-custom-diff-options "-u")
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-vertically)
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq forge-topic-list-limit '((pullreq . 50) (issue . 0)))

  ;; Disable line numbers in Magit buffers
  (add-hook 'magit-mode-hook (lambda () (display-line-numbers-mode -1)))

  ;; Limit pullreqs globally (still good practice)
  ;; Remove Forge's default PR section
  (remove-hook 'magit-status-sections-hook 'forge-insert-pullreqs)

  ;; Add custom PR section that shows only open PRs (not merged)
  (add-hook 'magit-status-sections-hook
            (defun +forge-insert-open-prs ()
              "Show only open (non-merged) pull requests in Magit status."
              (when (forge-get-repository nil t)
                (magit-insert-section (forge-pullreqs)
                  (magit-insert-heading "Open Pull Requests")
                  (dolist (pr (forge-sql [:select [number title state author login created updated]
                                          :from pullreq
                                          :where (and (= repository $s1) (= state "open"))]
                                         (forge-get-repository)))
                    (insert (format "#%s  %s\n" (aref pr 0) (aref pr 1))))))))

  ;; Add custom local branches section, excluding main, master, develop
  (add-hook 'magit-status-sections-hook
            (defun +magit-insert-filtered-local-branches ()
              "Show local branches excluding main, master, and develop in Magit status."
              (let ((branches (seq-filter (lambda (branch)
                                            (not (member branch '("main" "master" "develop"))))
                                          (magit-list-local-branch-names))))
                (when branches
                  (magit-insert-section (local-branches)
                    (magit-insert-heading "Local Branches")
                    (dolist (branch branches)
                      (magit-insert-section (branch branch)
                        (insert (format "%s\n" branch))))))))
            20)

  ;; Ensure Local Branches section is collapsed by default
  (add-to-list 'magit-section-initial-visibility-alist '(local-branches . hide))

  ;; Define function to move point to first uncommitted change
  (defun +magit-move-to-first-uncommitted-change ()
    "Move point to the first uncommitted change in the Magit status buffer."
    (interactive)
    (when (eq major-mode 'magit-status-mode)
      (run-at-time 0.1 nil
                   (lambda ()
                     (goto-char (point-min))
                     (when (or (re-search-forward "^Unstaged changes" nil t)
                               (re-search-forward "^Staged changes" nil t))
                       (goto-char (match-beginning 0))
                       (forward-line 1))))))
  (add-hook 'magit-status-mode-hook '+magit-move-to-first-uncommitted-change)

  ;; Define command to toggle Local Branches section globally
  (defun +magit-toggle-local-branches-section ()
    "Toggle visibility of the Local Branches section from anywhere in status region."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^Local Branches$" nil t)
        (let ((section (magit-current-section)))
          (when (magit-section-p section)
            (magit-section-toggle section)))))))

;; port-number => load from ~/.config/elisp
(use-package port-number)

;; nodoze => load from ~/.config/elisp
(use-package nodoze)

;; colima => load from ~/.config/elisp
(use-package colima)

;; git-tools => load from ~/.config/elisp
(use-package git-tools)

;; pg-tools => load from ~/.config/elisp
(use-package pg-tools)

;; vterm
(use-package vterm
  :config
  (setq vterm-always-compile-module t)
  (define-key vterm-mode-map (kbd "<tab>") 'vterm-send-tab))

;; Custom file
(setq custom-file (expand-file-name "custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
