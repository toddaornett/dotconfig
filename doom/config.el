;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(set-language-environment "UTF-8")

;; User identity
(setq user-full-name "Todd Ornett"
      user-mail-address "toddgh@acquirus.com")
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

;; Line numbers
(setq display-line-numbers-type t)

;; Org directory
(setq org-directory "~/Notes/")

;; Delay garbage collection for performance
(setq gc-cons-threshold (* 50 1000 1000))

;; Rust with Eglot
(after! rustic
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-on-save t)
  (set-company-backend! 'rustic-mode '(company-capf))
  (add-hook 'rustic-mode-hook (lambda ()
                                (yas-minor-mode -1)
                                (setq-local company-backends '(company-capf)))))

(after! eglot
  (setq eglot-sync-connect 0) ; Async LSP connection
  (setq eglot-autoshutdown t) ; Shutdown server when done
  (setq eglot-events-buffer-size 1000000) ; Log completion events for debugging
  (add-to-list 'eglot-server-programs
               '(rust-mode . ("rust-analyzer" :initializationOptions
                              (:procMacro (:enable t) ; Enable for macro completions
                               :diagnostics (:enable nil)
                               :cargo (:watch (:enable nil))
                               :completion (:autoimport (:enable t)))))))

(after! company
  (setq company-idle-delay 0.2) ; Auto-popup with delay
  (setq company-minimum-prefix-length 1) ; Trigger after 1 character
  (setq company-tooltip-limit 10) ; Limit suggestions
  (setq company-backends '(company-capf company-yasnippet)) ; Include yasnippet
  (defun tao/company-filter-contiguous-prefix (candidates)
    "Filter CANDIDATES to only those starting contiguously with the current prefix."
    (let ((prefix (downcase (if (stringp company-prefix) company-prefix ""))))
      (cl-remove-if-not
       (lambda (candidate)
         (string-prefix-p prefix (downcase candidate) t))
       candidates)))
  (setq company-transformers
        '(company-sort-by-backend-importance
          company-sort-prefer-same-case-prefix
          tao/company-filter-contiguous-prefix))
  (setq company-begin-commands '(self-insert-command))
  (define-key company-active-map (kbd "M-n") 'company-select-next)
  (define-key company-active-map (kbd "M-p") 'company-select-previous)
  (define-key company-active-map (kbd "<down>") 'company-select-next)
  (define-key company-active-map (kbd "<up>") 'company-select-previous)
  (define-key company-active-map (kbd "TAB") nil)
  (define-key company-active-map (kbd "<tab>") nil))

(after! yasnippet
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.config/yasnippets/"))
  (add-hook 'rustic-mode-hook
            (lambda ()
              (local-set-key (kbd "TAB") 'yas-expand))))

;; Disable tree-sitter for Rust
(after! treesit
  (setq treesit-language-source-alist
        '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))))

(use-package! typescript-ts-mode
  :mode (("\\.js\\'" . typescript-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook)
    #'lsp!))

;; Project root for Eglot
(cl-defmethod project-root ((project (head eglot-project)))
  (cdr project))

(after! eglot
  (defun tao-project-try-tsconfig-json (dir)
    (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
      (cons 'eglot-project found)))
  (add-hook 'project-find-functions 'tao-project-try-tsconfig-json nil nil)
  (add-to-list 'eglot-server-programs
               '((typescript-mode) "typescript-language-server" "--stdio")))

;; Keybindings
(defun insert-backslash ()
  "Insert backslash"
  (interactive)
  (insert "\\"))
(global-set-key (kbd "M-¥") 'insert-backslash)

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
(use-package! exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-arguments nil) ; Avoid loading slow shell configs
    (exec-path-from-shell-initialize)))

;; Org customizations
(after! org
  (setq
    ;; Define stages for todo tasks
    org-todo-keywords '((sequence "TODO" "DOING" "REVIEW" "BLOCKED" "|" "DONE" ))

    ;; When item enters DONE, add a CLOSED: property with current date-time stamp
    org-log-done 'time

    ;; Make TODO states easier to distinguish by using different colours
    ;; Using X11 colour names from: https://en.wikipedia.org/wiki/Web_colors
    org-todo-keyword-faces
    '(("TODO" . "Teal")
      ("DOING" . "Green")
      ("BLOCKED" . "Red")
      ("REVIEW" . "Aqua")
      ("DONE" . "SlateGray"))

    ;; Allows full cycle with C-c C-t
    org-use-fast-todo-selection t)

  (map! :map org-mode-map
        :n "t" #'org-todo)

  (defun tao/org-prettify-symbols ()
    "Set up prettify symbols for Org buffers."
    (when (derived-mode-p 'org-mode)
      (setq-local prettify-symbols-alist
                  (append prettify-symbols-alist
                          '(("[ ]" . "☐")
                            ("[X]" . "☑")
                            ("[-]" . "❍"))))
      (prettify-symbols-mode 1)))
  (add-hook! 'org-mode-hook #'tao/org-prettify-symbols)

  ;; Define a custom face for tasks with clock entries
  (defface org-task-with-clock
    '((t :foreground "Cyan"))
    "Face for Org tasks with clock entries.")

  ;; Function to check if a headline has clock entries
  (defun tao/org-has-clock-entries-p ()
    "Return non-nil if the current headline has clock entries."
    (save-excursion
      (org-back-to-heading t)
      (let ((end (org-entry-end-position)))
        (re-search-forward "^[ \t]*CLOCK:" end t))))

  ;; Function to fontify only the headline text, preserving prettified asterisks
  (defun tao/org-fontify-clock-tasks ()
    "Fontify Org tasks with clock entries, skipping the asterisks."
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-heading-regexp nil t)
          (let* ((beg (match-beginning 0))  ;; Start of the full headline
                 (end (match-end 0))        ;; End of the full headline
                 (text-beg (progn           ;; Start of the text (after asterisks/TODO)
                             (goto-char beg)
                             (skip-chars-forward "*[:space:]")
                             (when (looking-at org-todo-regexp)
                               (goto-char (match-end 0))
                               (skip-chars-forward "[:space:]"))
                             (point))))
            (when (tao/org-has-clock-entries-p)
              (add-text-properties text-beg end '(font-lock-face org-task-with-clock))))))))

  ;; Function to run when pomodoro starts
  (defun tao/org-pomodoro-start-or-finished-hook ()
    "Hook to run when org-pomodoro starts or finishes."
    (tao/org-fontify-clock-tasks))
  (add-hook! 'org-mode-hook #'tao/org-fontify-clock-tasks)
  (add-hook! 'org-agenda-finalize-hook #'tao/org-fontify-clock-tasks)
  (add-hook! 'org-pomodoro-started-hook #'tao/org-pomodoro-start-or-finished-hook)
  (add-hook! 'org-pomodoro-finished-hook #'tao/org-pomodoro-start-or-finished-hook))

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
  (add-hook! 'org-pomodoro-started-hook #'tao/org-pomodoro-start-or-finished-hook)
  (add-hook! 'org-pomodoro-finished-hook #'tao/org-pomodoro-start-or-finished-hook))

;; Custom keybindings
(map! :leader
      :prefix ("z" . "string inflection")
      :desc "all cases"       :n "a" #'string-inflection-all-cycle
      :desc "camelCase"       :n "c" #'string-inflection-camelcase
      :desc "kebab-case"      :n "k" #'string-inflection-kebab-case
      :desc "lowerCamelCase"  :n "l" #'string-inflection-lower-camelcase
      :desc "UpperCamelCase"  :n "p" #'string-inflection-upper-camelcase
      :desc "snake_case"      :n "s" #'string-inflection-underscore
      :desc "UPCASE"          :n "u" #'string-inflection-upcase)

;; Custom file
(setq custom-file (expand-file-name "custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
