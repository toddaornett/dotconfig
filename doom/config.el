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

;; Disable tree-sitter for Rust
(after! treesit
  (setq treesit-language-source-alist
        '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))))

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

(use-package! ibuffer-projectile
  :after ibuffer
  :commands ibuffer-projectile-generate-filter-groups)

(defun tao/ibuffer-custom-groups ()
  (require 'projectile)
  (require 'ibuffer-projectile)
  (let* ((project-buffers (projectile-project-buffers))
         (project-groups (or (ignore-errors (ibuffer-projectile-generate-filter-groups)) '()))
         (default-group `(("Default" (predicate . (not (member (current-buffer) ',project-buffers))))))
         (cleaned-groups (mapcar (lambda (group)
                                   (cons (file-name-nondirectory (directory-file-name (car group))) (cdr group)))
                                 (cl-remove-if (lambda (group) (string= (car group) "Default")) project-groups))))
    (append cleaned-groups (list default-group))))

(defun tao/ibuffer-toggle-project-buffers (project-root)
  (interactive (list (projectile-completing-read "Select project: " (projectile-relevant-known-projects))))
  (require 'ibuffer)
  (let ((group-name (file-name-nondirectory (directory-file-name project-root))))
    (with-current-buffer (get-buffer "*Ibuffer*")
      (if (member group-name (mapcar #'car ibuffer-filter-groups))
          (progn
            (setq ibuffer-filter-groups
                  (cl-remove-if (lambda (group) (string= (car group) group-name))
                                ibuffer-filter-groups))
            (ibuffer-update nil t))
        (progn
          (setq ibuffer-filter-groups (tao/ibuffer-custom-groups))
          (ibuffer-update nil t))))))

(use-package! ibuffer
  :after projectile
  :hook (ibuffer . (lambda ()
                     (let ((groups (tao/ibuffer-custom-groups)))
                       (when (proper-list-p groups)
                         (setq ibuffer-filter-groups groups)))
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic))))
  :bind (:map ibuffer-mode-map
              '("C-c p t" . tao/ibuffer-toggle-project-buffers)))

;; Exec-path-from-shell
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-arguments nil)
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
  (add-hook 'org-pomodoro-finished-hook #'tao/org-pomodoro-start-or-finished-hook))

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
    "Toggle visibility of the Local Branches section from anywhere in the Magit buffer."
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
