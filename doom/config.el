;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(set-language-environment "UTF-8")

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(after! straight
  (setq straight-repository-remap
        '(("git.savannah.gnu.org" . "github.com/emacsmirror")))
  (straight-override-recipe
   '(:type git :host "git.savannah.gnu.org" :repo "emacs/nongnu.git")
   '(:type git :host "github.com" :repo "emacsmirror/nongnu")))

(after! emacs
  (add-load-path! "lisp")
  (add-load-path! "../elisp"))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Todd Ornett"
      user-mail-address "toddgh@acquirus.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "FiraCode" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 16 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 16)
      doom-big-font (font-spec :family "Fira Sans" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-palenight)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Notes/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(after! treesit
  (add-to-list 'treesit-language-source-alist
               '((rust . ("https://github.com/tree-sitter/tree-sitter-rust" "master" "src"))
                 (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
                 (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))))

(use-package typescript-ts-mode
  :mode (("\\.js\\'" . typescript-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook)
    #'lsp!))

(defadvice! workaround--+lookup--xref-show (fn identifier &optional show-fn)
  :override #'+lookup--xref-show
  (let ((xrefs (funcall fn
                        (xref-find-backend)
                        identifier)))
    (when xrefs
      (funcall (or show-fn #'xref--show-defs)
               (lambda () xrefs)
               nil)
      (if (cdr xrefs)
          'deferred
        t))))

(add-function :after after-focus-change-function (lambda () (save-some-buffers t)))

;; I'm not sure why this is needed, but it throws an error if I remove it
(cl-defmethod project-root ((project (head eglot-project)))
  (cdr project))

(after! eglot
  (defun my-project-try-tsconfig-json (dir)
    (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
      (cons 'eglot-project found)))

  (add-hook 'project-find-functions
            'my-project-try-tsconfig-json nil nil)

  (add-to-list 'eglot-server-programs
               '((typescript-mode) "typescript-language-server" "--stdio"))
  )

(defun insert-backslash ()
  "insert back-slash"
  (interactive)
  (insert "\\"))

(global-set-key (kbd "M-¥") 'insert-backslash)

;; snippets
(yas-global-mode t)
(add-hook 'yas-minor-mode-hook (lambda ()
                                 (yas-activate-extra-mode 'fundamental-mode)))
(setq yas-snippet-dirs
      '("~/.config/yasnippets/"))

(defun add-yasnippet-ac-sources ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(add-hook 'rustic-mode-hook 'add-yasnippet-ac-sources)

(unless (and (fboundp 'play-sound-internal)
             (subrp (symbol-function 'play-sound-internal)))
  (require 'play-sound))

(let ((my-private-config-file "~/.local/config/emacs/config.el"))
  (if (file-exists-p my-private-config-file)
      (load (file-name-sans-extension my-private-config-file))
    (message "Ignoring missing local configration expected in %s" my-private-config-file)))

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

(after! rustic
  (setq lsp-rust-server 'rust-analyzer)
  (setq rustic-lsp-server 'rust-analyzer)
  (setq lsp-rust-analyzer-cargo-watch-enable t)
  (setq lsp-rust-analyzer-imports-merge-glob t)
  (setq lsp-rust-analyzer-imports-group t)
  (setq rustic-format-on-save t)
  (map! :map rustic-mode-map
        "M-j" #'lsp-ui-imenu
        "M-?" #'lsp-find-references
        "C-c C-c C-c" #'rustic-compile
        "C-c C-c l" #'flycheck-list-errors
        "C-c C-c a" #'lsp-execute-code-action
        "C-c C-c r" #'lsp-rename
        "C-c C-c q" #'lsp-workspace-restart
        "C-c C-c Q" #'lsp-workspace-shutdown
        "C-c C-c s" #'lsp-rust-analyzer-status)
  (setq lsp-enable-symbol-highlighting nil)
  (setq rustic-format-trigger nil)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (add-hook 'rustic-mode-hook #'flycheck-mode))

(after! company
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (add-to-list 'company-backends 'company-capf))

(after! evil
  (add-hook 'evil-insert-state-entry-hook #'company-mode))

(after! company-box
  (setq company-box-enable-icon t)
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))

(setq-hook! 'typescript-mode-hook +format-with :prettier)

(use-package! treesit-auto
  :config
  (setq treesit-auto-install t)
  (global-treesit-auto-mode))

(use-package! port-number)

(use-package! nodoze)

(use-package! colima)

(use-package! git-tools)

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

(use-package! exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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
    "Set the modeline accordingly to the current state.

     Note that this version supports the number of completed pomodoros
     to be displayed on the modeline."
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
    (force-mode-line-update t))))

(after! exec-path-from-shell
  (setenv "PATH" (concat (getenv "HOMEBREW") "bin/rg:" (getenv "PATH")))
  (add-to-list 'exec-path (concat (getenv "HOMEBREW") "/bin/rg")))

(use-package! magit
  :config
  :hook (magit-status-sections-hook . tao/magit-status-sections-hook)
  :config
    (defun tao/magit-status-sections-hook ()
      '(magit-insert-status-headers
        magit-insert-merge-log
        magit-insert-rebase-sequence
        magit-insert-am-sequence
        magit-insert-sequencer-sequence
        magit-insert-bisect-output
        magit-insert-bisect-rest
        magit-insert-bisect-log
        magit-insert-untracked-files
        magit-insert-unstaged-changes
        magit-insert-staged-changes
        magit-insert-stashes
        magit-insert-unpushed-to-pushremote
        magit-insert-unpushed-to-upstream-or-recent
        magit-insert-unpulled-from-pushremote
        magit-insert-unpulled-from-upstream
        magit-insert-local-branches)))

(map! :leader
      :prefix ("z" . "string inflection")
      :desc "all cases"       :n "a" #'string-inflection-all-cycle
      :desc "camelCase"       :n "c" #'string-inflection-camelcase
      :desc "kebab-case"      :n "k" #'string-inflection-kebab-case
      :desc "lowerCamelCase"  :n "l" #'string-inflection-lower-camelcase
      :desc "UpperCamelCase"  :n "p" #'string-inflection-upper-camelcase
      :desc "snake_case"      :n "s" #'string-inflection-underscore
      :desc "UPCASE"          :n "u" #'string-inflection-upcase)

(use-package! tide
  :ensure t
  :after (company flycheck)
  :hook ((typescript-ts-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

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

(setq custom-file (expand-file-name "custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
