;;; $DOOMDIR/config/org.el --- org mode configuration lexical-binding: t -*-
(defun tao/register-org-bookmarks (org-file)
  "Parse ORG-FILE and generate native Emacs interactive commands for every web link."
  (interactive)
  (when (file-exists-p org-file)
    (with-temp-buffer
      (insert-file-contents org-file)
      (org-mode)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (let ((type (org-element-property :type link))
                 (path (org-element-property :path link))
                 (desc (car (org-element-contents link))))
            (when (and (member type '("http" "https")) (stringp desc))
              (let* ((url (concat type ":" path))
                      (parent-titles '())
                      (parent (org-element-property :parent link)))
                (while parent
                  (when (eq (org-element-type parent) 'headline)
                    (let ((title-prop (org-element-property :title parent)))
                      (cond
                        ((stringp title-prop)
                          (push (substring-no-properties title-prop) parent-titles))
                        (title-prop
                          (push (substring-no-properties (org-element-interpret-data title-prop)) parent-titles)))))
                  (setq parent (org-element-property :parent parent)))

                (let* ((full-desc-list (append parent-titles (list (substring-no-properties desc))))
                        (combined-desc (mapconcat 'identity full-desc-list "-"))
                        (clean-desc (downcase (replace-regexp-in-string "[^a-zA-Z0-9]" "-" combined-desc)))
                        (normalized-desc (replace-regexp-in-string "-+" "-" clean-desc))
                        (cmd-name (string-trim normalized-desc "-"))
                        (cmd-symbol (intern (concat "tao/web-" cmd-name))))

                  (defalias cmd-symbol
                    (eval `(lambda ()
                             (interactive)
                             (browse-url ,url)))
                    (format "Open %s in the default browser." url)))))))))))
(tao/register-org-bookmarks "~/Notes/index.org")

;; Create an interactive search menu with aligned, colored abbreviation column flags
(defun tao/search-org-bookmarks ()
  "Interactively select and launch any registered web bookmark."
  (interactive)
  (let* ((commands '())
          (_ (mapatoms (lambda (atom)
                         (when (and (fboundp atom)
                                 (string-prefix-p "tao/web-" (symbol-name atom)))
                           (push (symbol-name atom) commands)))))
          (raw-choices (mapcar (lambda (cmd) (substring cmd 8)) commands))
          (processed-items '())
          (max-abbrev-len 0)
          (abbrev-alist '())
          (menu-choices '()))

    ;; Step 1: Pre-calculate all abbreviations and measure maximum required length
    (dolist (choice raw-choices)
      (let* ((parts (split-string choice "-"))
              (github-index (cl-position "github" parts :test 'string=))
              (target-parts (if github-index (nthcdr (1+ github-index) parts) parts))
              (abbrev-base (if target-parts target-parts parts))
              (abbrev (mapconcat (lambda (word) (substring word 0 1)) abbrev-base "")))
        (setq max-abbrev-len (max max-abbrev-len (length abbrev)))
        (push (list choice abbrev) processed-items)))

    ;; Step 2: Render aligned strings and apply theme symbol styling
    (dolist (item processed-items)
      (let* ((choice (car item))
              (abbrev (cadr item))
              ;; Dynamic layout formatting block to auto-align columns perfectly
              (fmt-string (format "%%-%ds   %%s" max-abbrev-len))
              (aligned-str (format fmt-string abbrev choice))
              ;; Fontify the matching abbreviation segment using standard Emacs symbol faces
              (styled-str (copy-sequence aligned-str)))
        (put-text-property 0 max-abbrev-len 'face 'font-lock-variable-name-face styled-str)
        (push (cons styled-str choice) abbrev-alist)
        (push styled-str menu-choices)))

    ;; Step 3: Run interactive minibuffer selection matching
    (let ((selection (completing-read "Launch Bookmark (Abbrev/Name): " menu-choices nil t)))
      (when selection
        (let ((final-cmd (cdr (assoc selection abbrev-alist))))
          (funcall (intern (concat "tao/web-" final-cmd))))))))

;; Bind the search menu to Doom's Open -> Web menu hierarchy
(map! :leader
  (:prefix-map ("o" . "open")
    :desc "Org Bookmarks Menu" "w" #'tao/search-org-bookmarks))

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
