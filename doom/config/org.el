;;; $DOOMDIR/config/org.el --- org mode configuration lexical-binding: t -*-
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

  (add-hook 'org-after-todo-state-change-hook #'tao/org-sink-done-heading)
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
                      (lambda ()
                        (interactive)
                        (browse-url url))
                      (format "Open %s in the default browser." url)))))))))))

  ;; Automatically parse your file on startup
  (tao/register-org-bookmarks "~/Notes/index.org")

  ;; Create an interactive search menu dedicated ONLY to your registered bookmarks
  (defun tao/search-org-bookmarks ()
    "Interactively select and launch any registered web bookmark."
    (interactive)
    (let* ((commands '())
            (_ (mapatoms (lambda (atom)
                           (when (and (fboundp atom)
                                   (string-prefix-p "tao/web-" (symbol-name atom)))
                             (push (symbol-name atom) commands)))))
            (choices (mapcar (lambda (cmd) (substring cmd 8)) commands))
            (selection (completing-read "Launch Bookmark: " choices nil t)))
      (when selection
        (funcall (intern (concat "tao/web-" selection))))))

  ;; Bind the search menu to Doom's Open -> Web menu hierarchy
  (map! :leader
    (:prefix-map ("o" . "open")
      :desc "Org Bookmarks Menu" "w" #'tao/search-org-bookmarks)))

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
