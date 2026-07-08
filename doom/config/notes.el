;;; $DOOMDIR/config/notes.el --- org mode configuration lexical-binding: t -*-
(setq org-directory "~/Notes/")

(defvar org-index-file (concat org-directory "index.org")
  "The org file of web bookmarks.

If it does not exist, than the web bookmarks menu will not be provided.")

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
      ;; Clear out bookmarks from any previous registration pass first, so
      ;; stale/phantom commands don't linger in the obarray. Leftover
      ;; commands not only show up as bogus menu entries, they also break
      ;; the "common prefix shared by everything" check in
      ;; `tao/search-org-bookmarks' (a stale entry with a different shape
      ;; can prevent a real common prefix from ever being detected).
      (mapatoms (lambda (atom)
                  (when (and (fboundp atom)
                             (string-prefix-p "tao/web-" (symbol-name atom)))
                    (fmakunbound atom))))
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
                  ;; If this link *is* the title of its immediate ancestor
                  ;; headline (e.g. "** [[url][desc]]"), skip that headline
                  ;; when walking up. Otherwise its title -- which is just
                  ;; this same link -- gets re-added as a "parent"
                  ;; breadcrumb, duplicating the link's own description
                  ;; (e.g. "index-https-slashdot-org-slashdot-org-slashdot-org").
                  (when (and parent
                             (eq (org-element-type parent) 'headline)
                             (let ((title (org-element-property :title parent)))
                               (or (eq title link)
                                   (and (listp title) (memq link title)))))
                    (setq parent (org-element-property :parent parent)))
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

  (when (and (boundp 'org-index-file)
             org-index-file
             (file-exists-p org-index-file))
    (tao/register-org-bookmarks org-index-file))

  ;; --- Helpers for grouping/sorting bookmarks by shared leading words ---

  (defun tao/--common-prefix-length (word-lists)
    "Return how many leading words are shared by every list in WORD-LISTS.
Returns 0 if there are fewer than two lists, since a \"common\" prefix
is only meaningful when comparing multiple entries."
    (if (or (null word-lists) (null (cdr word-lists)))
        0
      (let ((min-len (apply #'min (mapcar #'length word-lists)))
            (idx 0)
            (matching t))
        (while (and matching (< idx min-len))
          (let ((word (nth idx (car word-lists))))
            (if (cl-every (lambda (words) (string= (nth idx words) word))
                          (cdr word-lists))
                (setq idx (1+ idx))
              (setq matching nil))))
        idx)))

  (defun tao/--word-list-lessp (a b)
    "Compare word lists A and B word-by-word.
This groups entries that share the longest run of leading words next
to each other, only falling back to plain alphabetical order once the
shared words run out."
    (cond
     ((and (null a) (null b)) nil)
     ((null a) t)
     ((null b) nil)
     ((string= (car a) (car b)) (tao/--word-list-lessp (cdr a) (cdr b)))
     (t (string-lessp (car a) (car b)))))

  ;; Create an interactive search menu with aligned, colored abbreviation column flags
  (defun tao/search-org-bookmarks ()
    "Interactively select and launch any registered web bookmark.
Bookmarks are grouped and sorted by their longest shared leading words.
Any leading words common to every single bookmark are stripped from
both the displayed name and the abbreviation used for matching."
    (interactive)
    (if (and (boundp 'org-index-file)
             org-index-file
             (file-exists-p org-index-file))
        (let* ((commands '())
               (_ (mapatoms (lambda (atom)
                              (when (and (fboundp atom)
                                         (string-prefix-p "tao/web-" (symbol-name atom)))
                                (push (symbol-name atom) commands)))))
               (raw-choices (mapcar (lambda (cmd) (substring cmd 8)) commands))
               (word-lists (mapcar (lambda (choice) (split-string choice "-")) raw-choices))
               (common-prefix-len (tao/--common-prefix-length word-lists))
               (stripped-word-lists (mapcar (lambda (words) (nthcdr common-prefix-len words))
                                            word-lists))
               (entries (cl-mapcar #'cons raw-choices stripped-word-lists))
               ;; Group and sort by longest matching leading words
               (sorted-entries (sort entries
                                     (lambda (a b) (tao/--word-list-lessp (cdr a) (cdr b)))))
               (processed-items '())
               (max-abbrev-len 0)
               (abbrev-alist '())
               (menu-choices '()))

          ;; Step 1: Pre-calculate all abbreviations (on the stripped words) and
          ;; measure maximum required length
          (dolist (entry sorted-entries)
            (let* ((full-choice (car entry))
                   (words (cdr entry))
                   (abbrev (mapconcat (lambda (word) (substring word 0 1)) words "")))
              (setq max-abbrev-len (max max-abbrev-len (length abbrev)))
              (push (list full-choice words abbrev) processed-items)))
          (setq processed-items (nreverse processed-items))

          ;; Step 2: Render aligned strings (using the stripped words as the
          ;; display name) and apply theme symbol styling. Order is preserved
          ;; from the grouped/sorted pass above.
          (dolist (item processed-items)
            (let* ((full-choice (nth 0 item))
                   (words (nth 1 item))
                   (abbrev (nth 2 item))
                   ;; Fall back to the full name if stripping left nothing
                   ;; (i.e. the whole name equalled the common prefix)
                   (display-name (if words (mapconcat 'identity words "-") full-choice))
                   (fmt-string (format "%%-%ds   %%s" max-abbrev-len))
                   (aligned-str (format fmt-string abbrev display-name))
                   (styled-str (copy-sequence aligned-str)))
              (put-text-property 0 max-abbrev-len 'face 'font-lock-variable-name-face styled-str)
              (push (cons styled-str full-choice) abbrev-alist)
              (push styled-str menu-choices)))
          (setq menu-choices (nreverse menu-choices))

          ;; Step 3: Run interactive minibuffer selection matching
          (let ((selection (completing-read "Launch Bookmark (Abbrev/Name): " menu-choices nil t)))
            (when selection
              (let ((final-cmd (cdr (assoc selection abbrev-alist))))
                (funcall (intern (concat "tao/web-" final-cmd)))))))
      (user-error "Cannot search bookmarks: `%s` does not exist!" org-index-file)))

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
