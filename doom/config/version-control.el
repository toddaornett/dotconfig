;;; $DOOMDIR/config/version-control.el --- magit and forge config -*- lexical-binding: t -*-
(after! magit
  (setq ediff-diff-options "")
  (setq ediff-custom-diff-options "-u")
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-vertically)
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq magit-uniquify-buffer-names t)
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq forge-topic-list-limit '((pullreq . 50) (issue . 0)))

  (defun +magit-push-target-for-branch (branch)
    "Return REMOTE/BRANCH as the default push target for local BRANCH."
    (let* ((branch (or branch (magit-get-current-branch)))
           (remote (or (and branch (magit-get-push-remote branch))
                       (and branch (magit-get-remote branch))
                       (magit-primary-remote)
                       (car (magit-list-remotes)))))
      (when (and branch remote)
        (concat remote "/" branch))))

  (defun +magit-read-remote-branch--same-name-default (orig prompt &rest args)
    "When pushing, default to a same-named branch on the push remote."
    (let ((remote (nth 0 args))
          (default (nth 1 args))
          (local-branch (nth 2 args))
          (require-match (nth 3 args)))
      (when (and local-branch (not default))
        (setq default (+magit-push-target-for-branch local-branch)))
      (funcall orig prompt remote default local-branch require-match)))

  (advice-add #'magit-read-remote-branch :around
              #'+magit-read-remote-branch--same-name-default)

  (defun +magit-push-current-to-upstream--same-name-default (orig &rest args)
  "When setting upstream while pushing, default to same-named remote branch."
  (let ((magit-completing-read-orig (symbol-function 'magit-completing-read)))
    (cl-letf (((symbol-function 'magit-completing-read)
               (lambda (prompt choices &rest cr-args)
                 (let ((def (nth 4 cr-args)))
                   (when (and (string-match-p "\\`Set upstream of " prompt)
                              (not def)
                              (magit-get-current-branch))
                     (setf (nth 4 cr-args)
                           (+magit-push-target-for-branch
                            (magit-get-current-branch))))
                   (apply magit-completing-read-orig prompt choices cr-args)))))
      (apply orig args))))

  (advice-add #'magit-push-current-to-upstream :around
              #'+magit-push-current-to-upstream--same-name-default)

  (add-hook 'magit-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)))

  (remove-hook 'magit-status-sections-hook #'forge-insert-pullreqs)

  (defun +forge-insert-open-prs ()
    (when-let* ((repo-row (car (forge-sql
                                [:select [id] :from repository
                                 :where (= worktree $s1)]
                                (expand-file-name default-directory))))
                (repo-id (car repo-row))
                (prs (forge-sql
                      [:select [number title]
                       :from pullreq
                       :where (and (= repository $s1)
                                   (= state 'open))]
                      repo-id))
                (_ prs))
      (magit-insert-section (forge-pullreqs nil t)
        (magit-insert-heading
          (propertize (format "Open Pull Requests (%d)" (length prs))
                      'face 'magit-section-heading))
        (dolist (pr prs)
          (magit-insert-section (pullreq (car pr))
            (insert (format "  #%-4s  %s\n"
                            (car pr)
                            (cadr pr)))))
        (insert ?\n))))
  (add-hook 'magit-status-sections-hook #'+forge-insert-open-prs t)

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

  (add-hook 'magit-status-sections-hook
            #'+magit-insert-filtered-local-branches
            20)

  (add-to-list 'magit-section-initial-visibility-alist
               '(local-branches . hide))

  (defface tao-magit-orange-face
    '((t :foreground "#FFA500" :weight bold))
    "High-contrast orange face for custom magit revision messages.")

  (defun tao/magit-insert-revision-message ()
    (let ((rev (or magit-buffer-refname "HEAD")))
      (when-let ((msg (magit-git-string "log" "-1" "--format=%B" rev)))
        (unless (string-empty-p msg)
          (let* ((lines (split-string msg "\n"))
                  (title (car lines))
                  (body (string-trim (mapconcat #'identity (cdr lines) "\n"))))
            ;; Title: plain line, not a foldable section
            (insert (propertize "Title:   " 'face 'magit-section-secondary-heading))
            (let ((start (point)))
              (insert title "\n")
              (add-face-text-property start (point) 'tao-magit-orange-face))
            ;; Body: only inserted (and foldable) if it actually exists
            (unless (string-empty-p body)
              (insert "\n")
              (magit-insert-section (commit-message nil t) ; t = hidden by default
                (magit-insert-heading "Description")
                (let ((start (point)))
                  (insert body "\n")
                  (add-face-text-property start (point) 'tao-magit-orange-face))))
            (insert "\n"))))))

  ;; Remove the stock inserter, and any prior copy of ours, so this
  ;; block stays idempotent across config reloads.
  (remove-hook 'magit-revision-sections-hook #'magit-insert-revision-message)
  (remove-hook 'magit-revision-sections-hook #'tao/magit-insert-revision-message)

  (setq magit-revision-sections-hook
    '(magit-insert-revision-tag
       magit-insert-revision-headers
       tao/magit-insert-revision-message
       magit-insert-revision-notes
       magit-insert-revision-diff))

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

  (add-hook 'magit-status-mode-hook
            #'+magit-move-to-first-uncommitted-change)

  (defun tao/magit-worktree-flat-branch-name (branch)
    (and branch (string-replace "/" "-" branch)))

  (defun tao/magit-read-worktree-directory (prompt branch)
    (let* ((root (magit-toplevel))
           (parent (file-name-directory (directory-file-name root)))
           (proj (file-name-nondirectory (directory-file-name root)))
           (base (expand-file-name (concat proj "-worktrees/") parent))
           (default-name (or (tao/magit-worktree-flat-branch-name branch)
                             "worktree")))
      (unless (file-directory-p base)
        (make-directory base t))
      (read-directory-name prompt base nil nil default-name)))

  (setq magit-read-worktree-directory-function
        #'tao/magit-read-worktree-directory)

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
        (format "↑%s ↓%s"
                (match-string 1 out)
                (match-string 2 out)))))

  (defun +magit-insert-worktrees ()
    (when-let ((worktrees (magit-list-worktrees)))
      (magit-insert-section (worktrees)
        (magit-insert-heading "Worktrees")
        (let ((current (magit-toplevel)))
          (dolist (wt worktrees)
            (pcase-let ((`(,path ,branch ,_head ,_locked) wt))
              (let* ((is-current
                      (string=
                       (file-truename path)
                       (file-truename current)))
                     (dot (if is-current "●" " "))
                     (dirty (+magit-worktree-dirty-p path))
                     (status (if dirty "✗" "✓"))
                     (status-face (if dirty 'error 'success))
                     (ahead-behind (+magit-worktree-ahead-behind path))
                     (branch-name
                      (if (and branch
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

  (add-hook 'magit-status-sections-hook
            #'+magit-insert-worktrees
            5)

  (add-to-list 'magit-section-initial-visibility-alist
               '(worktrees . show))

  (defun +magit-dwim-visit ()
    (interactive)
    (let ((section (magit-current-section)))
      (pcase (oref section type)
        ('worktree
         (dired (oref section value)))
        ('pullreq
         (when-let* ((number (oref section value))
                     (repo-row (car (forge-sql
                                     [:select [id] :from repository
                                      :where (= worktree $s1)]
                                     (expand-file-name default-directory))))
                     (repo-id (car repo-row))
                     (pr (car (forge-sql
                               [:select [id] :from pullreq
                                :where (and (= repository $s1)
                                            (= number $s2))]
                               repo-id number))))
           (forge-visit-topic (forge-get-topic (car pr)))))
        ((or 'untracked 'unstaged 'staged 'file)
         (magit-diff-visit-file (oref section value)))
        (_
         (call-interactively #'magit-visit-thing)))))

  (define-key magit-status-mode-map
              (kbd "RET")
              #'+magit-dwim-visit)

  (defun +magit-dwim-browse ()
    (interactive)
    (let ((section (magit-current-section)))
      (pcase (oref section type)
        ('pullreq
         (forge-browse-topic
          (forge-get-topic
           (car (car (forge-sql
                      [:select [id] :from pullreq
                       :where (and (= repository $s1)
                                   (= number $s2))]
                      (car (car (forge-sql
                                 [:select [id] :from repository
                                  :where (= worktree $s1)]
                                 (expand-file-name default-directory))))
                      (oref section value)))))))
        (_ (call-interactively #'magit-reset)))))

  (map! :map magit-status-mode-map
        :n "o" #'+magit-dwim-browse)

  (defun +magit-checkout-pr-at-point ()
    (interactive)
    (let ((section (magit-current-section)))
      (when (eq (oref section type) 'pullreq)
        (let* ((number (oref section value))
               (branch (caar (forge-sql
                              [:select [head-ref] :from pullreq
                               :where (and (= repository $s1)
                                           (= number $s2))]
                              (car (car (forge-sql
                                         [:select [id] :from repository
                                          :where (= worktree $s1)]
                                         (expand-file-name default-directory))))
                              number))))
          (magit--checkout branch)))))

  (map! :leader
        :desc "Checkout PR at point"
        "g p" #'+magit-checkout-pr-at-point)

  (map! :leader
        :desc "Forge pull"
        "g F" #'forge-pull)

  (map! :map magit-status-mode-map
        :n "c" #'magit-commit)

  (defun tao/magit-switch-worktree ()
    (let* ((worktrees (magit-list-worktrees))
           (paths (mapcar #'car worktrees))
           (choice (completing-read "Worktree: " paths nil t)))
      (dired choice)))

  (defun tao/magit-create-worktree-from-branch ()
    (interactive)
    (let* ((branch (magit-read-branch "Branch"))
           (dir (tao/magit-read-worktree-directory
                 "Worktree directory: "
                 branch)))
      (magit-run-git "worktree" "add" dir branch)
      (magit-refresh)))

  (map! :leader
        :desc "Switch git worktree"
        "g w" #'tao/magit-switch-worktree
        :desc "Create worktree from branch"
        "g W" #'tao/magit-create-worktree-from-branch)

  (map! :map magit-status-mode-map
        :n "n" #'magit-section-forward
        :n "p" #'magit-section-backward)

  (general-define-key
   :states 'normal
   :keymaps 'override
   :prefix doom-leader-key
   "m g c" '(git-tools-open-all-conflict-files :which-key "git open conflict files"))

  (defun +magit-toggle-local-branches-section ()
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^Local Branches$" nil t)
        (let ((section (magit-current-section)))
          (when (magit-section-p section)
            (magit-section-toggle section)))))))

;; -----------------------------------------------------------------------
;; Live diff markers in editing buffers
;;
;; This is `git-gutter`, the package Doom's `:ui vc-gutter` module wires
;; up. It shows +/-/~ markers (or fringe icons, with the +pretty flag) next
;; to lines that are added, deleted, or modified relative to HEAD, updated
;; live as you type — separate from magit's status/diff views above.
;;
;; If you don't already have this module, add it to $DOOMDIR/init.el:
;;   :ui
;;   (vc-gutter +pretty)   ; or just `vc-gutter` for the plain character style
;; then `doom sync` and restart.
;; -----------------------------------------------------------------------
(after! git-gutter
  ;; Recompute markers frequently, without requiring a save first.
  (setq git-gutter:update-interval 0.5)

  ;; Enable it in every file-visiting buffer.
  (add-hook 'find-file-hook #'git-gutter-mode)

  ;; Keep the gutter in sync right after staging/committing/etc. in magit.
  (add-hook 'magit-post-refresh-hook #'git-gutter:update-all-windows))

;; If you're using the +pretty flag (fringe bitmaps instead of characters),
;; this tweaks their shape/placement; safe to remove if you prefer the
;; module's defaults.
(after! git-gutter-fringe
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))
