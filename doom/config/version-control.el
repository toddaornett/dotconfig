(after! magit
  (setq ediff-diff-options "")
  (setq ediff-custom-diff-options "-u")
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-vertically)
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq magit-uniqfy-buffer-names t)
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq forge-topic-list-limit '((pullreq . 50) (issue . 0)))

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
