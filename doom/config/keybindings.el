;;; $DOOMDIR/config/keybindings.el --- various keybindings -*- lexical-binding: t -*-
(map! :leader
      :desc "Comment line" "-" #'comment-line)
(map! :leader
      :desc "Comment region" "=" #'comment-line)

(map! :after org
      :map org-mode-map
      :localleader
      :n "p" #'org-pomodoro)

(map! :leader
      :desc "Reslect last region"
      "r r" #'tao/reselect-last-region)

(map! :leader
      :desc "Toggle speedbar or updates"
      :n "t s" #'tao/smart-speedbar-toggle)

(map! :leader
      :desc "Quit ERC" "q i" #'tao/erc-quit-and-cleanup)

(map! :nvi
      "s-c" #'clipboard-kill-ring-save
      "s-v" #'tao/paste-from-clipboard)
(define-key minibuffer-local-map (kbd "s-v") #'tao/paste-from-clipboard)

(defun tao/copy-relative-path-dwim ()
  "Copy relative path of file at point in Dired, or current buffer's file otherwise."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (tao/dired-copy-relative-path)
    (tao/copy-buffer-relative-path)))

(defun tao/copy-full-path-dwim ()
  "Copy full path of file at point in Dired, or current buffer's file otherwise."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (tao/dired-copy-full-path)
    (tao/copy-buffer-full-path)))

(map! :leader
      (:prefix-map ("f y" . "yank path")
       :desc "Copy full path"     "y" #'tao/copy-full-path-dwim
       :desc "Copy relative path" "r" #'tao/copy-relative-path-dwim))
