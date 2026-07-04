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
      :desc "Reselect last region"
      "r r" #'tao/reselect-last-region)

(map! :leader
      :desc "Toggle speedbar or updates"
      :n "t s" #'tao/smart-speedbar-toggle)

(map! :leader
      :desc "Quit ERC" "q i" #'tao/erc-quit-and-cleanup)
