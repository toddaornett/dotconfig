;;; $DOOMDIR/config/keybindings.el --- various keybindings -*- lexical-binding: t -*-

(map! :leader
  :desc "Comment line" "-" #'comment-line)
(map! :leader
  :desc "Comment region" "=" #'comment-line)

(map! :after org-pomodoro
  :leader
  :n "t t" #'org-pomodoro)
