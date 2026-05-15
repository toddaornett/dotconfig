;;; $DOOMDIR/packages.el -*- no-byte-compile: t; -*-

(package! meow)
(package! apheleia)
(package! exec-path-from-shell)
(package! jira)
(package! nvm)
(package! npm)
(package! org-superstar)
(package! request)
(package! string-inflection)
(package! treesit-auto)
(package! vue-mode)
(package! ws-butler)
(package! yasnippet-snippets)

(package! swagger
  :recipe (:local-repo "~/.config/elisp"
           :files ("swagger.el")))

(package! insert-random-uuid-into-buffer
  :recipe (:local-repo "~/.config/elisp"
           :files ("insert-random-uuid-into-buffer.el")))

(package! jira-todo
  :recipe (:local-repo "~/.config/elisp"
           :files ("jira-todo.el")))

(package! slackcount
  :recipe (:local-repo "~/.config/elisp"
           :files ("slackcount.el")))
