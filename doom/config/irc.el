;;; $DOOMDIR/config/irc.el --- irc config -*- lexical-binding: t -*-

(defcustom irc-channels '("#emacs" "#linux")
  "A list of IRC channels to join automatically on Libera Chat."
  :type '(repeat string))

(setq auth-sources '("~/.authinfo"))

(defun tao/erc-quit-and-cleanup ()
  "Quit all ERC connections and kill all ERC buffers (and close speedbar)."
  (interactive)
  (when (fboundp 'erc-cmd-GQUIT)
    (erc-cmd-GQUIT ""))
  (when (fboundp 'erc-buffer-list)
    (dolist (buf (erc-buffer-list))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

;; Defined at top level (not inside `after! erc') so it's always callable,
;; even before ERC itself has ever been loaded. `require' forces the
;; `after! erc' block below to run first, which sets up erc-server,
;; erc-port, erc-nick, SASL, etc.
(defun +irc/connect ()
  "Connect to Libera Chat automatically using pre-configured variables."
  (interactive)
  (require 'erc)
  (erc-tls :server erc-server
           :port erc-port
           :nick erc-nick))

(after! erc
  (if (and (boundp 'irc-nickname)
           (stringp irc-nickname)
           (not (string-empty-p irc-nickname)))
      (progn
        (add-to-list 'erc-modules 'sasl)
        (erc-update-modules)
        (setq erc-prompt-for-password nil)
        (setq erc-server "irc.libera.chat"
              erc-port 6697
              erc-ssl t)
        (setq erc-autojoin-channels-alist
              `(("libera.chat" ,@irc-channels)))
        (setq erc-sasl-mechanism 'plain
              erc-sasl-user irc-nickname
              erc-nick irc-nickname
              erc-sasl-password :password)
        (add-hook 'erc-join-hook #'erc-speedbar-browser)
        (setq speedbar-update-speed 60)
        (setq speedbar-frame-parameters
              '((minibuffer . nil)
                (width . 20)
                (border-width . 0)
                (menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (unsplittable . t)
                (side . right)))
        (defun tao/erc-auto-close-speedbar (&rest _args)
          "Close speedbar if there are no active ERC buffers remaining."
          (unless (seq-find (lambda (buf)
                              (with-current-buffer buf
                                (derived-mode-p 'erc-mode)))
                            (buffer-list))
            (when (and (featurep 'speedbar)
                       (fboundp 'speedbar-close-window)
                       (fboundp 'speedbar-current-frame)
                       (speedbar-current-frame))
              (speedbar-close-window)))):w
        (add-hook 'kill-buffer-hook #'tao/erc-auto-close-speedbar)
        (add-hook 'erc-disconnected-hook #'tao/erc-auto-close-speedbar))

    (message "ERC automatic login skipped: irc-nickname is empty or undefined.")))

(defun tao/smart-speedbar-toggle ()
  "Toggle the speedbar. If visible from a different buffer, toggle updates."
  (interactive)
  (if (and (bound-and-true-p speedbar-frame)
           (frame-live-p speedbar-frame)
           (frame-visible-p speedbar-frame))
      (progn
        (speedbar-toggle-updates)
        (message "Speedbar updates toggled."))
    (speedbar)))
