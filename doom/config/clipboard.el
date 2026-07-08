;;; $DOOMDIR/config/clipboard.el --- clipboard enhancements -*- lexical-binding: t -*-

(defcustom tao/paste-url-inspect-length 1000
  "Max number of characters tao/paste-from-clipboard will inspect/accept
before deciding clipboard content is a URL candidate. If the clipboard
contents exceed this length, tao/paste-from-clipboard always pastes
literally instead of trying to format a link."
  :type 'integer
  :group 'tao)

(defun tao/clipboard-string ()
  "Return the current system clipboard contents as a string, or nil.
Relies on `current-kill', which transparently syncs with the system
clipboard via `interprogram-paste-function' — the same mechanism
`clipboard-yank' uses."
  (current-kill 0 t))

(defun tao/url-p (str)
  "Non-nil if STR, trimmed, looks like a single bare URL."
  (and str
    (string-match-p
      (rx bos (* space)
        (or "http://" "https://" "ftp://" "www.")
        (+ (not space))
        (* space) eos)
      str)))

(defun tao/url->description (url)
  "Text after the last `/' in URL, ignoring a trailing slash if present."
  (car (last (split-string url "/" t))))

(defun tao/format-url-for-mode (url)
  "Return URL formatted as a link appropriate to the current major mode."
  (let ((desc (tao/url->description url)))
    (cond
      ((derived-mode-p 'org-mode)      (format "[[%s][%s]]" url desc))
      ((derived-mode-p 'markdown-mode) (format "[%s](%s)" desc url))
      (t url))))

(defun tao/clipboard-string ()
  "Return the current system clipboard contents as a string, or nil.
Tries several retrieval paths in order, since any one of them can
legitimately return nil or \"\" depending on platform quirks,
`gui-selection-value''s dedup cache, or what the source app actually
put on the pasteboard:

1. `gui-get-selection' for CLIPBOARD/STRING — the most direct route,
   bypassing `gui-selection-value''s \"already seen this\" cache.
2. `gui-get-selection' for CLIPBOARD/TEXT — some apps only advertise
   a TEXT-typed pasteboard entry rather than STRING/UTF8.
3. `current-kill' — respects the kill-ring, catching cases where the
   content only ever arrived via `interprogram-paste-function' syncing
   into the kill-ring rather than being freshly re-fetchable.

Returns the first non-blank (after trimming) result, or nil if every
path comes up empty."
  (cl-some
   (lambda (fetch)
     (let ((s (ignore-errors (funcall fetch))))
       (and (stringp s)
            (not (string-blank-p s))
            s)))
   (list
    (lambda () (gui-get-selection 'CLIPBOARD 'STRING))
    (lambda () (gui-get-selection 'CLIPBOARD 'TEXT))
    (lambda () (current-kill 0 t)))))

(defun tao/url-p (str)
  "Non-nil if STR, trimmed, looks like a single bare URL."
  (and str
       (string-match-p
        (rx bos (* space)
            (or "http://" "https://" "ftp://" "www.")
            (+ (not space))
            (* space) eos)
        str)))

(defun tao/url->description (url)
  "Text after the last `/' in URL, ignoring a trailing slash if present."
  (car (last (split-string url "/" t))))

(defun tao/format-url-for-mode (url)
  "Return URL formatted as a link appropriate to the current major mode."
  (let ((desc (tao/url->description url)))
    (cond
     ((derived-mode-p 'org-mode)      (format "[[%s][%s]]" url desc))
     ((derived-mode-p 'markdown-mode) (format "[%s](%s)" desc url))
     (t url))))

(defun tao/paste-literal ()
  "Fall back to whatever the normal paste mechanism is."
  (cond
   ((bound-and-true-p evil-local-mode) (call-interactively #'evil-paste-after))
   (t (call-interactively #'yank))))

(defun tao/paste-from-clipboard ()
  "Paste the system clipboard, auto-linkifying bare URLs in org/markdown.

Inspects up to `tao/paste-url-inspect-length' characters of the
clipboard. If the entire (trimmed) clipboard content is within that
length and looks like a bare URL, and the buffer is `org-mode' or
`markdown-mode', insert a formatted link ([[URL][DESC]] or
[DESC](URL), DESC being the text after the URL's last `/'). In every
other case — longer content, non-URL content, or a buffer that's
neither org nor markdown — paste literally via the usual mechanism."
  (interactive)
  (let* ((clip (tao/clipboard-string))
         (trimmed (and clip (string-trim (substring clip 0 (min (length clip) tao/paste-url-inspect-length))))))
    (if (and clip
             (<= (length clip) tao/paste-url-inspect-length)
             (tao/url-p trimmed)
             (derived-mode-p 'org-mode 'markdown-mode))
        (insert (tao/format-url-for-mode trimmed))
      (tao/paste-literal))))
