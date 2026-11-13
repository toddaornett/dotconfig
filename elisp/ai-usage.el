;;; ai-usage.el --- Track AI coding assistant usage and spend -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Todd Ornett
;;
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: November 13, 2026
;; Modified: November 13, 2026
;; Version: 0.1.0
;; Keywords: tools ai convenience
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/toddaornett/dotconfig
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Track usage and spend across AI coding providers, starting with Cursor.
;;
;; Cursor stores its session JWT in the local VS Code state database
;; (`state.vscdb', key `cursorAuth/accessToken').  Recent versions wrap the
;; token in Chromium's safe-storage envelope
;;
;;     cursor-safe-storage:v1:<base64 (v10 || AES-128-CBC ciphertext)>
;;
;; where the AES key is PBKDF2-SHA1 of the browser's safe-storage password
;; (macOS: Keychain item "Cursor Safe Storage"; Linux: libsecret or the
;; "peanuts" fallback) with salt "saltysalt", 1003 iterations and a 16-space
;; IV.  Windows uses DPAPI and is not supported here.
;;
;; Usage:
;;   M-x ai-usage-report   Display usage summary in a dedicated buffer.
;;   M-x ai-usage-mode     Global minor mode showing usage in the mode-line.
;;   M-x ai-usage-refresh  Force a refresh.
;;
;; Providers are registered in `ai-usage-providers'; each fetcher takes a
;; CALLBACK and eventually calls it with a normalized plist.
;;
;;; Code:

(require 'json)
(require 'url)
(require 'subr-x)
(require 'cl-lib)

(defgroup ai-usage nil
  "Monitor usage and spend across AI coding providers."
  :group 'tools
  :prefix "ai-usage-")

(defcustom ai-usage-providers
  '((cursor . ai-usage--fetch-cursor))
  "Alist mapping provider symbols to fetcher functions.
Each fetcher function takes a CALLBACK `(lambda (plist) ...)' where PLIST
contains standardized usage keys:
  :provider    Symbol name of the provider.
  :plan        Plan or membership name (string).
  :used        Amount used in the current period (number, in `:unit').
  :limit       Total limit for the current period (number); nil if none.
  :percent     Percentage of `:limit' used (float).
  :unit        Display unit, e.g. \"USD\".
  :requests    Number of billable requests in the current period, or nil.
  :reset-date  Formatted end of the current billing cycle.
  :raw         Raw parsed response for provider-specific details.
  :error       Error message string when the fetch failed."
  :type '(alist :key-type symbol :value-type function)
  :group 'ai-usage)

(defcustom ai-usage-cursor-db-path nil
  "Path to Cursor's `state.vscdb' SQLite database.
When nil, detect the platform default."
  :type '(choice (const :tag "Auto-detect" nil) file)
  :group 'ai-usage)

(defcustom ai-usage-cursor-token nil
  "Manually supplied Cursor session JWT.
Bypasses `state.vscdb' and safe-storage decryption entirely."
  :type '(choice (const :tag "Read from state.vscdb" nil) string)
  :group 'ai-usage)

(defcustom ai-usage-cursor-safe-storage-password nil
  "Safe-storage password used to decrypt a Chromium-encrypted token.
When nil, read it from the OS store (Keychain on macOS, `secret-tool' on
Linux) and fall back to Chromium's plain \"peanuts\" password."
  :type '(choice (const :tag "Read from OS store" nil) string)
  :group 'ai-usage)

(defcustom ai-usage-refresh-interval 900
  "Seconds between automatic refreshes while `ai-usage-mode' is active."
  :type 'integer
  :group 'ai-usage)

(defvar ai-usage--cache nil
  "Alist mapping provider symbols to their last fetched usage plists.")

(defvar ai-usage--token-cache nil
  "Cons of (TOKEN . EXPIRY-TIME) cached between refreshes.")

(defvar ai-usage--refresh-timer nil
  "Timer for periodic background refreshes.")

(defvar ai-usage--mode-line-string ""
  "String rendered in the mode-line by `ai-usage-mode'.")

;;; --- Crypto helpers ---

(defun ai-usage--hmac-sha1 (key msg)
  "Return the binary HMAC-SHA1 of MSG keyed with KEY."
  (let* ((key (if (> (length key) 64) (secure-hash 'sha1 key nil nil 'binary) key))
         (key (concat key (make-string (- 64 (length key)) 0)))
         (ipad (make-string 64 0))
         (opad (make-string 64 0)))
    (dotimes (i 64)
      (aset ipad i (logxor (aref key i) #x36))
      (aset opad i (logxor (aref key i) #x5c)))
    (secure-hash 'sha1
                 (concat opad (secure-hash 'sha1 (concat ipad msg) nil nil 'binary))
                 nil nil 'binary)))

(defun ai-usage--pbkdf2-sha1 (password salt iterations length)
  "Derive LENGTH bytes from PASSWORD using PBKDF2-HMAC-SHA1.
SALT is the binary salt string and ITERATIONS the iteration count."
  (let* ((hlen 20)
         (blocks (ceiling (/ (float length) hlen)))
         (chunks nil))
    (dotimes (block blocks)
      (let* ((index (1+ block))
             (u (ai-usage--hmac-sha1
                 password
                 (concat salt (unibyte-string 0 0 0 index))))
             (acc (copy-sequence u)))
        (dotimes (_ (1- iterations))
          (setq u (ai-usage--hmac-sha1 password u))
          (dotimes (i hlen)
            (aset acc i (logxor (aref acc i) (aref u i)))))
        (push acc chunks)))
    (substring (apply #'concat (nreverse chunks)) 0 length)))

(defun ai-usage--hex (bytes)
  "Return BYTES as a lowercase hex string."
  (mapconcat (lambda (byte) (format "%02x" byte)) (string-to-list bytes) ""))

(defun ai-usage--aes-128-cbc-decrypt (key iv ciphertext)
  "Decrypt CIPHERTEXT with AES-128-CBC using KEY and IV.
Relies on the `openssl' CLI for the block cipher; padding is removed."
  (unless (executable-find "openssl")
    (error "The `openssl' executable is required to decrypt the Cursor token"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert ciphertext)
    (let ((status (call-process-region (point-min) (point-max) "openssl" t t nil
                                       "enc" "-d" "-aes-128-cbc"
                                       "-K" (ai-usage--hex key)
                                       "-iv" (ai-usage--hex iv))))
      (unless (eq status 0)
        (error "openssl failed to decrypt the Cursor token"))
      (buffer-string))))

(defun ai-usage--safe-storage-password ()
  "Return the Chromium safe-storage password for this platform."
  (or ai-usage-cursor-safe-storage-password
      (cond
       ((eq system-type 'darwin)
        (string-trim
         (with-output-to-string
           (with-demoted-errors "ai-usage: %S"
             (call-process "security" nil standard-output nil
                           "find-generic-password"
                           "-s" "Cursor Safe Storage" "-a" "Cursor" "-w")))))
       ((eq system-type 'gnu/linux)
        (string-trim
         (with-output-to-string
           (with-demoted-errors "ai-usage: %S"
             (call-process "secret-tool" nil standard-output nil
                           "lookup" "application" "chrome")))))
       ((eq system-type 'windows-nt)
        (error "Windows DPAPI token decryption is not supported; set `ai-usage-cursor-token'"))
       (t nil))
      "peanuts"))

;;; --- Cursor provider ---

(defun ai-usage--cursor-db-path ()
  "Return the platform default path to Cursor's `state.vscdb'."
  (or ai-usage-cursor-db-path
      (expand-file-name
       "Cursor/User/globalStorage/state.vscdb"
       (cond
        ((eq system-type 'darwin) "~/Library/Application Support/")
        ((eq system-type 'windows-nt) (or (getenv "APPDATA") "~/AppData/Roaming/"))
        (t (or (getenv "XDG_CONFIG_HOME") "~/.config/"))))))

(defun ai-usage--sqlite-value (db sql)
  "Return the first column of the first row of SQL from SQLite database DB."
  (unless (file-exists-p db)
    (error "Cursor database not found at %s" db))
  (if (and (fboundp 'sqlite-available-p) (sqlite-available-p))
      (let ((conn (sqlite-open db)))
        (unwind-protect
            (caar (sqlite-select conn sql))
          (sqlite-close conn)))
    (unless (executable-find "sqlite3")
      (error "Neither built-in SQLite nor the `sqlite3' CLI is available"))
    (string-trim
     (shell-command-to-string
      (format "sqlite3 %s %s"
              (shell-quote-argument db) (shell-quote-argument sql))))))

(defun ai-usage--decrypt-safe-storage (value)
  "Decrypt Chromium safe-storage VALUE into its plaintext string."
  (let* ((blob (base64-decode-string
                (substring value (length "cursor-safe-storage:v1:"))))
         (version (substring blob 0 3))
         (cipher (substring blob 3)))
    (unless (equal version "v10")
      (error "Unsupported safe-storage version %S" version))
    (let* ((password (encode-coding-string (ai-usage--safe-storage-password) 'utf-8))
           (key (ai-usage--pbkdf2-sha1 password "saltysalt" 1003 16))
           (iv (make-string 16 ?\s)))
      (decode-coding-string (ai-usage--aes-128-cbc-decrypt key iv cipher) 'utf-8 t))))

(defun ai-usage--jwt-payload (jwt)
  "Return the decoded JSON payload of JWT as an alist."
  (let ((parts (split-string jwt "\\.")))
    (unless (nth 1 parts)
      (error "access token is not a JWT"))
    (let ((b64 (replace-regexp-in-string
                "[-_]" (lambda (c) (if (equal c "-") "+" "/")) (nth 1 parts))))
      (json-read-from-string
       (base64-decode-string (concat b64 (make-string (% (- 4 (% (length b64) 4)) 4) ?=)))))))

(defun ai-usage--cursor-token ()
  "Return a valid Cursor session JWT, caching it until shortly before expiry."
  (or ai-usage-cursor-token
      (when (and ai-usage--token-cache
                 (< (float-time) (cdr ai-usage--token-cache)))
        (car ai-usage--token-cache))
      (let* ((raw (or (ai-usage--sqlite-value
                       (ai-usage--cursor-db-path)
                       "SELECT value FROM ItemTable WHERE key = 'cursorAuth/accessToken';")
                      (error "No `cursorAuth/accessToken' in the Cursor database")))
             (token (if (string-prefix-p "cursor-safe-storage:" raw)
                        (string-trim (ai-usage--decrypt-safe-storage raw))
                      (string-trim raw)))
             (exp (alist-get 'exp (condition-case nil
                                      (ai-usage--jwt-payload token)
                                    (error nil)))))
        (setq ai-usage--token-cache
              (cons token (if exp (float (string-to-number (format "%s" exp))) 0)))
        token)))

(defun ai-usage--cursor-headers (token)
  "Return request headers authenticating as the Cursor session described by TOKEN."
  (let* ((sub (alist-get 'sub (ai-usage--jwt-payload token)))
         (cookie (format "WorkosCursorSessionToken=%s%%3A%%3A%s" sub token)))
    `(("Content-Type" . "application/json")
      ("Accept" . "application/json")
      ("Cookie" . ,cookie)
      ("Origin" . "https://cursor.com")
      ("Referer" . "https://cursor.com")
      ("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)"))))

(defun ai-usage--http-json (method url headers body callback)
  "Call METHOD URL with HEADERS and BODY, then CALLBACK with `(ERR JSON)'.
ERR is a string on network or parse failure, otherwise nil."
  (let ((url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data (and body (encode-coding-string body 'utf-8))))
    (url-retrieve
     url
     (lambda (status)
       (let ((failure (or (plist-get status :error)
                          (ai-usage--http-status-failure))))
         (if failure
             (funcall callback (format "HTTP error: %s" failure) nil)
           (goto-char (point-min))
           (if (not (re-search-forward "\r?\n\r?\n" nil t))
               (funcall callback "Malformed HTTP response" nil)
             (let ((raw (buffer-substring-no-properties (point) (point-max)))
                   (json-object-type 'alist)
                   (json-key-type 'symbol))
               (condition-case err
                   (funcall callback nil (json-read-from-string raw))
                 (error
                  (funcall callback
                           (format "JSON parse error: %s (body: %.80s)"
                                   (error-message-string err) raw)
                           nil))))))))
     nil t t)))

(defun ai-usage--http-status-failure ()
  "Return a description of the non-2xx status line in the current buffer, if any."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)\\(.*\\)$" nil t)
      (let ((code (string-to-number (match-string 1))))
        (unless (<= 200 code 299)
          (format "%d%s" code (string-trim (match-string 2))))))))

(defconst ai-usage--cursor-summary-url "https://cursor.com/api/usage-summary"
  "Endpoint returning Cursor plan usage for the current billing cycle.")

(defconst ai-usage--cursor-events-url
  "https://cursor.com/api/dashboard/get-filtered-usage-events"
  "Endpoint returning Cursor usage events; its count is the request count.")

(defun ai-usage--parse-summary (data)
  "Normalize Cursor usage summary DATA into a standard plist."
  (let* ((overall (alist-get 'overall (alist-get 'individualUsage data)))
         (on-demand (alist-get 'onDemand (alist-get 'teamUsage data)))
         (used (float (or (alist-get 'used overall) 0)))
         (limit (float (or (alist-get 'limit overall) 0)))
         (reset (alist-get 'billingCycleEnd data)))
    (list :provider 'cursor
          :plan (or (alist-get 'membershipType data) "cursor")
          :used (/ used 100.0)
          :limit (/ limit 100.0)
          :percent (if (> limit 0) (* 100.0 (/ used limit)) 0.0)
          :unit "USD"
          :unlimited (eq (alist-get 'isUnlimited data) t)
          :on-demand-used (/ (float (or (alist-get 'used on-demand) 0)) 100.0)
          :on-demand-limit (/ (float (or (alist-get 'limit on-demand) 0)) 100.0)
          :reset-date (or (and reset (format-time-string "%b %d" (date-to-time reset)))
                          "unknown")
          :raw data)))

(defun ai-usage--iso->millis (iso)
  "Convert ISO 8601 timestamp ISO to epoch milliseconds as a string."
  (format "%.0f" (* 1000 (float-time (date-to-time iso)))))

(defun ai-usage--fetch-cursor (callback)
  "Fetch Cursor usage for the current cycle and pass a plist to CALLBACK."
  (condition-case err
      (let* ((token (ai-usage--cursor-token))
             (headers (ai-usage--cursor-headers token)))
        (ai-usage--http-json
         "GET" ai-usage--cursor-summary-url headers nil
         (lambda (err data)
           (if err
               (progn
                 (setq ai-usage--token-cache nil)
                 (funcall callback (list :provider 'cursor :error err)))
             (let ((plist (ai-usage--parse-summary data))
                   (body (json-encode
                          `((startDate . ,(ai-usage--iso->millis
                                           (alist-get 'billingCycleStart data)))
                            (endDate . ,(ai-usage--iso->millis
                                         (alist-get 'billingCycleEnd data)))))))
               (ai-usage--http-json
                "POST" ai-usage--cursor-events-url headers body
                (lambda (events-err events)
                  (funcall callback
                           (if events-err
                               (plist-put plist :requests nil)
                             (plist-put plist :requests
                                        (alist-get 'totalUsageEventsCount events)))))))))))
    (error
     (setq ai-usage--token-cache nil)
     (funcall callback (list :provider 'cursor :error (error-message-string err))))))

;;; --- Rendering ---

(defun ai-usage--entry-line (plist)
  "Return a one-line summary of usage PLIST."
  (if-let* ((err (plist-get plist :error)))
      (format "%s: %s" (capitalize (symbol-name (plist-get plist :provider))) err)
    (format "%s: $%.2f / $%.2f (%s)%s"
            (capitalize (symbol-name (plist-get plist :provider)))
            (or (plist-get plist :used) 0.0)
            (or (plist-get plist :limit) 0.0)
            (if (plist-get plist :unlimited)
                "unlimited"
              (format "%.1f%%" (or (plist-get plist :percent) 0.0)))
            (if-let* ((req (plist-get plist :requests)))
                (format " · %d requests" req)
              ""))))

(defun ai-usage--update-mode-line ()
  "Recompute `ai-usage--mode-line-string' from `ai-usage--cache'."
  (let ((entries (delq nil (mapcar (lambda (item) (ai-usage--entry-line (cdr item)))
                                   ai-usage--cache))))
    (setq ai-usage--mode-line-string
          (if entries
              (concat " [" (string-join entries " | ") "]")
            ""))))

(defun ai-usage--render-buffer ()
  "Fill the `*ai-usage*' buffer from `ai-usage--cache'."
  (let ((buffer (get-buffer-create "*ai-usage*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "AI usage — updated %s\n\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S")))
        (if (null ai-usage--cache)
            (insert "No data yet; fetching...\n")
          (dolist (item ai-usage--cache)
            (let ((plist (cdr item)))
              (insert (ai-usage--entry-line plist) "\n")
              (unless (plist-get plist :error)
                (insert (format "  plan      %s\n" (plist-get plist :plan)))
                (insert (format "  cycle end %s\n" (plist-get plist :reset-date)))
                (when (plist-get plist :on-demand-limit)
                  (insert (format "  on-demand $%.2f / $%.2f\n"
                                  (plist-get plist :on-demand-used)
                                  (plist-get plist :on-demand-limit)))))
              (insert "\n"))))
        (goto-char (point-min))
        (special-mode)))))

(defun ai-usage--refresh-done ()
  "Render cached usage after a refresh completes."
  (ai-usage--update-mode-line)
  (when (get-buffer "*ai-usage*")
    (ai-usage--render-buffer))
  (force-mode-line-update t))

;;;###autoload
(defun ai-usage-refresh ()
  "Fetch usage from every registered provider and update the UI."
  (interactive)
  (let ((pending (length ai-usage-providers)))
    (if (zerop pending)
        (ai-usage--refresh-done)
      (dolist (entry ai-usage-providers)
        (let ((provider (car entry)))
          (funcall (cdr entry)
                   (lambda (plist)
                     (setf (alist-get provider ai-usage--cache) plist)
                     (when (zerop (cl-decf pending))
                       (ai-usage--refresh-done)))))))))

;;;###autoload
(defun ai-usage-report ()
  "Display AI provider usage in a dedicated buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create "*ai-usage*"))
  (ai-usage--render-buffer)
  (ai-usage-refresh))

(defun ai-usage--mode-line-toggle (enable)
  "Add or remove the usage indicator from `global-mode-string' per ENABLE."
  (if enable
      (progn
        (add-to-list 'global-mode-string '(:eval ai-usage--mode-line-string) t)
        (setq ai-usage--refresh-timer (run-with-timer 0 ai-usage-refresh-interval
                                                     #'ai-usage-refresh))
        (ai-usage-refresh))
    (setq global-mode-string
          (delete '(:eval ai-usage--mode-line-string) global-mode-string))
    (when ai-usage--refresh-timer
      (cancel-timer ai-usage--refresh-timer)
      (setq ai-usage--refresh-timer nil))))

;;;###autoload
(define-minor-mode ai-usage-mode
  "Globally display AI provider usage in the mode-line."
  :global t
  :group 'ai-usage
  (ai-usage--mode-line-toggle ai-usage-mode))

(provide 'ai-usage)
;;; ai-usage.el ends here
