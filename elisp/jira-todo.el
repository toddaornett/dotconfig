;;; jira-todo.el --- Generate org-mode TODO a1d Slack message for JIRA tickets -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Todd Ornett
;;
;; Author: Todd Ornett <toddgh@acquirus.com>
;; Maintainer: Todd Ornett <toddgh@acquirus.com>
;; Created: April 22, 2026
;; Modified: October 2, 2026
;; Version: 0.0.1
;; Keywords: jira, org, tools
;; Homepage: https://github-tao/toddaornett/dotconfig
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Fetches a JIRA ticket and generates an org-mode TODO entry with a Slack
;; message and an org-babel source block that posts to Microsoft Teams.
;; Teams messages are sent with `xteams' (https://github.com/boazy/xteams-cli).
;; Requires the `request` package and the following variables to be set:
;;   jira-base-url, jira-issue-key-prefix, jira-username, jira-token
;;
;;; Code:

(require 'request)
(require 'json)
(require 'subr-x)
(require 'cl-lib)
(require 'org)
(require 'git-tools)
(require 'caveman)

(defgroup jira-todo nil
  "Generate `org-mode' TODOs from JIRA tickets."
  :group 'tools)

(defcustom jira-todo-base-url
  (or (getenv "JIRA_ISSUE_BASE_URL") "https://atlassian.net")
  "Base URL for JIRA instance."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-issue-key-prefix
  (or (getenv "JIRA_ISSUE_KEY_PREFIX") "JIRA")
  "JIRA issue key prefix, e.g. JIRA."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-username
  (or (getenv "JIRA_USER") "")
  "JIRA username (email)."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-token
  (or (getenv "JIRA_TOKEN") "")
  "JIRA API token."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-pr-messaging-provider
  (or (getenv "MESSAGING_PROVIDER") "slack")
  "Determine the format of message format.
`slack' or `teams' (Microsoft Teams)"
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-microsoft-teams-channel "Private"
  "Microsoft Teams channel the generated `xteams' block posts to.
The value is passed to `xteams message new' as its conversation
argument, so it can be a channel name, a Teams conversation id
`(59:...@thread.somethingcool), or a Teams link (`xteams channel list'
prints ids for the channels you follow)."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-git-directory
  (or (getenv "JIRA_TODO_GIT_DIRECTORY") "~/Projects")
  "Directory for creating git branch from todo."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-peer-code-review-prefix
  (or (getenv "JIRA_TODO_PEER_CODE_REVIEW_PREFIX") "")
  "Prefix to append after TODO text for a peer code review task."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-pr-reviewers
  (or (getenv "PULL_REQUEST_REVIEWERS") "")
  "GitHub PR reviewers for Slack or Teams message."
  :type 'string
  :group 'jira-todo)

(defcustom jira-todo-github-user-map nil
  "Alist mapping git author emails to PTAL display names.
Each element is a cons cell (EMAIL . DISPLAY-NAME), both strings.
DISPLAY-NAME is the mention to write, typically \"@Full Name\"."
  :type '(alist :key-type (string :tag "Git author email")
           :value-type (string :tag "Display name"))
  :group 'jira-todo)

(defun jira-todo--string-equal-fold (a b)
  "Return non-nil if A and B are equal, ignoring case."
  (and (stringp a) (stringp b)
    (string= (downcase a) (downcase b))))

(defun jira-todo--email-local-part (email)
  "Return EMAIL's local part, stripping a plus-address tag."
  (when (and (stringp email) (not (string-empty-p email)))
    (car (split-string (car (split-string email "@")) "\\+"))))

(defun jira-todo--email-plus-stripped (email)
  "Return EMAIL with a plus-address tag removed from the local part."
  (when (and (stringp email)
          (string-match "\\`\\([^@+]+\\)\\(?:\\+[^@]*\\)?@\\(.+\\)\\'" email))
    (concat (match-string 1 email) "@" (match-string 2 email))))

(defun jira-todo--coerce-map-value (val)
  "Return VAL as the display name from a user-map cdr.

VAL is the part after the `.` in (EMAIL . NAME), or the first
string in a two-element (EMAIL NAME) list.  Other values yield
nil."
  (cond
    ((stringp val) val)
    ((and (consp val) (stringp (car val))) (car val))))

(defun jira-todo--map-entry-name (cell)
  "Return the non-empty display name from map CELL, or nil.

CELL is (EMAIL . NAME) or (EMAIL NAME).  Empty NAME values mark
suppressed authors and are not returned here."
  (when-let* ((name (jira-todo--coerce-map-value (cdr cell))))
    (and (not (string-empty-p (string-trim name))) name)))

(defun jira-todo--identity-norms (key)
  "Return unique normalized identity strings for KEY.

KEY may be an email, a dotted local-part, or a display name.
Normalization is `jira-todo--normalize-ptal-name', so
\"xie.zirui\", \"zirui.xie@host\" and \"Zirui Xie\" share one
form."
  (let (norms)
    (when-let* ((local (jira-todo--email-local-part key)))
      (let ((n (jira-todo--normalize-ptal-name local)))
        (unless (or (null n) (string-empty-p n))
          (push n norms))))
    (when (stringp key)
      (let ((n (jira-todo--normalize-ptal-name key)))
        (unless (or (string-empty-p n) (member n norms))
          (push n norms))))
    (nreverse norms)))

(defun jira-todo--map-value-for-norms (norms)
  "Return the first map display name matching any of NORMS.

A map entry matches when its email local-part or its display
name, after `jira-todo--normalize-ptal-name', equals a member of
NORMS.  Empty (suppressed) display names are ignored."
  (when (and norms jira-todo-github-user-map)
    (cl-some
      (lambda (cell)
        (when-let* ((name (jira-todo--map-entry-name cell)))
          (and (or (cl-some
                     (lambda (n)
                       (string= n (jira-todo--normalize-ptal-name
                                    (or (jira-todo--email-local-part
                                          (car cell))
                                      ""))))
                     norms)
                 (cl-some
                   (lambda (n)
                     (string= n (jira-todo--normalize-ptal-name name)))
                   norms))
            name)))
      jira-todo-github-user-map)))

(defun jira-todo-github-user-map-get (key)
  "Return the PTAL display name for KEY, or nil if unset.

KEY is a git author email, a dotted local-part, or a display
name.  Lookup is case-insensitive.  A plus-address
\(user+tag@domain) also matches user@domain.  When that fails,
the local part is compared across domains so
gem.hung@levelblue.com matches gem.hung@cybereason.com.

Dotted or underscored tokens match in any order, so
\"zirui.xie@host\" and \"Zirui Xie\" both resolve to the mapped
name after the `.` for \"xie.zirui@cybereason.com\".  Two-element
lists (EMAIL NAME) are accepted as well as dotted pairs.

Return nil when the map is unset or KEY is not present."
  (when (and key jira-todo-github-user-map)
    (or (jira-todo--coerce-map-value
          (alist-get key
            jira-todo-github-user-map
            nil nil #'jira-todo--string-equal-fold))
      (let ((stripped (jira-todo--email-plus-stripped key)))
        (and stripped
          (not (jira-todo--string-equal-fold stripped key))
          (jira-todo--coerce-map-value
            (alist-get stripped
              jira-todo-github-user-map
              nil nil #'jira-todo--string-equal-fold))))
      (let ((local (jira-todo--email-local-part key)))
        (when (and local (not (string-empty-p local)))
          (cl-some (lambda (cell)
                     (and (jira-todo--string-equal-fold
                            local (jira-todo--email-local-part (car cell)))
                       (jira-todo--coerce-map-value (cdr cell))))
            jira-todo-github-user-map)))
      (jira-todo--map-value-for-norms (jira-todo--identity-norms key)))))

(defun jira-todo--author-suppressed-p (author-string)
  "Return non-nil if AUTHOR-STRING is mapped to an empty display name.

A `jira-todo-github-user-map' entry such as
\(\"daniel.hay@cybereason.com\" . \"\") means the person must never
be listed or counted as a PTAL reviewer.  AUTHOR-STRING is anything
`jira-todo--author-email' accepts.  Matching uses the same rules as
`jira-todo-github-user-map-get'."
  (when-let* ((email (jira-todo--author-email
                       (if (consp author-string) (car author-string) author-string)))
               (mapped (jira-todo-github-user-map-get email)))
    (and (stringp mapped)
      (string-empty-p (string-trim mapped)))))

(defun jira-todo--remove-suppressed-authors (entries)
  "Drop ENTRIES (AUTHOR-STRING . LINES) whose author is mapped to \"\"."
  (cl-remove-if (lambda (entry)
                  (jira-todo--author-suppressed-p (car entry)))
    entries))

(defun jira-todo-github-user-map-set (key display-name)
  "Set DISPLAY-NAME for KEY in the mapping, adding or updating."
  (setf (alist-get key
          jira-todo-github-user-map
          nil nil #'jira-todo--string-equal-fold)
    display-name))

(defun jira-todo-github-user-map-remove (key)
  "Remove KEY from the mapping, if present."
  (setf (alist-get key
          jira-todo-github-user-map
          nil 'remove #'jira-todo--string-equal-fold)
    nil))

(defun jira-todo--rest-url (issue-number)
  "Return the JIRA REST API URL for ISSUE-NUMBER."
  (format "%s/rest/api/3/issue/%s-%s"
    jira-todo-base-url
    jira-todo-issue-key-prefix
    issue-number))

(defun jira-todo--browse-url (key)
  "Return the JIRA browse URL for KEY."
  (format "%s/browse/%s" jira-todo-base-url key))

(defun jira-todo--auth-header ()
  "Return the Basic Auth header value."
  (concat "Basic "
    (base64-encode-string
      (concat jira-todo-username ":" jira-todo-token)
      t)))

(defun jira-todo--smart-open-line-above ()
  "Insert an empty line above the current line and indent it.
If in an `org-mode' buffer within a TODO, move point to a new line
immediately above the first sibling TODO under the parent heading."
  (if (derived-mode-p 'org-mode)
    (let ((target nil)
           (current-level (save-excursion
                            (org-back-to-heading t)
                            (org-current-level))))
      (save-excursion
        ;; Go up to parent heading
        (org-back-to-heading t)
        (when (org-up-heading-safe)
          (let ((parent-end (save-excursion (org-end-of-subtree t t))))
            (while (and (not target)
                     (re-search-forward org-heading-regexp parent-end t))
              (when (and (= (org-current-level) current-level)
                      (org-entry-is-todo-p))
                (setq target (line-beginning-position)))))))
      (if target
        (progn
          (goto-char target)
          (open-line 1))
        ;; Fallback: original behavior
        (move-beginning-of-line nil)
        (newline-and-indent)
        (forward-line -1)
        (indent-according-to-mode)))
    ;; Not in org-mode: original behavior
    (move-beginning-of-line nil)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)))

(defconst jira-todo--teams-heredoc-delimiter "JIRA-TODO-MSG"
  "Here-document delimiter used by generated Teams `xteams' blocks.")

(defun jira-todo--autolink-urls (text)
  "Wrap the bare URLs in TEXT as CommonMark autolinks, `<URL>'.

`xteams' builds the Teams HTML from CommonMark, which does not
autolink bare URLs (its `pulldown-cmark' pass enables only tables,
strikethrough, and task lists), so Teams would store them as plain
text.  URLs already inside `<>' are left alone.  Return
\(NEW-TEXT . COUNT)."
  (let ((count 0))
    (cons
      (replace-regexp-in-string
        "\\(^\\|[^<]\\)\\(https?://[^[:space:]<>]+\\)"
        (lambda (m)
          (setq count (1+ count))
          (concat (match-string 1 m) "<" (match-string 2 m) ">"))
        (or text ""))
      count)))

(defun jira-todo--teams-mention (name)
  "Return NAME as a Microsoft Teams mention token, `@{NAME}'.

A leading `@' is dropped, and NAME already in `@{...}' form is
returned unchanged.  `xteams' turns these tokens into both the
mention markup and the `properties.mentions' metadata Teams needs
for the mention to highlight and notify."
  (let ((name (string-trim (or name ""))))
    (cond
      ((string-empty-p name) "")
      ((string-match-p "\\`@{[^}]*}\\'" name) name)
      (t (format "@{%s}" (replace-regexp-in-string "\\`@+" "" name))))))

(defun jira-todo--teams-mention-string (&optional reviewers)
  "Return REVIEWERS as space-separated Teams `@{NAME}' mentions.

REVIEWERS defaults to `jira-todo-pr-reviewers'.  It is split into
names by `jira-todo--split-pr-reviewer-names', so names separated
by commas and/or `@' are encoded one by one and multi-word names
stay intact."
  (mapconcat #'jira-todo--teams-mention
    (jira-todo--split-pr-reviewer-names
      (or reviewers jira-todo-pr-reviewers))
    " "))

(defun jira-todo--teams-mention-names (names)
  "Return NAMES (\"@Ada Lovelace\") as space-separated `@{...}' mentions."
  (mapconcat #'jira-todo--teams-mention names " "))

(defun jira-todo--format-slack-message (summary)
  "Format message with SUMMARY for Slack."
  (concat
    (format "Slack:\n")
    (format "--begin--\n")
    (format ":pull_request: PTAL %s\n" jira-todo-pr-reviewers)
    (format "<PR-TBD>\n")
    (format "%s\n" summary)
    (format "--end--\n")))

(defun jira-todo--format-teams-message (summary)
  "Format an org-babel block that posts SUMMARY to Microsoft Teams.

The block feeds the message body to `xteams message new' on
standard input, so evaluating it (\\[org-ctrl-c-ctrl-c] inside the
block) sends the message to `jira-todo-microsoft-teams-channel'.
Reviewer names are encoded as `@{NAME}' mention tokens and URLs in
SUMMARY are wrapped as CommonMark autolinks by
`jira-todo--autolink-urls'."
  (concat
    (format "Teams:\n")
    (format "#+begin_src sh :results none\n")
    (format "xteams message new %s <<'%s'\n"
      (shell-quote-argument jira-todo-microsoft-teams-channel)
      jira-todo--teams-heredoc-delimiter)
    (format "PTAL %s\\\n" (jira-todo--teams-mention-string))
    (format "[<PR-TBD>](<PR-TBD>)\\\n")
    (format "%s\n" (car (jira-todo--autolink-urls summary)))
    (format "%s\n" jira-todo--teams-heredoc-delimiter)
    (format "#+end_src\n")))

(defun jira-todo--format-output (data)
  "Format `org-mode' TODO and message from parsed JIRA DATA.
Also copies the prompt block via `caveman-copy-region'."
  (let* ((key             (format "%s" (alist-get 'key data)))
          (fields          (alist-get 'fields data))
          (summary         (format "%s" (alist-get 'summary fields)))
          (url             (jira-todo--key-to-browse-url key))
          (clean-summary   (replace-regexp-in-string "\\[[A-Z]+\\][ ]*" "" summary))
          (branch-words    (replace-regexp-in-string "[^A-Za-z0-9]+" "-" clean-summary))
          (branch-compact  (replace-regexp-in-string "-+" "-" branch-words))
          (branch-trimmed  (replace-regexp-in-string "-+$" "" branch-compact))
          (branch-summ     (downcase branch-trimmed))
          (branch          (git-tools-normalize-branch-name
                             (format "%s_%s" key branch-summ)))
          (output
            (concat
              (format "*** TODO CR: %s %s\n" key clean-summary)
              (format "JIRA: [[%s][%s]]\n" url key)
              (format "Branch: %s\n" branch)
              (format "Git Directory: %s\n" jira-todo-git-directory)
              (format "Prompt:\n")
              (format "--begin--\n")
              (format "Under the %s directory in my current branch %s" jira-todo-git-directory branch)
              (format " please implement the JIRA at %s " url)
              (format "and amend commit the change to my existing commit with this same ticket.\n")
              (format "--end--\n")
              (format "Title: %s: %s\n" key clean-summary)
              (format "PR: <PR-TBD>\n")
              (format "PR Text:\n")
              (format "--begin--\n")
              (format "## JIRA\n")
              (format "[%s](%s)\n" key url)
              (format "## Description\n")
              (format "%s\n" clean-summary)
              (format "--end--\n")
              (pcase jira-todo-pr-messaging-provider
                ("slack" (jira-todo--format-slack-message clean-summary))
                ("teams" (jira-todo--format-teams-message clean-summary))
                (_ ""))
              (format ":LOGBOOK:\n")
              (format ":END:"))))
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (when (re-search-forward "^--begin--\n" nil t)
        (let ((start (point)))
          (when (re-search-forward "^--end--" nil t)
            (caveman-copy-region start (match-beginning 0))))))
    output))

(defun jira-todo--parse-labeled-fields (text)
  "Parse TEXT for lines of the form \"Label: value\".
Return an alist of (LABEL . VALUE), both trimmed strings.  LABEL is
everything before the first colon on the line; VALUE is everything
after the first \": \" that follows it.  If a label appears more than
once, the first occurrence wins."
  (let ((fields nil)
         (case-fold-search nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*\\([^:\n]+\\): \\(.+\\)$" nil t)
        (let ((label (string-trim (match-string 1)))
               (value (string-trim (match-string 2))))
          (unless (assoc label fields)
            (push (cons label value) fields)))))
    (nreverse fields)))

(defun jira-todo--insert-todo-entry (text)
  "Insert TEXT as a new TODO entry above the first sibling TODO.

Leave point on the inserted heading.  Signal unless the current
buffer is in `org-mode', because TEXT is inserted as a heading."
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be called from an org-mode TODO"))
  (jira-todo--smart-open-line-above)
  (let ((start (point)))
    (insert text)
    (goto-char start)
    (org-back-to-heading t))
  (when (fboundp 'evil-force-normal-state)
    (evil-force-normal-state))
  text)

(defun jira-todo--insert-output (data)
  "Insert formatted `org-mode' TODO for JIRA DATA at point in the current buffer."
  (let ((output (jira-todo--format-output data)))
    (jira-todo--insert-todo-entry output)
    (let* ((fields (jira-todo--parse-labeled-fields output))
            (branch (cdr (assoc "Branch" fields)))
            (title (cdr (assoc "Title" fields))))
      (cond
        ((or (null branch) (string-empty-p branch))
          (message "You must manually create branch, could not identify name."))
        ((git-tools-branch-create-from-main branch jira-todo-git-directory)
          (git-tools-empty-commit-message title jira-todo-git-directory))))))

(defun jira-todo--parse-input (input)
  "Parse INPUT into a JIRA key.

INPUT can be in any of the following forms:
- Full URL:  https://atlassian.net/browse/JIRA-11111
- Full key:  JIRA-11111
- Number:    11111

Returns the JIRA key as a string, e.g. JIRA-11111.
Signals an error if INPUT cannot be parsed."
  (cond
    ;; Full URL: https://atlassian.net/browse/JIRA-11111
    ((string-match "/browse/\\([A-Z]+-[0-9]+\\)" input)
      (match-string 1 input))
    ;; Full key: JIRA-11111
    ((string-match "^\\([A-Z]+-[0-9]+\\)$" input)
      (match-string 1 input))
    ;; Just a number: 11111
    ((string-match "^[0-9]+$" input)
      (format "%s-%s" jira-todo-issue-key-prefix input))
    (t
      (error "Could not parse JIRA issue from input: %s" input))))

(defun jira-todo--key-to-rest-url (key)
  "Return the JIRA REST API URL for KEY (e.g. JIRA-11111)."
  (format "%s/rest/api/3/issue/%s" jira-todo-base-url key))

(defun jira-todo--key-to-browse-url (key)
  "Return the JIRA browse URL for KEY."
  (format "%s/browse/%s" jira-todo-base-url key))

(defun jira-todo--clipboard-text ()
  "Return trimmed kill-ring/clipboard text, or nil if unavailable."
  (let ((text (ignore-errors (current-kill 0 t))))
    (when (and (stringp text) (not (string-empty-p (string-trim text))))
      (string-trim text))))

(defun jira-todo--http-url-p (text)
  "Return non-nil if TEXT is a single bare http(s) URL."
  (and (stringp text)
    (string-match-p "\\`https?://[^[:space:]]+\\'" text)))

(defun jira-todo--clipboard-jira-url ()
  "Return clipboard text when it names a JIRA issue, otherwise nil.

Accepts a browse URL, a key such as JIRA-11111, or a bare issue number."
  (let ((text (jira-todo--clipboard-text)))
    (when (and text (ignore-errors (jira-todo--parse-input text)))
      text)))

(defun jira-todo--clipboard-pr-url ()
  "Return the clipboard text when it is a non-JIRA http(s) URL, otherwise nil."
  (let ((url (jira-todo--clipboard-text)))
    (when (and (jira-todo--http-url-p url)
            (not (string-search jira-todo-issue-key-prefix url)))
      url)))

(defun jira-todo--heading-bounds ()
  "Return (START . END) of the current org heading subtree.

END is a marker.  Includes folded/invisible text."
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be called from an org-mode TODO"))
  (save-excursion
    (org-back-to-heading t)
    (cons (point)
      (copy-marker (save-excursion (org-end-of-subtree t t) (point))))))

(defun jira-todo--heading-text ()
  "Return the current org heading and its subtree as a string."
  (let* ((bounds (jira-todo--heading-bounds))
          (text (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (set-marker (cdr bounds) nil)
    text))

(defun jira-todo--replace-heading-text (new-text)
  "Replace the current org heading subtree with NEW-TEXT.
Reveal the heading so the edit is visible.  Return NEW-TEXT."
  (let ((bounds (jira-todo--heading-bounds))
         (search-invisible t))
    (save-excursion
      (delete-region (car bounds) (cdr bounds))
      (goto-char (car bounds))
      (insert new-text)
      (set-marker (cdr bounds) nil))
    (save-excursion
      (org-back-to-heading t)
      (cond
        ((fboundp 'org-fold-show-subtree) (org-fold-show-subtree))
        ((fboundp 'org-show-subtree) (org-show-subtree))
        (t (org-fold-show-entry))))
    new-text))

(defun jira-todo--heading-fields ()
  "Return labeled fields from the current org heading."
  (jira-todo--parse-labeled-fields (jira-todo--heading-text)))

(defun jira-todo--heading-field (label &optional fields)
  "Return the trimmed value of LABEL from FIELDS or the current heading."
  (let ((value (cdr (assoc label (or fields (jira-todo--heading-fields))))))
    (and (stringp value) (not (string-empty-p value)) value)))

(defun jira-todo--first-pr-url-in-text (text)
  "Return the first GitHub-style pull-request URL in TEXT, or nil."
  (when (and (stringp text)
          (string-match "https?://[^[:space:]]+/pull/[0-9]+" text))
    (match-string 0 text)))

(defun jira-todo--pr-url-owner-repo (url)
  "Return (OWNER . REPO) parsed from a pull-request URL, or nil."
  (when (and (stringp url)
          (string-match
            "/\\([^/]+\\)/\\([^/]+\\)/pull/[0-9]+"
            url))
    (cons (match-string 1 url)
      (replace-regexp-in-string "\\.git\\'" "" (match-string 2 url)))))

(defun jira-todo--heading-pr-url (&optional fields)
  "Return an already-filled PR URL with optional FIELDS from the current heading.

Prefers the PR labeled field, then the first /pull/ URL in the
heading text (Teams/Slack message body)."
  (or (let ((pr (jira-todo--heading-field "PR" fields)))
        (when (and pr
                (jira-todo--http-url-p pr)
                (not (string-match-p "<\\(PR-\\)?TBD>" pr)))
          pr))
    (jira-todo--first-pr-url-in-text
      (ignore-errors (jira-todo--heading-text)))))

(defun jira-todo--origin-matches-p (dir owner repo)
  "Return non-nil if DIR's origin is OWNER/REPO."
  (when-let* ((pair (ignore-errors
                      (git-tools--pr-owner-repo
                        (file-name-as-directory (file-truename dir))))))
    (and (string= (downcase (car pair)) (downcase owner))
      (string= (downcase (cdr pair)) (downcase repo)))))

(defun jira-todo--repo-has-branch-p (dir branch)
  "Return non-nil if DIR has BRANCH or origin/BRANCH."
  (let ((default-directory (file-name-as-directory dir)))
    (or (magit-branch-p branch)
      (magit-branch-p (concat "origin/" branch)))))

(defun jira-todo--git-directory-search-roots ()
  "Return directories to search for a matching local clone.

Only `jira-todo-git-directory' is searched directly.  The parallel
review clones under `git-tools-review-home' are added by
`jira-todo--directory-candidates' through
`git-tools-review-clone-candidates'.  The current buffer directory
is not searched, so a random repo (for example the org notes tree)
cannot win over the configured project roots."
  (let ((root (and (stringp jira-todo-git-directory)
                (not (string-empty-p jira-todo-git-directory))
                (expand-file-name jira-todo-git-directory))))
    (when root
      (list (directory-file-name root)))))

(defun jira-todo--directory-candidates (owner repo)
  "Return local directories that might be a clone of OWNER/REPO."
  (let (candidates)
    (dolist (root (jira-todo--git-directory-search-roots))
      (when (file-directory-p root)
        (push root candidates)
        (when (and repo (not (string-empty-p repo)))
          (dolist (name (cl-delete-duplicates
                          (list repo
                            (capitalize repo)
                            (upcase repo))
                          :test #'string=))
            (let ((child (expand-file-name name root)))
              (when (file-directory-p child)
                (push (directory-file-name child) candidates)))))
        (unless (git-tools--git-repo-p root)
          (dolist (child (directory-files root t "\\`[^.]"))
            (when (file-directory-p child)
              (push (directory-file-name child) candidates))))))
    (dolist (dir (git-tools-review-clone-candidates owner repo))
      (push dir candidates))
    (cl-delete-duplicates candidates :test #'file-equal-p)))

(defun jira-todo--find-repo-from-remote (owner repo branch)
  "Return a local clone matching OWNER/REPO and/or remote BRANCH.

Prefers a directory whose origin is OWNER/REPO and that has
BRANCH or origin/BRANCH.  Then origin only, then a repo that
has the remote branch.  Returns nil when nothing matches."
  (let (both origin-match branch-match)
    (dolist (dir (jira-todo--directory-candidates owner repo))
      (when (git-tools--git-repo-p dir)
        (let* ((origin-ok (and owner repo
                            (jira-todo--origin-matches-p dir owner repo)))
                (branch-ok (and branch
                             (not (string-empty-p branch))
                             (jira-todo--repo-has-branch-p dir branch))))
          (cond
            ((and origin-ok branch-ok) (setq both dir))
            ((and origin-ok (not origin-match)) (setq origin-match dir))
            ((and branch-ok (not branch-match)) (setq branch-match dir))))))
    (when-let* ((chosen (or both origin-match branch-match)))
      (directory-file-name (file-truename chosen)))))

(defun jira-todo--git-directory (&optional fields)
  "Return the git directory for the current heading with optional FIELDS.

Git Directory is optional.  Resolution order:
1. The heading Git Directory field, when present.
2. A local clone matching the PR URL's owner/repo and/or the
   heading Branch field (local or origin/BRANCH).
3. `jira-todo-git-directory'."
  (let* ((fields (or fields (ignore-errors (jira-todo--heading-fields))))
          (explicit (jira-todo--heading-field "Git Directory" fields))
          (pr (or (jira-todo--heading-pr-url fields)
                (ignore-errors (jira-todo--clipboard-pr-url))))
          (owner-repo (jira-todo--pr-url-owner-repo pr))
          (branch (jira-todo--heading-branch fields))
          (found (unless explicit
                   (jira-todo--find-repo-from-remote
                     (car owner-repo) (cdr owner-repo) branch))))
    (file-name-as-directory
      (expand-file-name
        (git-tools--ensure-project-directory
          (or explicit found jira-todo-git-directory))))))

(defun jira-todo--heading-branch (&optional fields)
  "Return the Branch field from the current heading, or nil with optional FIELDS."
  (jira-todo--heading-field "Branch" fields))

(defun jira-todo--fetch-remote-branch (branch root)
  "Fetch BRANCH from origin into ROOT.  Return non-nil on success."
  (let ((default-directory (file-name-as-directory root)))
    (zerop (call-process "git" nil nil nil "fetch" "origin" branch))))

(defun jira-todo--branch-rev (branch root)
  "Return a git revision for BRANCH in ROOT.

Prefer the local branch, then origin/BRANCH.  Fetch origin when
neither exists yet.  Return nil to mean HEAD."
  (let* ((default-directory (file-name-as-directory root))
          (origin (and branch (concat "origin/" branch)))
          (local-p (and branch (magit-branch-p branch)))
          (remote-p (and origin (magit-branch-p origin))))
    (cond
      (local-p branch)
      (remote-p origin)
      ((and branch (jira-todo--fetch-remote-branch branch root))
        (cond
          ((magit-branch-p branch) branch)
          ((magit-branch-p origin) origin)))
      (t nil))))

(defun jira-todo--resolve-pr-url (&optional url)
  "Return URL, else a clipboard PR URL, else the heading PR, else prompt.
Empty strings are treated as omitted so clipboard/heading/prompt still run.
When the current heading already has a PR URL, do not prompt."
  (let ((url (and (stringp url) (string-trim url))))
    (cond
      ((and url (not (string-empty-p url))) url)
      ((jira-todo--clipboard-pr-url))
      ((ignore-errors (jira-todo--heading-pr-url)))
      (t (let ((typed (string-trim (read-string "PR URL: "))))
           (when (string-empty-p typed)
             (user-error "No PR URL provided"))
           typed)))))

(defun jira-todo--pr-title-via-gh (owner repo number)
  "Return PR NUMBER's title in OWNER/REPO via the `gh' CLI, or nil."
  (when (executable-find "gh")
    (with-temp-buffer
      (when (zerop (ignore-errors
                     (call-process "gh" nil t nil
                       "pr" "view" (format "%s" number)
                       "--repo" (format "%s/%s" owner repo)
                       "--json" "title"
                       "-q" ".title")))
        (let ((title (string-trim (buffer-string))))
          (unless (string-empty-p title) title))))))

(defun jira-todo--pr-title-via-api (owner repo number)
  "Return PR NUMBER's title in OWNER/REPO via the GitHub REST API, or nil."
  (condition-case nil
    (let (result)
      (with-current-buffer
        (url-retrieve-synchronously
          (format "https://api.github.com/repos/%s/%s/pulls/%s"
            owner repo number)
          t t 10)
        (goto-char (point-min))
        (when (re-search-forward "\n\n" nil t)
          (let* ((json-object-type 'alist)
                  (data (json-read))
                  (title (alist-get 'title data)))
            (when (stringp title) (setq result title))))
        (kill-buffer))
      result)
    (error nil)))

(defconst jira-todo--ticket-prefix-regexp
  (concat "\\`\\(?:"
    "\\[[A-Za-z][A-Za-z0-9 _-]*\\][ \t]*"           ; "[ENG-1234] "
    "\\|[A-Za-z][A-Za-z0-9]*-[0-9]+[ \t]*:[ \t]*"   ; "ENG-1234: "
    "\\)")
  "Regexp matching one leading ticket prefix in a title or summary.

Prefixes are the bracketed form (\"[ENG-1234] \") and the ticket
key with a colon (\"ENG-1234: \"), either of which may appear more
than once.")

(defconst jira-todo--commit-type-prefix-regexp
  (concat "\\`\\(?:"
    "build\\|chore\\|ci\\|docs\\|feat\\|fix\\|perf\\|refactor"
    "\\|revert\\|style\\|test"
    "\\)\\(?:([^)]+)\\)?!?:[ \t]*")   ; "feat: ", "fix(api)!: "
  "Regexp matching one leading conventional-commit prefix in a title.

Matches a conventional-commit type followed by an optional
parenthesised scope, an optional `!', and the `\": \"' separator,
for example \"feat: \" or \"fix(api)!: \".")

(defun jira-todo--strip-title-prefix (text)
  "Return TEXT with any leading ticket or commit-type prefix removed.

Strips every leading prefix matched by
`jira-todo--ticket-prefix-regexp' or
`jira-todo--commit-type-prefix-regexp', including the `\": \"'
that separates a ticket key or commit type from the summary, and
repeats until no prefix remains.  Surrounding whitespace is
trimmed.  TEXT without a prefix is returned trimmed."
  (if (not (stringp text))
    text
    (let ((prev nil)
           (text (string-trim text))
           (regexps (list jira-todo--ticket-prefix-regexp
                      jira-todo--commit-type-prefix-regexp)))
      (while (not (equal prev text))
        (setq prev text)
        (dolist (regexp regexps)
          (setq text (replace-regexp-in-string regexp "" text))))
      (string-trim text))))

(defun jira-todo--pr-title (url)
  "Return the summary of the pull request at URL, or nil.

Tries the `gh' CLI first, then the GitHub REST API.  The PR title
is returned with any leading ticket or conventional-commit prefix
removed.  Returns nil when URL is not a GitHub pull-request URL,
both lookups fail, or only a prefix was found."
  (when-let* ((number (git-tools--github-pr-number url))
               (owner-repo (jira-todo--pr-url-owner-repo url))
               (owner (car owner-repo))
               (repo (cdr owner-repo))
               (title (or (jira-todo--pr-title-via-gh owner repo number)
                        (jira-todo--pr-title-via-api owner repo number)))
               (summary (jira-todo--strip-title-prefix title))
               ((not (string-empty-p summary))))
    summary))

(defun jira-todo--replace-pr-placeholders (url)
  "Replace <PR-TBD> and <TBD> placeholders in the current org heading with URL.

Each <PR-TBD> gets URL with the pull request title summary
`(ticket and conventional-commit prefixes removed) inserted on the
next line when one can be retrieved; a bare <TBD> gets URL only.
Return the number of replacements, which may be zero when the PR
URL is already filled in.  Signal if point is not in an org heading.
Works even when the subtree is folded."
  (let* ((text (jira-todo--heading-text))
          (title (jira-todo--pr-title url))
          (replacement (if title (concat url "\n" title) url))
          (count 0)
          (new (replace-regexp-in-string
                 "<\\(PR-\\)?TBD>"
                 (lambda (match)
                   (setq count (1+ count))
                   (if (string-prefix-p "<PR-" match) replacement url))
                 text t t)))
    (when (> count 0)
      (jira-todo--replace-heading-text new))
    count))

(defun jira-todo--username-from-email (email)
  "Return a git/GitHub username derived from EMAIL."
  (let ((local (car (split-string email "@"))))
    (if (string-match-p "\\+" local)
      (car (last (split-string local "\\+")))
      local)))

(defun jira-todo--author-email (author-string)
  "Extract an email from AUTHOR-STRING.

Accepts \"Name <email>\", \"email (name)\", or a bare email."
  (cond
    ((and (stringp author-string)
       (string-match "<\\([^<>[:space:]]+@[^<>[:space:]]+\\)>" author-string))
      (string-trim (match-string 1 author-string)))
    ((and (stringp author-string)
       (string-match "\\`\\([^[:space:]]+@[^[:space:]]+\\)" author-string))
      (string-trim (match-string 1 author-string)))
    ((and (stringp author-string)
       (string-match "\\([^[:space:]]+@[^[:space:]]+\\)" author-string))
      (string-trim (match-string 1 author-string)))))

(defun jira-todo--author-username (author-string)
  "Extract a git username from AUTHOR-STRING."
  (if-let* ((email (jira-todo--author-email author-string)))
    (jira-todo--username-from-email email)
    author-string))

(defun jira-todo--author-display-name (author-string)
  "Return a human display name from AUTHOR-STRING, or nil."
  (cond
    ((and (stringp author-string)
       (string-match "\\`\\([^[:space:]]+\\) (\\(.*\\))\\'" author-string))
      (let ((name (string-trim (match-string 2 author-string))))
        (unless (string-empty-p name) name)))
    ((and (stringp author-string)
       (string-match "\\`\\(.+\\)[ \t]+<[^>]+>\\'" author-string))
      (let ((name (string-trim (match-string 1 author-string))))
        (unless (string-empty-p name) name)))))

(defun jira-todo--ensure-at-mention (name)
  "Return NAME with a leading `@' and surrounding whitespace trimmed."
  (let ((name (string-trim (or name ""))))
    (cond
      ((string-empty-p name) nil)
      ((string-prefix-p "@" name) name)
      (t (concat "@" name)))))

(defun jira-todo--mapped-display-name (author)
  "Return the mapped PTAL display name for AUTHOR, or nil.

AUTHOR may be a git author email or a full author string
accepted by `jira-todo--author-email'.  Lookup uses
`jira-todo-github-user-map': the email (case-insensitive,
plus-tags and domain ignored), then a dotted local-part or
display name compared with `jira-todo--normalize-ptal-name' so
\"zirui.xie@host\" and \"Zirui Xie\" both resolve to the name
after the `.` when that map value is \"@Xie Zirui\".  A leading
`@' is added when the map value does not already have one."
  (when (and author jira-todo-github-user-map)
    (let* ((author (if (consp author) (car author) author))
            (email (jira-todo--author-email author))
            (display (jira-todo--author-display-name author))
            (mapped (or (and email (jira-todo-github-user-map-get email))
                      (and display (jira-todo-github-user-map-get display))
                      (and (stringp author)
                        (jira-todo-github-user-map-get author)))))
      (and mapped (jira-todo--ensure-at-mention mapped)))))

(defun jira-todo--format-ptal-mention (author-string)
  "Format AUTHOR-STRING for a PTAL mention.

If AUTHOR-STRING matches a `jira-todo-github-user-map' entry by
email, dotted local-part, or display name, return that mapped
name and never the git spelling.  Accepts \"Name <email>\",
\"@Name <email>\", \"email (name)\", a bare email, or a name
such as \"Zirui Xie\".

If the map is unset or has no entry, use the original email
name: the name that accompanies the email, without the address.
The result always has a leading `@'."
  (when (consp author-string)
    (setq author-string (car author-string)))
  (let* ((author-string (and (stringp author-string) author-string))
          (email (jira-todo--author-email author-string))
          (mapped (jira-todo--mapped-display-name author-string))
          (original (jira-todo--author-display-name author-string)))
    (jira-todo--ensure-at-mention
      (or mapped original email author-string))))

(defun jira-todo--apply-email-map-to-text (text)
  "Replace mapped emails in TEXT with `jira-todo-github-user-map' values.

Rewrites \"@Name <email>\", \"Name <email>\", \"email (name)\",
and leftover bare emails.  Unmapped emails are left unchanged."
  (let ((text (or text "")))
    (dolist (email (jira-todo--emails-in-text text))
      (when-let* ((mapped (jira-todo--mapped-display-name email)))
        (setq text (jira-todo--replace-mapped-email text email mapped))))
    text))

(defun jira-todo--emails-in-text (text)
  "Return unique email addresses found in TEXT."
  (let ((text (or text ""))
         emails start)
    (while (string-match
             "\\([A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]\\{2,\\}\\)"
             text (or start 0))
      (let ((email (match-string 1 text)))
        (unless (member (downcase email) (mapcar #'downcase emails))
          (push email emails))
        (setq start (match-end 0))))
    (nreverse emails)))

(defun jira-todo--replace-mapped-email (text email mapped)
  "Replace EMAIL (and its surrounding name) in TEXT with MAPPED."
  (let ((q (regexp-quote email)))
    (dolist (re (list
                  (concat "@[^@<\n]+[ \t]+<" q ">")
                  (concat "[^@<\n]+[ \t]+<" q ">")
                  (concat "<" q ">")
                  (concat q "[ \t]+([^)]*)")
                  q))
      (setq text (replace-regexp-in-string re mapped text t t)))
    text))

(defun jira-todo--effective-pr-reviewers ()
  "Return reviewer text from the custom or the environment.

GUI Emacs often lacks `PULL_REQUEST_REVIEWERS' at load time
because it is not copied by `exec-path-from-shell'.  Re-read it
here, also accepting `GITHUB_PULL_REQUEST_REVIEWERS'."
  (let ((custom (and (stringp jira-todo-pr-reviewers)
                  (not (string-empty-p jira-todo-pr-reviewers))
                  jira-todo-pr-reviewers)))
    (or custom
      (getenv "PULL_REQUEST_REVIEWERS")
      (getenv "GITHUB_PULL_REQUEST_REVIEWERS")
      "")))

(defun jira-todo--split-at-mentions (text)
  "Split TEXT on `@' mentions, trimming each name.

A leading token without `@' is kept.  Mentions run until the
next `@' so names such as \"@Xin Tang\" stay intact."
  (let ((text (string-trim (or text "")))
         names)
    (unless (string-empty-p text)
      (when (string-match "\\`\\([^@]+\\)" text)
        (let ((lead (string-trim (match-string 1 text))))
          (unless (string-empty-p lead)
            (push lead names)))
        (setq text (substring text (match-end 0))))
      (while (string-match "\\`@\\([^@]+\\)" text)
        (let ((name (string-trim (match-string 1 text))))
          (unless (string-empty-p name)
            (push (concat "@" name) names)))
        (setq text (substring text (match-end 0)))))
    (nreverse names)))

(defun jira-todo--split-pr-reviewer-names (&optional reviewers)
  "Split REVIEWERS into display-name tokens.

REVIEWERS defaults to `jira-todo--effective-pr-reviewers'
\(`PULL_REQUEST_REVIEWERS' / `GITHUB_PULL_REQUEST_REVIEWERS').
Names may be separated by commas and/or `@' mentions.  Each
token is trimmed.  Emails are rewritten via
`jira-todo-github-user-map' when present."
  (let* ((text (string-trim (or reviewers (jira-todo--effective-pr-reviewers) "")))
          names)
    (unless (string-empty-p text)
      (dolist (chunk (split-string text "," t))
        (let ((chunk (string-trim chunk)))
          (unless (string-empty-p chunk)
            (setq names
              (nconc names
                (if (string-match-p "@" chunk)
                  (jira-todo--split-at-mentions chunk)
                  (list chunk))))))))
    (delq nil
      (mapcar (lambda (name)
                (let* ((name (string-trim name))
                        (mapped (jira-todo--mapped-display-name name)))
                  (jira-todo--ensure-at-mention (or mapped name))))
        names))))

(defun jira-todo--normalize-ptal-name (name)
  "Normalize NAME for uniqueness comparison.

Trims, drops a leading `@', treats `.' and `_' as spaces,
collapses whitespace, downcases, and sorts name tokens.  So
\"@Shashank Vangari\" and \"@Shashank.Vangari\" compare equal,
and \"@Xie Zirui\" matches \"@Zirui Xie\"."
  (let ((name (string-trim (or name ""))))
    (setq name (replace-regexp-in-string "\\`@" "" name))
    (setq name (replace-regexp-in-string "[._]+" " " name))
    (setq name (replace-regexp-in-string "[ \t]+" " " name))
    (setq name (downcase (string-trim name)))
    (mapconcat #'identity (sort (split-string name) #'string-lessp) " ")))

(defun jira-todo--merge-ptal-names (auto-names extra-names)
  "Return AUTO-NAMES followed by EXTRA-NAMES not already rendered.

Each display name appears once.  Comparison is case-insensitive
and ignores a leading `@' and `.'/ `_' separators.  AUTO-NAMES
keep their original order; EXTRA-NAMES that are new are appended
in their original order."
  (let ((seen (make-hash-table :test #'equal))
         merged)
    (dolist (name (append auto-names extra-names))
      (let* ((name (jira-todo--ensure-at-mention name))
              (key (jira-todo--normalize-ptal-name name)))
        (unless (or (null name) (string-empty-p key) (gethash key seen))
          (puthash key t seen)
          (push name merged))))
    (nreverse merged)))

(defun jira-todo--author-identity-keys (author-string)
  "Return identity keys used to merge AUTHOR-STRING with aliases.

Keys are drawn from the mapped PTAL name, the git display name,
and the email local-part (plus-tags and case ignored)."
  (let* ((email (jira-todo--author-email author-string))
          (mapped (jira-todo--mapped-display-name author-string))
          (display (jira-todo--author-display-name author-string))
          keys)
    (when mapped
      (push (concat "n:" (jira-todo--normalize-ptal-name mapped)) keys))
    (when display
      (push (concat "n:" (jira-todo--normalize-ptal-name display)) keys))
    (when-let* ((local (jira-todo--email-local-part email)))
      (push (concat "l:" (downcase local)) keys))
    keys))

(defun jira-todo--self-identity-keys (&optional root)
  "Return identity keys for the current git user in ROOT."
  (let* ((default-directory (file-name-as-directory
                              (or root default-directory)))
          (git-email (ignore-errors (git-tools-git-config-value "user.email")))
          (git-name (ignore-errors (git-tools-git-config-value "user.name")))
          (email (or git-email user-mail-address))
          (name (or git-name user-full-name)))
    (jira-todo--author-identity-keys
      (git-tools--author-display name email))))

(defun jira-todo--identity-keys-overlap-p (a b)
  "Return non-nil if identity key lists A and B share an element."
  (cl-some (lambda (k) (member k b)) a))

(defun jira-todo--choose-author-string (a a-lines b b-lines)
  "Pick a representative author string from A A-LINES and B B-LINES.
Prefer a mapped PTAL email; otherwise the string with more lines."
  (let ((a-mapped (jira-todo--mapped-display-name a))
         (b-mapped (jira-todo--mapped-display-name b)))
    (cond
      ((and a-mapped (not b-mapped)) a)
      ((and b-mapped (not a-mapped)) b)
      ((> b-lines a-lines) b)
      (t a))))

(defun jira-todo--merge-author-line-counts (entries)
  "Merge ENTRIES (AUTHOR-STRING . LINES) that represent the same person.

Same person means the same mapped PTAL name, the same display
name, or the same email local-part (plus-tags ignored, case
ignored)."
  (let (groups)
    (dolist (entry entries)
      (let* ((author (car entry))
              (lines (or (cdr entry) 0))
              (keys (jira-todo--author-identity-keys author))
              matches others)
        (dolist (g groups)
          (if (jira-todo--identity-keys-overlap-p keys (nth 0 g))
            (push g matches)
            (push g others)))
        (let ((best-author author)
               (best-lines lines)
               (total lines)
               (all-keys keys))
          (dolist (g matches)
            (setq all-keys (cl-delete-duplicates
                             (append all-keys (nth 0 g)) :test #'equal)
              total (+ total (nth 3 g))
              best-author (jira-todo--choose-author-string
                            best-author best-lines (nth 1 g) (nth 2 g))
              best-lines (if (string= best-author (nth 1 g))
                           (nth 2 g)
                           best-lines)))
          (setq groups (cons (list all-keys best-author best-lines total)
                         others)))))
    (mapcar (lambda (g) (cons (nth 1 g) (nth 3 g))) groups)))

(defun jira-todo--exclude-self-authors (entries &optional root)
  "Drop ENTRIES whose identity matches the git user in ROOT."
  (let ((self (jira-todo--self-identity-keys root)))
    (if (null self)
      entries
      (cl-remove-if
        (lambda (entry)
          (jira-todo--identity-keys-overlap-p
            (jira-todo--author-identity-keys (car entry)) self))
        entries))))

(defun jira-todo--ptal-reviewer-names ()
  "Return the merged PTAL reviewer names, or nil if none.

Top 5 git authors from the change-path prefixes of files changed
versus main come first, formatted via
`jira-todo--format-ptal-mention'.  Display names from
`jira-todo-pr-reviewers' are appended when they are not already
present.  Each name carries a leading `@' and has mapped git
author emails rewritten by `jira-todo-github-user-map'."
  (let* ((authors (condition-case err
                    (jira-todo--top-authors-for-changed-dirs 5)
                    (error
                      (message "jira-todo: could not list PTAL authors: %s"
                        (error-message-string err))
                      nil)))
          (auto (mapcar #'jira-todo--format-ptal-mention authors))
          (extra (jira-todo--split-pr-reviewer-names))
          (merged (jira-todo--merge-ptal-names auto extra)))
    (when merged
      (mapcar #'jira-todo--apply-email-map-to-text merged))))

(defun jira-todo--ptal-reviewer-line ()
  "Return the merged PTAL reviewer names as one space-separated line."
  (when-let* ((names (jira-todo--ptal-reviewer-names)))
    (mapconcat #'identity names " ")))

(defun jira-todo--changed-files-against-main (&optional root branch)
  "Return files changed on BRANCH versus the main-branch merge-base.

ROOT defaults to `jira-todo--git-directory'.  BRANCH defaults to
the heading Branch field.  When BRANCH is missing locally, use
origin/BRANCH (fetching if needed).  When no branch can be
resolved, fall back to HEAD (`MAIN...')."
  (let* ((default-directory (or root (jira-todo--git-directory)))
          (main (git-tools-main-branch-name default-directory))
          (rev (jira-todo--branch-rev
                 (or branch (jira-todo--heading-branch))
                 default-directory))
          (range (concat main "..." (or rev ""))))
    (unless main
      (user-error "Could not determine main branch for repo in %s"
        default-directory))
    (or (ignore-errors
          (magit-git-items "diff" "-z" "--name-only" range))
      (with-temp-buffer
        (unless (zerop (call-process "git" nil t nil
                         "diff" "-z" "--name-only" range))
          (user-error "Call git diff %s failed in %s" range default-directory))
        (split-string (buffer-string) "\0" t)))))

(defun jira-todo--top-authors-for-changed-dirs (&optional limit)
  "Return the top LIMIT git author strings for changed-file path prefixes.

LIMIT defaults to 5.  Files from `jira-todo--changed-files-against-main'
are grouped by `git-tools--change-path-prefixes': one component past
the longest prefix shared by every changed file, including a sibling
directory that contains only one changed file.  A file at that fork,
such as a workspace `Cargo.lock', is not a separate author path.

`git-tools--merged-change-author-weights' takes the top 5 authors of
each prefix, merges those lists by email and commit count.  Authors
mapped to an empty string in `jira-todo-github-user-map' are then
removed, aliases of the same person are merged, the current git user
is dropped, and the top LIMIT author strings are returned in that
order.  Suppressed authors never count toward LIMIT."
  (let* ((limit (or limit 5))
          (default-directory (jira-todo--git-directory))
          (files (jira-todo--changed-files-against-main default-directory))
          (entries (git-tools--author-display-entries
                     (git-tools--merged-change-author-weights
                       default-directory 'commits files))))
    (setq entries
      (jira-todo--remove-suppressed-authors entries))
    (setq entries
      (jira-todo--remove-suppressed-authors
        (jira-todo--exclude-self-authors
          (jira-todo--merge-author-line-counts entries)
          default-directory)))
    (setq entries
      (sort entries
        (lambda (a b)
          (if (= (cdr a) (cdr b))
            (string-lessp (car a) (car b))
            (> (cdr a) (cdr b))))))
    (mapcar #'car (cl-subseq entries 0 (min limit (length entries))))))

(defun jira-todo--ptal-replacement-line (line reviewers)
  "Return LINE rewritten with REVIEWERS, or nil to leave LINE unchanged."
  (when (string-match
          "\\`\\([ \t]*\\)\\(:pull_request: \\)?PTAL\\(?:[ \t]+\\(.*\\)\\)?[ \t]*\r?\\'"
          line)
    (let ((indent (or (match-string 1 line) ""))
           (prefix (or (match-string 2 line) ""))
           (rest (or (match-string 3 line) "")))
      (unless (string-match-p "\\`PR[ \t]*\\'" rest)
        (concat indent prefix "PTAL " reviewers)))))

(defun jira-todo--insert-ptal-in-message-block (text reviewers teams-reviewers)
  "Insert a PTAL line with REVIEWERS into the Teams/Slack block in TEXT.

TEAMS-REVIEWERS is the same list encoded as Teams `@{NAME}' mention
tokens and defaults to REVIEWERS.  A Teams `xteams' block gets it
after the here-document opener; a Slack block gets REVIEWERS after
`--begin--'.  Return (NEW-TEXT . COUNT)."
  (let ((count 0)
         (new text))
    (setq new
      (replace-regexp-in-string
        "\\(Teams:\n\\(?:#\\+begin_src[^\n]*\n\\)?[^\n]*<<'[A-Za-z0-9_-]+'\n\\)"
        (lambda (m)
          (setq count (1+ count))
          (concat m "PTAL " (or teams-reviewers reviewers) "\n"))
        new t t))
    (when (zerop count)
      (setq new
        (replace-regexp-in-string
          "\\(Slack:\n--begin--\n\\)"
          (lambda (m)
            (setq count (1+ count))
            (concat m "PTAL " reviewers "\n"))
          new t t)))
    (when (zerop count)
      (setq new
        (replace-regexp-in-string
          "\\(--begin--\n\\)"
          (lambda (m)
            (setq count (1+ count))
            (concat m "PTAL " reviewers "\n"))
          new t t)))
    (cons new count)))

(defun jira-todo--messaging-body-bounds (&optional text only-provider)
  "Return (PROVIDER START . END) offsets of the first message body in TEXT.

PROVIDER is `teams' or `slack'.  A `Teams:' section holds an
org-babel `xteams' block, so its body is the here-document contents
up to the delimiter line.  A `Slack:' section's body is the lines
between `--begin--' and `--end--'.  Marker lines are excluded.
Prompt and PR Text blocks are ignored.  When both bodies are
present the first one wins, unless ONLY-PROVIDER (`teams' or
`slack') restricts the search.  TEXT defaults to the current org
heading; nil when it holds no such body."
  (let ((text (or text (jira-todo--heading-text)))
         (provider nil)
         (delimiter nil)
         (in-body nil)
         (start nil)
         (offset 0)
         bounds)
    (dolist (line (split-string text "\n" nil))
      (let ((next (1+ (+ offset (length line)))))
        (cond
          (bounds nil)
          ((and (not in-body)
             (or (null only-provider) (eq only-provider 'teams))
             (string-match-p "\\`[ \t]*Teams:[ \t]*\\'" line))
            (setq provider 'teams))
          ((and (not in-body)
             (or (null only-provider) (eq only-provider 'slack))
             (string-match-p "\\`[ \t]*Slack:[ \t]*\\'" line))
            (setq provider 'slack))
          ((and (eq provider 'teams) (not in-body)
             (string-match "\\`[ \t]*.*<<'\\([A-Za-z0-9_-]+\\)'[ \t]*\\'" line))
            (setq delimiter (match-string 1 line)
              in-body t
              start next))
          ((and in-body (eq provider 'teams)
             (string= (string-trim line) delimiter))
            (setq bounds (cons 'teams (cons start (max start (1- offset))))
              in-body nil
              provider nil))
          ((and in-body (eq provider 'slack)
             (string-match-p "\\`[ \t]*--end--[ \t]*\\'" line))
            (setq bounds (cons 'slack (cons start (max start (1- offset))))
              in-body nil
              provider nil))
          ((and (eq provider 'slack) (not in-body)
             (string-match-p "\\`[ \t]*--begin--[ \t]*\\'" line))
            (setq in-body t
              start next)))
        (setq offset next)))
    bounds))

(defun jira-todo--messaging-section-body (&optional text)
  "Return the Teams/Slack message body from TEXT or the current heading.

A `Teams:' section holds an org-babel `xteams' block, so its body
is the here-document contents up to the delimiter line.  A
`Slack:' section's body is the lines between `--begin--' and
`--end--'.  Marker lines are not included.  Prompt and PR Text
blocks are ignored."
  (let* ((text (or text (jira-todo--heading-text)))
          (bounds (jira-todo--messaging-body-bounds text))
          (body (and bounds (substring text (cadr bounds) (cddr bounds)))))
    (unless (or (null body) (string-empty-p body))
      body)))

(defun jira-todo--copy-messaging-section (&optional text)
  "Copy the Teams/Slack message body and optional TEXT to the kill ring.
Return the copied text, or nil if no such section exists."
  (when-let* ((body (jira-todo--messaging-section-body text)))
    (kill-new body)
    body))

(defun jira-todo--linkify-teams-body ()
  "Turn the bare URLs of the Teams message body into CommonMark autolinks.

`xteams' builds the Teams HTML from CommonMark, which does not
autolink bare URLs, so Teams would otherwise store them as plain
text.  Slack bodies and already-wrapped URLs are left alone.
Return the number of URLs wrapped."
  (let* ((text (jira-todo--heading-text))
          (bounds (jira-todo--messaging-body-bounds text 'teams)))
    (if (not bounds)
      0
      (let* ((start (cadr bounds))
              (end (cddr bounds))
              (autolinked (jira-todo--autolink-urls (substring text start end)))
              (count (cdr autolinked)))
        (when (> count 0)
          (jira-todo--replace-heading-text
            (concat (substring text 0 start) (car autolinked) (substring text end))))
        count))))

(defun jira-todo--replace-ptal-reviewers (reviewers &optional teams-reviewers)
  "Replace PTAL reviewer lists in the current org heading with REVIEWERS.

TEAMS-REVIEWERS is REVIEWERS encoded as Teams `@{NAME}' mention
tokens and defaults to REVIEWERS; lines inside a Teams `xteams'
block use it.  Leaves a Teams \"PTAL PR\" line unchanged.  If no
PTAL line exists, insert one into the Teams block or under the
Slack --begin-- marker.  Works when the subtree is folded.  Return
the number of replacements."
  (let* ((text (jira-todo--heading-text))
          (teams-reviewers (or teams-reviewers reviewers))
          (in-teams nil)
          (count 0)
          (new
            (mapconcat
              (lambda (line)
                (cond
                  ((string-match-p "\\`[ \t]*Teams:[ \t]*\\'" line)
                    (setq in-teams t)
                    line)
                  ((string-match-p
                     "\\`[ \t]*\\(?:Slack\\|Prompt\\|PR Text\\):[ \t]*\\'" line)
                    (setq in-teams nil)
                    line)
                  ((string-match-p "\\`[ \t]*#\\+end_src[ \t]*\\'" line)
                    (setq in-teams nil)
                    line)
                  (t
                    (let ((rewritten
                            (jira-todo--ptal-replacement-line
                              line (if in-teams teams-reviewers reviewers))))
                      (if rewritten
                        (progn (setq count (1+ count)) rewritten)
                        line)))))
              (split-string text "\n" nil)
              "\n")))
    (when (zerop count)
      (let ((inserted (jira-todo--insert-ptal-in-message-block
                        text reviewers teams-reviewers)))
        (setq new (car inserted)
          count (cdr inserted))))
    (when (> count 0)
      (jira-todo--replace-heading-text new))
    count))

;;;###autoload
(defun jira-todo-fetch (&optional input)
  "Fetch a JIRA ticket and generate an `org-mode' TODO and Slack message.
INPUT can be a full URL, a key like JIRA-11111, or just an issue number.
If INPUT is not provided, prompt interactively."
  (interactive)
  (let* ((input (or (jira-todo--clipboard-jira-url) input
                  (and (not (called-interactively-p 'any)) nil)
                  (read-string "JIRA issue (URL, key, or number): ")))
          (key (jira-todo--parse-input input))
          (rest-url (jira-todo--key-to-rest-url key))
          ;; The response callback runs in whichever buffer is current
          ;; when it fires, so pin the buffer the TODO belongs to.
          (buf (current-buffer)))
    (request rest-url
      :headers `(("Accept"        . "application/json")
                  ("Authorization" . ,(jira-todo--auth-header)))
      :parser #'json-read
      :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (if (buffer-live-p buf)
                     (with-current-buffer buf
                       (jira-todo--insert-output data))
                     (message "JIRA %s fetched, but buffer %s no longer exists"
                       key buf))))
      :error (cl-function
               (lambda (&key error-thrown &allow-other-keys)
                 (message "Error fetching JIRA ticket: %S" error-thrown))))))

;;;###autoload
(defun jira-todo-update-with-pr (&optional url)
  "Update the current TODO with URL, clipboard, or then prompt for it.

Replace <PR-TBD> patterns in the current TODO when present, with
the pull request title summary on the line under the URL.  The
title is looked up via `gh' or the GitHub REST API and has any
leading ticket prefix (bracketed, or a ticket key with its `\": \"')
removed.  An already-filled PR URL in the heading is reused and is
not an error.

Also rewrite the Teams/Slack PTAL reviewer line with the top 5 git
authors from `jira-todo--top-authors-for-changed-dirs' (commit
count), merged across unique change-path prefixes of files from
`git diff --name-only MAIN...BRANCH'.  BRANCH is the heading
Branch field when present (local, else origin/BRANCH after fetch);
otherwise HEAD.  Git Directory is optional: the heading field if
present, else a local clone matching the PR URL repo and/or the
remote Branch name, else `jira-todo-git-directory'.  Names from
`jira-todo-pr-reviewers' that are not already on the line are
appended.  Names inside a Teams block are encoded as `@{Name}'
mention tokens.  Bare URLs in a Teams block body are wrapped as
CommonMark autolinks so Teams stores them clickable.  After the
heading is updated, copy the Teams/Slack message body to the kill
ring."
  (interactive)
  (let* ((search-invisible t)
          (url (jira-todo--resolve-pr-url url))
          (count (jira-todo--replace-pr-placeholders url))
          (links (jira-todo--linkify-teams-body))
          (reviewers (or (jira-todo--ptal-reviewer-names)
                       (user-error
                         "Could not build a PTAL reviewer list (no authors and no PULL_REQUEST_REVIEWERS)")))
          (ptal (mapconcat #'identity reviewers " "))
          (teams-ptal (jira-todo--teams-mention-names reviewers))
          (ptal-count (jira-todo--replace-ptal-reviewers ptal teams-ptal))
          (copied (jira-todo--copy-messaging-section)))
    (message "Updated %d PR placeholder(s), %d URL(s) autolinked, PTAL %s%s%s"
      count links ptal
      (if (zerop ptal-count) " (heading not rewritten)" "")
      (if copied " (copied Teams/Slack message)" ""))
    count))

;;;###autoload
(defun jira-todo-insert-peer-code-review-task ()
  "Start a peer code review and insert a TODO describing it.

The pull request URL is taken from the system clipboard and must be
a non-JIRA http(s) GitHub pull request URL; otherwise nothing is
inserted and this signals.  A local clone whose origin matches the
pull request's owner/repo must be discoverable (see
`jira-todo--find-repo-from-remote': `jira-todo-git-directory' and
the parallel review clones under `git-tools-review-home');
otherwise this signals.  Point must be in an `org-mode' buffer,
because the TODO is inserted as a heading.

The pull request is reviewed in a parallel clone under
`git-tools-review-home', created from that local clone's origin by
`git-tools-review-start', which resets and cleans the review clone
and checks out the pull request's head branch.  Only then is the
TODO inserted, above the TODO point was on, with the Branch the
review ended up on, that review directory, and the review prompt
`git-tools-review-start' leaves on the kill ring.  That ordering
matters: inserted before the review, the Branch would name the
pre-review branch and the Prompt would be the previous kill, not
the review prompt.

The text between the --begin-- and --end-- lines is then passed
through `caveman-region'.

`git-tools-review-start' reports a failed fetch or checkout only in
its process buffer, so the branch is re-read afterwards.  When the
review did not leave the pull request's head branch, nothing is
inserted and this signals."
  (interactive)
  (let ((url (or (jira-todo--clipboard-pr-url)
               (user-error "Clipboard does not hold a pull request URL"))))
    (unless (git-tools--github-pr-number url)
      (user-error "Not a GitHub pull request URL: %s" url))
    (unless (derived-mode-p 'org-mode)
      (user-error "Must be called from an org-mode TODO"))
    (let* ((owner-repo (jira-todo--pr-url-owner-repo url))
            (owner (car owner-repo))
            (repo (cdr owner-repo))
            (source (jira-todo--find-repo-from-remote owner repo nil))
            (buf (current-buffer))
            (position (copy-marker (point))))
      (unless source
        (user-error
          "No local clone matching %s/%s found under %s or %s"
          owner repo jira-todo-git-directory (git-tools-review-base-directory)))
      (unwind-protect
        (let* ((home (save-window-excursion (git-tools-review-start source)))
                (branch (git-tools-current-branch-name home)))
          (when (or (null branch)
                  (equal branch (git-tools-main-branch-name home)))
            (user-error
              "Review left %s on %s; no TODO inserted"
              home (or branch "a detached HEAD")))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (goto-char position)
              (jira-todo--insert-todo-entry
                (concat
                  (format "*** TODO %s: Review PR %s\n"
                    jira-todo-peer-code-review-prefix url)
                  (format "Branch: %s\n" branch)
                  (format "Git Directory: %s\n" home)
                  (format "Prompt:\n")
                  (format "--begin--\n")
                  (format "%s\n" (or (current-kill 0 t) ""))
                  (format "--end--")))
              ;; Transform only the text between --begin-- and --end--.
              (save-excursion
                (goto-char (point-min))
                (when (search-forward
                        (format "Review PR %s\nBranch: %s\n" url branch)
                        nil t)
                  (when (re-search-forward "^--begin--\n" nil t)
                    (let ((start (point)))
                      (when (re-search-forward "^--end--" nil t)
                        (let ((end (copy-marker (match-beginning 0))))
                          (caveman-copy-region start end)
                          (set-marker end nil))))))))))
        (set-marker position nil)))))

(provide 'jira-todo)
;;; jira-todo.el ends here
