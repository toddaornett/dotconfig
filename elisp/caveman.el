;;; caveman.el --- Simplify English into terse, parseable text -*- lexical-binding: t; -*-

;; Author: Todd Ornett
;; Copyright (C) 2026 Todd Ornett
;; Created: September 28, 2026
;; Modified: September 29, 2026
;; Version: 0.0.1
;; Keywords: convenience, text
;; Package-Requires: ((emacs "26.1"))
;; Homepage: https://github.com/toddaornett/dotconfig
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;; Translates English text into terse, simplified English suitable for
;; LLM input.  Drops articles, copulas, auxiliaries, expletives,
;; politeness and intensifiers, rewrites wordy phrases and light-verb
;; padding, reduces inflected words to their base form, simplifies
;; pronouns and common irregular verbs, collapses repeated words, keeps
;; negation, preserves case, and leaves paths, identifiers, flags,
;; numbers, all-caps words, inline code spans and fenced code blocks
;; untouched.

;;; Code:

(require 'subr-x)

(defgroup caveman nil
  "Translate English text into terse, simplified English."
  :group 'editing)

(defcustom caveman-aggressive nil
  "When non-nil, also drop the words in `caveman-aggressive-words'."
  :type 'boolean
  :group 'caveman)

(defcustom caveman-aggressive-words
  '("of" "in" "on" "at" "that" "which" "so"
    "quickly" "carefully" "easily" "immediately" "typically" "usually"
    "generally" "finally" "probably" "perhaps" "maybe")
  "Lowercase words dropped only when `caveman-aggressive' is non-nil."
  :type '(repeat string)
  :group 'caveman)

(defcustom caveman-phrase-map
  '(;; Wordy connectives, longest form first.
    ("\\bdue to the fact that\\b" . "because")
    ("\\bdue to\\b" . "because")
    ("\\bbecause of\\b" . "because")
    ("\\bin order to\\b" . "to")
    ("\\bprior to\\b" . "before")
    ("\\bsubsequent to\\b" . "after")
    ("\\bin the event that\\b" . "if")
    ("\\bin case of\\b" . "if")
    ("\\bin case\\b" . "if")
    ("\\bwith regard to\\b" . "about")
    ("\\bwith respect to\\b" . "about")
    ("\\bin relation to\\b" . "about")
    ("\\bas well as\\b" . "and")
    ("\\bin addition to\\b" . "and")
    ("\\bin addition\\b" . "and")
    ("\\bfor example\\b" . "example")
    ("\\bfor instance\\b" . "example")
    ("\\bfor each\\b" . "each")
    ("\\bstart with\\b" . "first")
    ("\\bif possible\\b" . "if can")
    ("\\bcomments for improvement\\b" . "improvement comments")
    ("\\ba number of\\b" . "some")
    ("\\ba lot of\\b" . "many")
    ("\\bthe majority of\\b" . "most")
    ("\\bat this point in time\\b" . "now")
    ("\\bat the moment\\b" . "now")
    ("\\bin the near future\\b" . "soon")
    ;; Ability and obligation.
    ("\\b\\(?:be\\|am\\|is\\|are\\|was\\|were\\) able to\\b" . "can")
    ("\\b\\(?:be\\|am\\|is\\|are\\|was\\|were\\) unable to\\b" . "cannot")
    ("\\bhas the ability to\\b" . "can")
    ("\\b\\(?:is\\|are\\|am\\) required to\\b" . "must")
    ("\\bit is necessary to\\b" . "must")
    ("\\bwould like to\\b" . "want to")
    ("\\bmake sure to\\b" . "must")
    ("\\bmake sure\\( that\\)?\\b" . "ensure")
    ;; Light-verb padding.
    ("\\btake into account\\b" . "consider")
    ("\\btake into consideration\\b" . "consider")
    ("\\bmake a decision\\b" . "decide")
    ("\\bmake a choice\\b" . "choose")
    ("\\bprovide assistance\\b" . "help")
    ("\\bprovide a description\\b" . "describe")
    ("\\bperform a review\\b" . "review")
    ("\\bconduct a review\\b" . "review")
    ("\\bgive an explanation\\b" . "explain")
    ;; Passive voice.
    ("\\bto be \\([[:alpha:]]+\\(?:ed\\|en\\)\\)\\b" . "\\1")
    ;; Expletives, hedges and empty openers: drop the whole phrase.
    ("\\bit is important to note that\\b[ \t]*" . "")
    ("\\bplease note that\\b[ \t]*" . "")
    ("\\bnote that\\b[ \t]*" . "")
    ("\\bas you know\\b[ \t]*" . "")
    ("\\bsort of\\b[ \t]*" . "")
    ("\\bkind of\\b[ \t]*" . "")
    ("\\bit\\(?:['’]s\\| is\\| would be\\| will be\\| was\\)\\b[ \t]*" . "")
    ("\\bthere\\(?:['’]s\\| is\\| are\\| was\\| were\\)\\b[ \t]*" . ""))
  "Alist of (REGEXP . REPLACEMENT) applied before word processing.
Matching is case-insensitive and each replacement adopts the case of the
matched text.  REPLACEMENT may use \\\\1 style back-references, and an
empty REPLACEMENT drops the match."
  :type '(alist :key-type regexp :value-type string)
  :group 'caveman)

(defcustom caveman-pronoun-map
  '(("i" . "me") ("my" . "me") ("mine" . "me") ("myself" . "me")
    ("your" . "you") ("yours" . "you") ("yourself" . "you")
    ("he" . "him") ("his" . "him") ("himself" . "him")
    ("she" . "her") ("hers" . "her") ("herself" . "her")
    ("they" . "them") ("their" . "them") ("theirs" . "them")
    ("themselves" . "them")
    ("we" . "us") ("our" . "us") ("ours" . "us") ("ourselves" . "us"))
  "Alist mapping lowercase pronouns to simplified pronouns."
  :type '(alist :key-type string :value-type string)
  :group 'caveman)

(defcustom caveman-word-map
  '(;; Negation and special contractions.
    ("can't" . "cannot") ("cannot" . "cannot") ("won't" . "not")
    ("let's" . "let us") ("ain't" . "not")
    ;; Shorter synonyms.
    ("provide" . "give") ("include" . "add") ("concise" . "short")
    ("utilize" . "use") ("utilise" . "use") ("leverage" . "use")
    ("approximately" . "about") ("regarding" . "about")
    ("concerning" . "about")
    ("assist" . "help") ("assistance" . "help")
    ("obtain" . "get") ("receive" . "get") ("purchase" . "buy")
    ("require" . "need") ("attempt" . "try") ("demonstrate" . "show")
    ("indicate" . "show") ("inform" . "tell") ("commence" . "start")
    ("initiate" . "start") ("terminate" . "stop") ("execute" . "run")
    ("construct" . "build") ("modify" . "change") ("alter" . "change")
    ("modification" . "change") ("occur" . "happen") ("believe" . "think")
    ("perform" . "do") ("allow" . "let")
    ("additional" . "more") ("numerous" . "many") ("sufficient" . "enough")
    ("entire" . "whole") ("initial" . "first") ("final" . "last")
    ("subsequent" . "next") ("previous" . "last")
    ("currently" . "now") ("frequently" . "often") ("primarily" . "mostly")
    ("information" . "info") ("documentation" . "docs")
    ("configuration" . "config") ("possibility" . "chance")
    ("however" . "but") ("nevertheless" . "but") ("therefore" . "so")
    ("thus" . "so") ("furthermore" . "and") ("moreover" . "and")
    ;; Irregular verbs -> base form.
    ("has" . "have") ("had" . "have") ("having" . "have")
    ("goes" . "go") ("went" . "go") ("gone" . "go") ("going" . "go")
    ("saw" . "see") ("seen" . "see")
    ("made" . "make") ("took" . "take") ("taken" . "take")
    ("got" . "get") ("gotten" . "get")
    ("gave" . "give") ("given" . "give")
    ("came" . "come") ("knew" . "know") ("known" . "know")
    ("thought" . "think") ("found" . "find") ("said" . "say")
    ("told" . "tell") ("ran" . "run")
    ("wrote" . "write") ("written" . "write")
    ("bought" . "buy") ("built" . "build") ("began" . "begin")
    ("begun" . "begin") ("chose" . "choose") ("chosen" . "choose"))
  "Alist mapping lowercase words to replacements.
An empty replacement deletes the word.  Inflected forms are reduced to
their base form with `caveman-inflection-rules', so listing a base form
also covers its -s, -ed and -ing forms."
  :type '(alist :key-type string :value-type string)
  :group 'caveman)

(defcustom caveman-inflection-rules
  '(("ies" . "y") ("ied" . "y")
    ("es" . "") ("s" . "")
    ("pping" . "p") ("nning" . "n") ("tting" . "t")
    ("gging" . "g") ("mming" . "m")
    ("ing" . "e") ("ing" . "")
    ("pped" . "p") ("nned" . "n") ("tted" . "t")
    ("gged" . "g") ("mmed" . "m")
    ("ed" . "e") ("ed" . ""))
  "Alist of (SUFFIX . REPLACEMENT) used to guess the base form of a word.
Rules are tried in order against a word that is not itself in
`caveman-word-map'; a rule matches when the word ends in SUFFIX, and the
candidate base form is the word with SUFFIX replaced by REPLACEMENT.  A
candidate is used only when it is at least three characters long and is
either a `caveman-word-map' key or listed in `caveman-base-forms'."
  :type '(alist :key-type string :value-type string)
  :group 'caveman)

(defcustom caveman-base-forms
  '("apply" "ask" "begin" "build" "buy" "change" "check" "choose" "come"
    "create" "delete" "do" "fail" "find" "fix" "get" "give" "go" "have"
    "help" "know" "let" "make" "need" "read" "review" "run" "say" "see"
    "send" "show" "start" "stop" "take" "tell" "test" "think" "try"
    "update" "use" "want" "work" "write")
  "Lowercase base forms that inflected words may be reduced to.
An inflected word whose candidate base form appears here is replaced by
that base form, which collapses \"makes\" to \"make\"."
  :type '(repeat string)
  :group 'caveman)

(defcustom caveman-filler-words
  '("the" "a" "an"
    "is" "are" "am" "was" "were" "been" "be" "being"
    "do" "does" "did" "will" "would" "shall"
    "please" "kindly" "also"
    "very" "really" "just" "quite" "extremely" "completely"
    "actually" "basically" "simply" "certainly" "definitely"
    "somewhat" "rather" "fairly" "totally" "absolutely" "literally"
    "essentially" "obviously" "indeed" "honestly" "anyway" "anyways")
  "Lowercase words to delete.
Prepositions and modals such as \"should\", \"must\" and \"can\" are
kept on purpose because removing them changes the meaning."
  :type '(repeat string)
  :group 'caveman)

(defconst caveman--word-body "[:alnum:]_.~/\\\\@#'’–—-"
  "Class body of the characters that make up a word-like chunk.")

(defconst caveman--trail-chars "[.'’–—-]"
  "Character class matching the punctuation kept beside a word.")

(defconst caveman--chunk-regexp
  (concat "\\([" caveman--word-body "]+\\)"
          "\\|\\([^" caveman--word-body "]+\\)")
  "Group 1 is a word-like chunk, group 2 is a run of separators.")

(defconst caveman--word-regexp
  (concat "\\`\\(['’]*\\)\\(.*?\\)\\(" caveman--trail-chars "*\\)\\'")
  "Group 1 is a leading quote, group 2 the word, group 3 its punctuation.")

(defconst caveman--verbatim-regexp
  "```\\(?:.\\|\n\\)*?```\\|`[^`\n]+`"
  "Matches fenced code blocks and inline code spans.")

(defconst caveman--sentence-end
  "[.!?][\"')]*\\(?:[ \t\n]\\|\\'\\)\\|\n[ \t]*\n"
  "Matches separator text that ends a sentence.")

(defconst caveman--drop-marker "\0"
  "Character left behind where a phrase rule dropped its match.
The word loop turns it into a recap flag so that the first kept word
after a dropped phrase is still capitalized.")

(defconst caveman--drop-marker-regexp "\0+"
  "Regexp matching one or more drop markers.")

(defconst caveman--cleanup-rules
  '(("\0+" . "")
    ("[ \t]+" . " ")
    (" \\([,.;:!?]\\)" . "\\1")
    ("\\b\\([[:alpha:]]+\\) \\1\\b" . "\\1"))
  "Alist of (REGEXP . REPLACEMENT) rules applied to assembled prose.
The first drops leftover drop markers, the second collapses runs of
spaces, the third removes spaces before punctuation, and the fourth
collapses a word repeated twice in a row.")

(defun caveman--all-caps-p (text)
  "Non-nil if TEXT is all caps, like an acronym or a constant.

TEXT qualifies when it holds no lowercase letters and at least two
uppercase ones, so a single capitalized word such as \"I\" stays
eligible for rewriting.  Spaces and punctuation are ignored."
  (let ((case-fold-search nil))
    (and (null (string-match-p "[[:lower:]]" text))
         (string-match-p "[[:upper:]]" text)
         (string-match-p "[[:upper:]]" text
                         (1+ (string-match-p "[[:upper:]]" text))))))

(defun caveman--protected-p (core)
  "Non-nil if CORE is a path, identifier, flag or number to leave alone.

Words holding a dash or an underscore, and all-caps words such as
acronyms, are left alone too."
  (or (string-match-p "[-_/\\\\@#0-9–—]" core)
      (string-match-p "[[:alnum:]][.][[:alnum:]]" core)
      (caveman--all-caps-p core)))

(defun caveman--lookup-key (key)
  "Return the replacement for lowercase KEY, \"\" to drop it, or nil."
  (cond ((cdr (assoc key caveman-pronoun-map)))
        ((cdr (assoc key caveman-word-map)))
        ((member key caveman-filler-words) "")
        ((and caveman-aggressive (member key caveman-aggressive-words)) "")))

(defun caveman--inflected-stem (key)
  "Return the base form of lowercase KEY, or nil if there is none.

The base form is derived with `caveman-inflection-rules' and is returned
only when it is at least three characters long and is either a
`caveman-word-map' key or a member of `caveman-base-forms'."
  (catch 'caveman--stem
    (dolist (rule caveman-inflection-rules)
      (let* ((suffix (car rule))
             (len (length suffix))
             (head (- (length key) len)))
        (when (and (> head 0)
                   (string= suffix (substring key head)))
          (let ((stem (concat (substring key 0 head) (cdr rule))))
            (when (and (>= (length stem) 3)
                       (or (assoc stem caveman-word-map)
                           (member stem caveman-base-forms)))
              (throw 'caveman--stem stem))))))))

(defun caveman--lookup (word)
  "Return the replacement for WORD, \"\" to drop it, or nil to keep it."
  (let ((key (downcase (replace-regexp-in-string "’" "'" word))))
    (or (caveman--lookup-key key)
        (when-let* ((stem (caveman--inflected-stem key)))
          (or (cdr (assoc stem caveman-word-map)) stem))
        (cond
         ((string-match-p "\\`.+n't\\'" key) "not")
         ((string-match "\\`\\(.+\\)'\\(?:s\\|re\\|ve\\|ll\\|d\\|m\\)\\'" key)
          (let ((base (match-string 1 key)))
            (if (>= (length base) 2)
                (or (caveman--lookup-key base) base)
              word)))))))

(defun caveman--capitalize (string)
  "Upcase only the first character of STRING."
  (if (string-empty-p string)
      string
    (concat (upcase (substring string 0 1)) (substring string 1))))

(defun caveman--match-case (word replacement sentence-start)
  "Give REPLACEMENT the case style of WORD.
SENTENCE-START non-nil forces an initial capital."
  (cond
   (sentence-start (caveman--capitalize replacement))
   ((and (> (length word) 1) (string= word (upcase word)))
    (upcase replacement))
   ((and (/= (aref word 0) (downcase (aref word 0)))
         (not (string= word "I")))
    (caveman--capitalize replacement))
   (t replacement)))

(defun caveman--expand-phrase (replacement text)
  "Expand the back-references in REPLACEMENT against the match in TEXT.

Every \\\\N in REPLACEMENT becomes the text of the Nth group of the
match, or the empty string when that group did not participate."
  (let ((groups (let ((match (match-data))
                      (acc nil))
                  (while match
                    (push (if (car match)
                              (substring text (car match) (cadr match))
                            "")
                          acc)
                    (setq match (cddr match)))
                  (nreverse acc))))
    (replace-regexp-in-string
     "\\\\\\([0-9]\\)"
     (lambda (ref) (or (nth (string-to-number (substring ref 1)) groups) ""))
     replacement t t)))

(defun caveman--phrase-case (matched replacement)
  "Give REPLACEMENT the case style of MATCHED."
  (let ((case-fold-search nil))
    (cond ((string-empty-p replacement) replacement)
          ((and (string-match-p "[[:alpha:]]" matched)
                (string= matched (upcase matched)))
           (upcase replacement))
          ((string-match-p "\\`[[:upper:]]" matched)
           (caveman--capitalize replacement))
          (t replacement))))

(defun caveman--replace-phrase (regexp replacement text)
  "Replace REGEXP in TEXT with REPLACEMENT, adopting the case of each match.

REPLACEMENT may refer to match groups with \\\\N."
  (let ((case-fold-search t)
        (pos 0)
        (out nil))
    (while (string-match regexp text pos)
      (let* ((beg (match-beginning 0))
             (end (match-end 0))
             (matched (match-string 0 text))
             ;; Expanded here, while the match data is still current.
             (expanded (caveman--expand-phrase replacement text))
             (new (if (caveman--all-caps-p matched)
                      matched
                    (caveman--phrase-case matched expanded))))
        (if (= beg end)
            (setq pos (min (1+ end) (length text)))
          (push (substring text pos beg) out)
          (push (if (string-empty-p new) caveman--drop-marker new) out)
          (setq pos end))))
    (if (null out)
        text
      (push (substring text pos) out)
      (apply #'concat (nreverse out)))))

(defun caveman--apply-phrases (text)
  "Apply `caveman-phrase-map' rewrites to TEXT."
  (dolist (rule caveman-phrase-map text)
    (setq text (caveman--replace-phrase (car rule) (cdr rule) text))))

(defun caveman--segments (text)
  "Split TEXT into (VERBATIM . STRING) segments.

VERBATIM is non-nil for fenced code blocks and inline code spans, which
are never rewritten."
  (let ((pos 0)
        (segments nil))
    (while (string-match caveman--verbatim-regexp text pos)
      (when (> (match-beginning 0) pos)
        (push (cons nil (substring text pos (match-beginning 0))) segments))
      (push (cons t (match-string 0 text)) segments)
      (setq pos (match-end 0)))
    (when (< pos (length text))
      (push (cons nil (substring text pos)) segments))
    (nreverse segments)))

(defun caveman--cleanup (text)
  "Normalize spacing and repeated words in TEXT."
  (let ((case-fold-search t))
    (dolist (rule caveman--cleanup-rules text)
      (setq text (replace-regexp-in-string (car rule) (cdr rule) text)))))

(defun caveman--tersify (text state)
  "Rewrite prose TEXT into terse English, tracking sentence starts in STATE.

STATE is a cons cell whose car holds the sentence-start flag and whose
cdr holds the recap flag; this function updates both of them."
  (let ((start (car state))
        (recap (cdr state))
        (pos 0)
        (carry "")
        (out nil))
    (setq text (caveman--apply-phrases text))
    (let ((emit (lambda (s)
                  (unless (string-empty-p s)
                    (push s out)
                    (when (string-match-p caveman--sentence-end s)
                      (setq start t))))))
      (while (string-match caveman--chunk-regexp text pos)
        (setq pos (match-end 0))
        (let ((chunk (match-string 1 text))
              (sep (match-string 2 text)))
          (if sep
              (let ((piece (concat carry sep)))
                (when (string-match-p caveman--drop-marker-regexp piece)
                  ;; A drop counts as sentence-initial when the text in front
                  ;; of it already ended a sentence, not just when the
                  ;; sentence-start flag is set.
                  (let ((head (car (split-string
                                    piece caveman--drop-marker-regexp))))
                    (when (or start
                              (string-match-p caveman--sentence-end head))
                      (setq recap t)))
                  (setq piece (replace-regexp-in-string
                               caveman--drop-marker-regexp "" piece)))
                (funcall emit piece)
                (setq carry ""))
            (string-match caveman--word-regexp chunk)
            (let ((lead (match-string 1 chunk))
                  (core (match-string 2 chunk))
                  (trail (match-string 3 chunk)))
              (funcall emit lead)
              (unless (string-empty-p core)
                (if (caveman--protected-p core)
                    (progn
                      (push core out)
                      (setq start nil
                            recap nil))
                  (let ((rep (caveman--lookup core)))
                    (if (equal rep "")
                        (when start (setq recap t))
                      (let ((new (if rep
                                     (caveman--match-case core rep start)
                                   core)))
                        (when recap (setq new (caveman--capitalize new)))
                        (push new out)
                        (setq start nil
                              recap nil))))))
              (setq carry trail)))))
      (push carry out))
    (setcar state start)
    (setcdr state recap)
    (caveman--cleanup (apply #'concat (nreverse out)))))

(defun caveman-string (text &optional aggressive)
  "Return TEXT rewritten as terse, simplified English.

AGGRESSIVE non-nil also drops the words in `caveman-aggressive-words',
whatever the value of that variable is.  Inline code spans and fenced
code blocks in TEXT are copied through unchanged."
  (let ((caveman-aggressive (or aggressive caveman-aggressive))
        (state (cons t nil))
        (out nil))
    (dolist (segment (caveman--segments text)
                     (string-trim (apply #'concat (nreverse out))
                                  "[ \t]+" "[ \t]+"))
      (push (if (car segment)
                (progn
                  ;; A code span is the sentence's first token itself, so it
                  ;; consumes any pending recap.
                  (setcdr state nil)
                  (cdr segment))
              (caveman--tersify (cdr segment) state))
            out))))

;;;###autoload
(defun caveman-region (start end &optional aggressive)
  "Convert the region from START to END into simplified text.

An AGGRESSIVE argument also drops the words in
`caveman-aggressive-words'; interactively a prefix argument requests
that."
  (interactive "r\nP")
  (let ((new-text (caveman-string (buffer-substring-no-properties start end)
                                  aggressive)))
    (goto-char start)
    (delete-region start end)
    (insert new-text)))

;;;###autoload
(defun caveman-buffer (&optional aggressive)
  "Convert the entire current buffer into simplified text.

An AGGRESSIVE argument also drops the words in
`caveman-aggressive-words'."
  (interactive "P")
  (caveman-region (point-min) (point-max) aggressive))

;;;###autoload
(defun caveman-copy-region (start end &optional aggressive)
  "Copy the region from START to END, simplified, to the kill ring.

The buffer itself is left alone.  An AGGRESSIVE argument also drops the
words in `caveman-aggressive-words'."
  (interactive "r\nP")
  (let ((new-text (caveman-string (buffer-substring-no-properties start end)
                                  aggressive)))
    (kill-new new-text)
    (message "Caveman copied %d chars" (length new-text))))

;;;###autoload
(defun caveman-copy-buffer (&optional aggressive)
  "Copy the whole buffer, simplified, to the kill ring.

The buffer itself is left alone.  An AGGRESSIVE argument also drops the
words in `caveman-aggressive-words'."
  (interactive "P")
  (caveman-copy-region (point-min) (point-max) aggressive))

;;;###autoload
(defun caveman-toggle-aggressive ()
  "Toggle `caveman-aggressive'."
  (interactive)
  (setq caveman-aggressive (not caveman-aggressive))
  (message "Caveman aggressive %s" (if caveman-aggressive "on" "off")))

(provide 'caveman)
;;; caveman.el ends here
