;;; caveman.el --- Simplify English into terse, parseable text -*- lexical-binding: t; -*-

;; Author: Todd Ornett
;; Copyright (C) 2026 Todd Ornett
;; Created: September 28, 2026
;; Modified: September 28, 2026
;; Version: 0.0.1
;; Keywords: convenience, text
;; Package-Requires: ((emacs "26.1"))
;; Homepage: https://github.com/toddaornett/dotconfig
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;; Translates English text into terse, simplified English suitable for
;; LLM input.  Drops articles, copulas, auxiliaries, politeness and
;; intensifiers, rewrites wordy phrases, simplifies pronouns and common
;; irregular verbs, keeps negation, preserves case, and leaves paths,
;; identifiers, flags and numbers untouched.

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
  '("of" "in" "on" "at" "that" "which" "so")
  "Lowercase words dropped only when `caveman-aggressive' is non-nil."
  :type '(repeat string)
  :group 'caveman)

(defcustom caveman-phrase-map
  '(("\\bto be \\([[:alpha:]]+\\(?:ed\\|en\\)\\)\\b" . "\\1")
     ("\\bstart with\\b" . "first")
     ("\\bif possible\\b" . "if can")
     ("\\bcomments for improvement\\b" . "improvement comments")
     ("\\bin order to\\b" . "to")
     ("\\bprior to\\b" . "before")
     ("\\bdue to\\b" . "because")
     ("\\bbecause of\\b" . "because")
     ("\\bas well as\\b" . "and")
     ("\\bmake sure\\( that\\)?\\b" . "ensure")
     ("\\bwould like to\\b" . "want to")
     ("\\b\\(?:be\\|am\\|is\\|are\\|was\\|were\\) able to\\b" . "can")
     ("\\bfor example\\b" . "example")
     ("\\bfor each\\b" . "each"))
  "Alist of (REGEXP . REPLACEMENT) applied before word processing.
Matching is case-insensitive and the replacement adopts the case of the
matched text.  REPLACEMENT may use \\\\1 style back-references."
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
  '(;; Negation and special contractions
     ("can't" . "cannot") ("cannot" . "cannot") ("won't" . "not")
     ("let's" . "let us") ("ain't" . "not")
     ;; Shorter synonyms
     ("provide" . "give") ("provides" . "give") ("include" . "add")
     ("includes" . "add") ("concise" . "short") ("utilize" . "use")
     ("utilise" . "use") ("approximately" . "about")
     ;; Irregular verbs -> base form
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
  "Alist mapping lowercase words to replacements."
  :type '(alist :key-type string :value-type string)
  :group 'caveman)

(defcustom caveman-filler-words
  '("the" "a" "an"
     "is" "are" "am" "was" "were" "been" "be" "being"
     "do" "does" "did" "will" "would" "shall"
     "please" "kindly" "also"
     "very" "really" "just" "quite" "extremely" "completely"
     "actually" "basically" "simply" "certainly" "definitely")
  "Lowercase words to delete.
Prepositions and modals such as \"should\", \"must\" and \"can\" are
kept on purpose because removing them changes the meaning."
  :type '(repeat string)
  :group 'caveman)

(defconst caveman--chunk-regexp
  "\\([[:alnum:]_.~/\\\\@#'’-]+\\)\\|\\([^[:alnum:]_.~/\\\\@#'’-]+\\)"
  "Group 1 is a word-like chunk, group 2 is a run of separators.")

(defconst caveman--sentence-end
  "[.!?][\"')]*\\(?:[ \t\n]\\|\\'\\)\\|\n[ \t]*\n"
  "Matches separator text that ends a sentence.")

(defun caveman--protected-p (core)
  "Non-nil if CORE is a path, identifier, flag or number to leave alone."
  (string-match-p "[_/\\\\@#0-9]\\|\\`-\\|[[:alnum:]][-.][[:alnum:]]" core))

(defun caveman--lookup-key (key)
  "Return the replacement for lowercase KEY, \"\" to drop it, or nil."
  (cond ((cdr (assoc key caveman-pronoun-map)))
    ((cdr (assoc key caveman-word-map)))
    ((member key caveman-filler-words) "")
    ((and caveman-aggressive (member key caveman-aggressive-words)) "")))

(defun caveman--lookup (word)
  "Return the replacement for WORD, \"\" to drop it, or nil to keep it."
  (let ((key (downcase (replace-regexp-in-string "’" "'" word))))
    (or (caveman--lookup-key key)
      (cond
        ((string-match-p "\\`.+n't\\'" key) "not")
        ((string-match "\\`\\(.+\\)'\\(?:re\\|ve\\|ll\\|d\\|m\\)\\'" key)
          (let ((base (match-string 1 key)))
            (or (caveman--lookup-key base) base)))
        ((string-match
           "\\`\\(he\\|she\\|it\\|that\\|there\\|here\\|what\\|who\\|where\\|how\\)'s\\'"
           key)
          (let ((base (match-string 1 key)))
            (or (caveman--lookup-key base) base)))))))

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

(defun caveman--apply-phrases (text)
  "Apply `caveman-phrase-map' rewrites to TEXT."
  (let ((case-fold-search t))
    (dolist (rule caveman-phrase-map text)
      (setq text (replace-regexp-in-string (car rule) (cdr rule) text)))))

(defun caveman-string (text)
  "Return TEXT rewritten as terse, simplified English."
  (let ((pos 0) (out nil) (start t) (recap nil) (carry ""))
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
            (progn (funcall emit (concat carry sep))
              (setq carry ""))
            (string-match "\\`\\(['’]*\\)\\(.*?\\)\\([.'’-]*\\)\\'" chunk)
            (let ((lead (match-string 1 chunk))
                   (core (match-string 2 chunk))
                   (trail (match-string 3 chunk)))
              (funcall emit lead)
              (unless (string-empty-p core)
                (if (caveman--protected-p core)
                  (progn (push core out) (setq start nil recap nil))
                  (let ((rep (caveman--lookup core)))
                    (if (equal rep "")
                      (when start (setq recap t))
                      (let ((new (if rep
                                   (caveman--match-case core rep start)
                                   core)))
                        (when recap (setq new (caveman--capitalize new)))
                        (push new out)
                        (setq start nil recap nil))))))
              (setq carry trail)))))
      (push carry out))
    (let ((result (apply #'concat (nreverse out))))
      (dolist (rule '(("[ \t]+" . " ")
                       (" \\([,.;:!?]\\)" . "\\1")
                       ("^ +" . "")
                       (" +$" . "")))
        (setq result (replace-regexp-in-string (car rule) (cdr rule) result)))
      result)))

;;;###autoload
(defun caveman-region (start end)
  "Convert the region from START to END into simplified text."
  (interactive "r")
  (let ((new-text (caveman-string (buffer-substring-no-properties start end))))
    (goto-char start)
    (delete-region start end)
    (insert new-text)))

;;;###autoload
(defun caveman-buffer ()
  "Convert the entire current buffer into simplified text."
  (interactive)
  (caveman-region (point-min) (point-max)))

(provide 'caveman)
;;; caveman.el ends here
