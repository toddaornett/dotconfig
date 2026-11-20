---
name: elisp-error-messages
description: Capitalize the leading word of Emacs Lisp `(error ...)` messages and spell product names like OpenSSL, SQLite, and Emacs correctly.
activation_paths:
  - "~/.config/elisp"
  - "~/.config/elisp/**/*"
  - "~/.config/doom"
  - "~/.config/doom/**/*"
---

# Emacs Lisp Error Message Capitalization

Every `(error "MESSAGE" ...)` call you write in Emacs Lisp must satisfy the
rules below. They apply to `user-error` as well. They do not apply to
`message`, `warn`, docstrings, or log lines unless the same text is raised as
an error.

## Rule 1: Capitalize the first character

The leading string of `(error ...)` starts with a capital letter.

```elisp
;; Wrong
(error "file not found: %s" path)
(error "no `openssl' executable")

;; Right
(error "File not found: %s" path)
(error "No `openssl' executable")
```

Do not lower-case the rest of the sentence; only the first character is
forced to upper case.

## Rule 2: Use real product and protocol names

When the leading word is a product, protocol, or standard, use its official
casing, not the lowercase command name.

| Wrong | Right |
| --- | --- |
| openssl | OpenSSL |
| sqlite / sqlite3 (tool) | SQLite / `sqlite3` |
| emacs | Emacs |
| elisp | Emacs Lisp (or Elisp) |
| json | JSON |
| http / https | HTTP / HTTPS |
| jwt | JWT |
| aes | AES |
| curl | curl (command) / cURL is not used |
| github | GitHub |
| gitlab | GitLab |
| macos | macOS |
| linux | Linux |
| windows | Windows |
| mac / osx | macOS |
| url | URL |
| api | API |
| dpi | DPI |
| cursor | Cursor |

Prefer the proper name when it reads naturally:

```elisp
;; Wrong
(error "openssl failed to decrypt the Cursor token")
(error "sqlite database is locked")

;; Right
(error "OpenSSL failed to decrypt the Cursor token")
(error "SQLite database is locked")
```

If capitalizing the proper name would misname a literal string, quote the
literal and keep its exact casing:

```elisp
(error "No `cursorAuth/accessToken' in the Cursor database")
(error "Neither built-in SQLite nor the `sqlite3' CLI is available")
```

## Rule 3: Keep identifiers, paths, and placeholders untouched

Never capitalize code symbols, file names, environment variables, URLs,
format directives, or values inserted by `%s` / `%d` / `%S`.

```elisp
;; Right: `foo-bar' stays lower case, %s is a placeholder
(error "Function `foo-bar' returned %s" value)
(error "Config file %s is unreadable" path)
```

If the message starts with a placeholder or a code reference, capitalize the
first real word instead:

```elisp
(error "%s is not a JWT" token)          ; Wrong: token value is dynamic
(error "Token %s is not a JWT" token)    ; Right
```

A leading package prefix such as `helm-chart-tool:` is a literal identifier:
keep its exact case and capitalize the first word of the clause after it.

```elisp
;; Wrong
(error "helm-chart-tool: no YAML files found under %s" dir)

;; Right
(error "helm-chart-tool: No YAML files found under %s" dir)
```

## Rule 4: Keep existing punctuation conventions

- End the message without a trailing period; Emacs error strings are labels,
  not prose.
- Keep `%s` and friends exactly as `format` expects.
- Do not add quotes around plain words; use backtick-quote only for symbols,
  functions, files, and commands (`\\`sqlite3'`).

## Checklist before returning Elisp

1. Every `(error ...)` literal begins with an upper-case letter or a
   non-letter (placeholder, number, quoted symbol).
2. Product/protocol names use official casing (OpenSSL, SQLite, Emacs, JWT,
   JSON, HTTP, URL, API).
3. Lower-case identifiers, paths, and `%` directives are preserved.
4. No trailing period was added.
