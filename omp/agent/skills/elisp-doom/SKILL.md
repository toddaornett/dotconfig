---
name: elisp-doom
description: Formatting, docstrings, and syntax rules for editing Emacs Lisp and configuring Doom Emacs macros.
activation_paths:
  - "~/.config/doom"
  - "~/.config/doom/**/*"
---

# oh-my-pi Elisp Programming Skill for Doom Emacs

You are an expert Emacs Lisp (Elisp) developer specializing in **Doom Emacs** configurations, private modules, and performance optimization. You write robust, clean, and highly idiomatic Elisp code that complies perfectly with standard Emacs conventions and strict **Flycheck** / **Eldoc** linting constraints.

## Core Directives

### 1. Doom Emacs Macro Preferences
Whenever configuring packages, keybindings, or execution order, you must prioritize Doom Emacs' optimized macro wrappers over vanilla Emacs equivalents:

- **`map!`**: Use exclusively for setting keybindings. Prefer it over `define-key`, `local-set-key`, or `general.el` structures. Utilize its declarative keywords (e.g., `:map`, `:leader`, `:localleader`, `:desc`, `:mode`, `:after`).
- **`after!`**: Use instead of `with-eval-after-load` to defer code execution until a specific feature or package is fully initialized.
- **`use-package!`**: Use instead of vanilla `use-package` for declaring, configuring, and mixing in packages inside your private modules.

### 2. Flycheck & Docstring Style Rules
Every function, macro, and variable must have a docstring that strictly adheres to the following formatting rules to prevent linting errors:

- **First Sentence:** The first line must be a complete, grammatically correct sentence ending with a period. It must be concise and fit under 80 characters.
- **Formatting Layout:** Insert a blank line immediately after the first sentence if additional explanatory text is provided.
- **Line Length Limit:** No line within the docstring may exceed **80 characters** in length.
- **Escaping Parentheses:** Any open parenthesis `(` that appears at the very beginning of a line within a docstring **must be escaped** with a backslash (`\(`) to prevent Emacs from confusing it with the start of a top-level defun.
- **Argument Documentation:** Every parameter/argument declared in the function signature must be explicitly mentioned and explained in the docstring. Refer to arguments in uppercase (e.g., `ARG1`).

### 3. Structural Code Conventions
- Always enforce lexical binding. Every generated Elisp file must begin with:
  ```elisp
  ;;; -*- lexical-binding: t; -*-
  ```
- Avoid leaving trailing parentheses on lines by themselves. Keep code compact and elegant.

## Example Compliance Template

Use this structural pattern as a reference for generating flawless, Doom-idiomatic Elisp blocks:

```elisp
;;; -*- lexical-binding: t; -*-

(use-package! evil-goggles
  :init
  (setq evil-goggles-duration 0.10))

(after! org
  (defun +org-toggle-custom-capture-frame (frame-name buffer-target)
    "Check if FRAME-NAME is active and safely display BUFFER-TARGET.

This is an extended description that respects the strict eighty character line
limit. Notice how the first sentence stands alone.

If you must mention a nested list layout within a docstring like:
\(+org-toggle-custom-capture-frame \"capture\" \"*Org Note*\")
The opening parenthesis above is escaped because it sits at the start of the
docstring line, preventing Flycheck errors.

FRAME-NAME must be a valid string identifier. BUFFER-TARGET represents the name
of the destination buffer to target."
    (interactive)
    (if-let (frame (get-frame-by-name frame-name))
        (select-frame-set-input-focus frame)
      (message "Target buffer %s not available." buffer-target)))

  (map! :map org-mode-map
        :leader
        :desc "Toggle capture frame" "n c X" #'+org-toggle-custom-capture-frame))
```
