---
name: pull-request
description: >-
  Commit, push, and open a pull request. Use when asked to commit code,
  push a branch, or raise/create a PR in the repo. Covers conventional
  commit format, pushing the branch (including --force-with-lease after a
  rewrite), and `gh pr create` with the repo's PR template, title convention and
  labels. Reviewers are populated by GitHub Actions, so this skill never requests
  reviewers and never announces the PR anywhere.
allowed-tools:
  - Bash
  - Read
  - Grep
---

# Commit → Push → PR

Three steps for a change: commit it, push the branch, open the pull
request. Nothing else belongs in this skill.

**Repo-local rules win.** Whatever the repository already dictates — commit
scopes, ticket footers, the base branch, the PR template, the label set, its
own `/commit` or `/pull-request` skills — is read from that repo, not restated
here. The defaults below apply only where the repo is silent.

## 1. Commit

- `<type>[(<scope>)]: <subject>` — type from
  `feat|fix|docs|style|refactor|perf|test|build|ci|chore|revert`, imperative
  mood, ≤ 50 characters, no trailing period.
- Scope only when the repo defines scopes; use its list, never an invented one.
- Body explains **why**, wrapped at 72 columns.
- Breaking change → `!` after type/scope **and** `🚨 BREAKING CHANGE:` in the
  body.
- Trailers (ticket footers, co-authors) follow the repo's convention. If the
  repo already requires one, that rule lives there — do not duplicate it here.

Stage only the files the change needs. If the repo's pre-commit hooks rewrite
anything, re-check the diff and the staged set before continuing.

## 2. Push

```bash
git push -u origin HEAD          # first push of the branch
git push --force-with-lease      # only after a rebase or an amend
```

- Never push to the repo's default branch; always a feature branch.
- Let the repo's pre-push hooks run. Do not bypass them to save time.

## 3. PR

Base branch: the repo's default branch — pass `--base` when the PR should
target something else.

Title: the repo's title convention (ticket prefix, key, …) followed by an
imperative description — capitalized, no conventional-commit type, roughly 50
characters when practical.

Body: fill the repository's pull request template when it has one
(`.github/pull_request_template.md`), keep every section it marks mandatory, and
describe the final state of the change rather than its development history. Add
a manual-test section only when manual verification is genuinely needed. Keep
the attribution line the repo asks for, e.g.:

```
This PR was opened using the `<agent>` AI agent
```

```bash
gh pr create --title "<prefix> <title>" --base <branch> \
  --body-file /tmp/pr-body.md --label <label> --label <label>
```

Labels: apply the labels the repo defines — typically one programming-language
label plus the type labels for the change. Skip labels the repo does not have.

Then verify the PR:

```bash
gh pr view <n> --json mergeable,mergeStateStatus   # CONFLICTING → rebase the branch
gh pr checks <n> --watch                           # report failures with reasons
```

## Not in this skill

- **No announcement.** There is no chat message to compose or post.
- **No reviewer requests.** GitHub Actions populate reviewers when the PR is
  created; never pass `--reviewer`, never edit a reviewer list, never resolve
  reviewer mentions.
- **No configuration, no scripts.** No `config.json`, no `scripts/` directory —
  the workflow is the three steps above.
