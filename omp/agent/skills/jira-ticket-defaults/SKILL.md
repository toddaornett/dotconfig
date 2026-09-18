---
name: jira-ticket-defaults
description: >-
  Apply the Phoenix ENG ticket defaults that are required on every newly created
  Jira issue — Story Points set to the agent's estimate, Sprint defaulted to the
  current active sprint, and RnD Lead set to the team's configured lead. Use
  whenever creating, filing, or drafting a Jira ticket / ENG issue, after
  /draft-jira-tickets or atlassian jira-create.ts, and when backfilling missing
  Story Points, Sprint, or RnD Lead on an existing ENG ticket.
allowed-tools:
  - Bash
  - Read
---

# Jira Ticket Defaults

Supplement to `/jira-issues` and `/draft-jira-tickets`: those cover **content** and
**lifecycle**. This skill covers the **fields** that must be populated at creation
time and are easy to forget.

Three defaults are mandatory on every ticket an agent creates or files:

1. **Story Points** — the agent's own estimate. Never left empty, never `TBD`, never `0`.
2. **Sprint** — the current active sprint (by default). Overridable, never silently skipped.
3. **RnD Lead** — the team's lead (`leadName` / `leadAccountId` in `config.json`),
   unless a team or the user names someone else.

`jira-create.ts` from the `atlassian` skill cannot set custom fields, so creation is
two steps: **create, then finalize**. Run the finalize step in the same turn as the
create — a ticket left without points/sprint/lead is an unfinished task.

## Configuration — `config.json` (machine-local, gitignored)

Every instance-specific value lives in `config.json` next to this file, never in
the script. It is gitignored (`~/.config/.gitignore`), so field ids and the lead
account never land in the dotfiles repo.

| Key | Meaning |
|---|---|
| `boardId` | Scrum board whose active sprint is "the current sprint" for new tickets |
| `boardName` | Label for that board (output only) |
| `fields.storyPoints` / `fields.sprint` / `fields.rndLead` | Custom field ids for this Jira instance |
| `expectedNames.storyPoints` / `.sprint` / `.rndLead` | Names those ids must resolve to; the script aborts on drift |
| `leadName` | Default RnD Lead display name (output only) |
| `leadAccountId` | accountId written to the RnD Lead field |
| `pointScale` | Estimation scale for the board; `--points` outside it warns (but still applies) |

If the file is missing, the script writes a **neutral template** — every value
empty/zero — and refuses to continue. Fill it in, then re-run:

```bash
bunx tsx ~/.agents/skills/atlassian/scripts/jira-fields.ts "Story Points"   # → fields.storyPoints
bunx tsx ~/.agents/skills/atlassian/scripts/jira-fields.ts "Sprint"        # → fields.sprint
bunx tsx ~/.agents/skills/atlassian/scripts/jira-fields.ts "RnD Lead"      # → fields.rndLead
# boardId: the scrum board the team plans on; leadAccountId:
# /rest/api/3/user/search?query=<name> on the same instance
```

Ids are per-instance: never copy a `customfield_*` id from another Jira site, and
never paste one into an ad-hoc `curl` without resolving its name first. The script
resolves every configured id back to a live field name via `/rest/api/3/field` and
**refuses to run** if a name does not match `expectedNames`, so a silent mis-write
is impossible.

## Workflow

```bash
# 1. Create (atlassian skill — content/lifecycle rules live in /draft-jira-tickets)
bunx tsx ~/.agents/skills/atlassian/scripts/jira-create.ts \
  '{"project": "ENG", "type": "Task", "summary": "…", "description": "…"}'

# 2. Finalize (this skill) — <KEY> and the agent's estimate
bun run ~/.config/omp/agent/skills/jira-ticket-defaults/scripts/finalize-ticket.ts \
  ENG-16527 --points 2
```

Step 2 is required for **every** issue type, including Bug: the ENG Bug create
screen has no Sprint / Story Points / RnD Lead fields, so those are only settable
after creation (they are editable post-create; the script checks `editmeta` first).

Backfill uses the same command — an existing ticket created without points or
sprint is fixed the same way.

### Options

| Flag | Default | Meaning |
|---|---|---|
| `--points <n>` | **required** | Story points estimate; refuses to run without it |
| `--sprint <v>` | `current` | `current` = active sprint of `--board`; `<sprintId>` = explicit; `none` = leave alone |
| `--lead <v>` | `default` | `default` = `leadName` from config; `<accountId>` = someone else; `none` = leave alone |
| `--board <id>` | `boardId` from config | Board whose active sprint counts as "the current sprint" |
| `--json` | off | Machine-readable result for scripting |

The script writes once, then **reads the issue back and compares**. It exits
non-zero if a field it attempted did not land. Fields absent from that issue's
edit screen (for example Story Points on a Sub-task) are reported as skipped
rather than failed; Story Points, being mandatory, are a hard failure.

Report the resulting id → name mapping table in your answer (e.g.
`customfield_XXXXX → "Story Points"`) so the user can verify nothing drifted.

## Estimation rubric

Estimate from the described work, on the board's scale (`pointScale` in
`config.json`). Values outside that scale are still written, but the script warns,
so keep the two in sync:

| Points | Scope |
|---|---|
| `0.5` | Docs, comments, config, one-line fix — no behavior change |
| `1` | One service, one concern: a bug fix with its regression test, a small endpoint tweak |
| `2` | Several files in one service, or one new handler/endpoint/component with tests |
| `3` | Cross-service change, DB migration plus code, new skill or service wiring |
| `5` | Multi-service feature (proto + DB + portal), or enough unknowns to need investigation |
| `8` / `13` | Needs a spike, or should be split before pickup |

Rules:

- Estimate in the same turn as filing; state the number and a one-line rationale.
- Between two values, take the **higher** and say so.
- If the honest estimate is `8` or more, say that the ticket should be split rather
  than silently filing an oversized ticket.
- If the user gives explicit points, use theirs — do not re-estimate over them.
- Values off the scale are accepted with a warning, never invented freely.

## Current sprint

Default source: the single `state=active` sprint of the `boardId` board in
`config.json` — queried live, never cached, because it changes every two weeks.

- No active sprint, or more than one → the script stops and tells you; resolve with
  `--sprint <id>` rather than guessing.
- Ticket belongs to a different board (Frontend, DevOps, Agent, …) → pass
  `--board <id>` so the sprint comes from that board.
- Deliberately unscheduled work → `--sprint none`, and say so in the ticket comment.

## Failures to avoid

- Filing a ticket and "adding points later" — they never get added; the board's
  capacity math depends on them.
- Passing a sprint id guessed from an old ticket: sprint ids roll over, and a stale
  id puts the work in a closed sprint.
- Setting RnD Lead to a reactivated or renamed account without resolving the
  accountId; the script validates the configured account against the instance on
  every default run.
- Hardcoding `customfield_*` ids into ad-hoc API calls after reading them here.
