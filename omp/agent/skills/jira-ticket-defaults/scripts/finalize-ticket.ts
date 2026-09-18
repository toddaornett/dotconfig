#!/usr/bin/env bun
/**
 * jira-ticket-defaults — apply Phoenix ENG ticket defaults to a newly created issue.
 *
 * Sets, then verifies by read-back:
 *   - Story Points   — the agent's estimate, mandatory
 *   - Assignee       — the authenticated caller by default (resolved live, never configured)
 *   - Sprint         — active sprint of the configured board by default
 *   - RnD Lead       — the configured lead by default
 *
 * Usage:
 *   bun run scripts/finalize-ticket.ts <ISSUE-KEY> --points <n> [options]
 *
 * Options:
 *   --points <n>      Story points estimate (required, > 0)
 *   --assignee <v>    me (default) | <accountId> | none
 *   --sprint <v>      current (default) | <sprintId> | none
 *   --lead <v>        config.json lead (default) | <accountId> | none
 *   --board <id>      Board used to resolve the current sprint (default: config.json)
 *   --json            Emit machine-readable output only
 *   --help            Show this help
 *
 * Instance-specific values (field ids, board, lead) live in `config.json` next to
 * SKILL.md. That file is gitignored and machine-local; a neutral, non-working
 * template is written on first run and the script refuses to continue until it is
 * filled in. Credentials come from ~/.local/secrets/atlassian.env
 * (ATLASSIAN_SITE / ATLASSIAN_EMAIL / ATLASSIAN_API_TOKEN) or the environment.
 */

import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";
import { fileURLToPath } from "node:url";

const CONFIG_PATH = fileURLToPath(new URL("../config.json", import.meta.url));
const ENV_PATH = `${process.env.HOME ?? ""}/.local/secrets/atlassian.env`;

const FIELDS_HELP = `  bunx tsx ~/.agents/skills/atlassian/scripts/jira-fields.ts "Story Points"
  bunx tsx ~/.agents/skills/atlassian/scripts/jira-fields.ts "Sprint"
  bunx tsx ~/.agents/skills/atlassian/scripts/jira-fields.ts "RnD Lead"`;

/**
 * Written when config.json is absent. Every value is deliberately empty/zero so an
 * unfilled config fails loudly instead of writing wrong fields to a real ticket.
 */
const CONFIG_TEMPLATE = `{
  "_help": {
    "fields": "Custom field ids for this Jira instance. Resolve each name with the commands under 'expectedNames' below; ids differ per instance.",
    "expectedNames": "Names those ids must resolve to on this instance. Resolve with: bunx tsx ~/.agents/skills/atlassian/scripts/jira-fields.ts \\"Story Points\\" (and \\"Sprint\\", \\"RnD Lead\\"). The script aborts if a name does not match.",
    "boardId": "Scrum board whose active sprint new tickets should land in.",
    "boardName": "Label for that board. Output only.",
    "leadName": "Default RnD Lead display name. Output only.",
    "leadAccountId": "accountId of that lead, from /rest/api/3/user/search?query=<name>.",
    "pointScale": "Estimation scale for this board. A --points value outside it is accepted with a warning; an empty list fails the config check."
  },
  "boardId": 0,
  "boardName": "",
  "fields": {
    "storyPoints": "",
    "sprint": "",
    "rndLead": ""
  },
  "expectedNames": {
    "storyPoints": "",
    "sprint": "",
    "rndLead": ""
  },
  "leadName": "",
  "leadAccountId": "",
  "pointScale": []
}
`;

const USAGE = `Usage: bun run scripts/finalize-ticket.ts <ISSUE-KEY> --points <n> [options]

Options:
  --points <n>      Story points estimate (required, > 0; scale from config.json)
  --assignee <v>    me (default) | <accountId> | none
  --sprint <v>      current (default) | <sprintId> | none
  --lead <v>        config.json lead (default) | <accountId> | none
  --board <id>      Board for the "current sprint" lookup (default: config.json boardId)
  --json            Machine-readable output

Config: ${CONFIG_PATH}`;

function fail(message: string): never {
  process.stderr.write(`error: ${message}\n`);
  process.exit(1);
}

// ---------------------------------------------------------------------------
// Config — every instance-specific value comes from config.json
// ---------------------------------------------------------------------------

interface SkillConfig {
  boardId: number;
  boardName: string;
  fields: {
    storyPoints: string;
    sprint: string;
    rndLead: string;
  };
  expectedNames: {
    storyPoints: string;
    sprint: string;
    rndLead: string;
  };
  leadName: string;
  leadAccountId: string;
  /** Board estimation scale; a value outside it earns a warning, not a failure. */
  pointScale: number[];
}

/** Narrow a JSONC node to an object without inventing an unchecked member shape. */
function asRecord(value: unknown): Record<string, unknown> {
  return value !== null && typeof value === "object" && !Array.isArray(value)
    ? (value as Record<string, unknown>)
    : {};
}

function readString(source: Record<string, unknown>, key: string): string {
  const value = source[key];
  return typeof value === "string" ? value.trim() : "";
}

/** Strict: a single bad entry makes the whole list empty, so the config check reports it. */
function readNumberArray(source: Record<string, unknown>, key: string): number[] {
  const value = source[key];
  if (!Array.isArray(value)) return [];
  const numbers = value.filter(
    (entry): entry is number => typeof entry === "number" && Number.isFinite(entry) && entry > 0,
  );
  return numbers.length === value.length ? numbers : [];
}

function loadConfig(): SkillConfig {
  if (!existsSync(CONFIG_PATH)) {
    mkdirSync(dirname(CONFIG_PATH), { recursive: true });
    writeFileSync(CONFIG_PATH, CONFIG_TEMPLATE);
    fail(
      `wrote a neutral config template to ${CONFIG_PATH}.\n` +
        "Fill it in, then re-run. Field ids, board and lead accountId are instance specific:\n" +
        `${FIELDS_HELP}\n` +
        "  boardId: scrum board whose active sprint new tickets belong to\n" +
        "  leadAccountId: /rest/api/3/user/search?query=<name>",
    );
  }

  let parsed: unknown;
  try {
    parsed = JSON.parse(readFileSync(CONFIG_PATH, "utf8"));
  } catch (error) {
    fail(
      `${CONFIG_PATH} is not valid JSON: ${error instanceof Error ? error.message : String(error)}. ` +
        "Delete it to regenerate the neutral template.",
    );
  }

  const raw = asRecord(parsed);
  const rawFields = asRecord(raw.fields);
  const rawNames = asRecord(raw.expectedNames);
  const rawBoardId = raw.boardId;

  const config: SkillConfig = {
    boardId:
      typeof rawBoardId === "number" && Number.isInteger(rawBoardId) && rawBoardId > 0
        ? rawBoardId
        : 0,
    boardName: readString(raw, "boardName"),
    fields: {
      storyPoints: readString(rawFields, "storyPoints"),
      sprint: readString(rawFields, "sprint"),
      rndLead: readString(rawFields, "rndLead"),
    },
    expectedNames: {
      storyPoints: readString(rawNames, "storyPoints"),
      sprint: readString(rawNames, "sprint"),
      rndLead: readString(rawNames, "rndLead"),
    },
    leadName: readString(raw, "leadName"),
    leadAccountId: readString(raw, "leadAccountId"),
    pointScale: readNumberArray(raw, "pointScale"),
  };

  const incomplete = [
    config.boardId > 0 ? null : "boardId",
    config.fields.storyPoints.length > 0 ? null : "fields.storyPoints",
    config.fields.sprint.length > 0 ? null : "fields.sprint",
    config.fields.rndLead.length > 0 ? null : "fields.rndLead",
    config.expectedNames.storyPoints.length > 0 ? null : "expectedNames.storyPoints",
    config.expectedNames.sprint.length > 0 ? null : "expectedNames.sprint",
    config.expectedNames.rndLead.length > 0 ? null : "expectedNames.rndLead",
    config.leadName.length > 0 ? null : "leadName",
    config.leadAccountId.length > 0 ? null : "leadAccountId",
    config.pointScale.length > 0 ? null : "pointScale",
  ].filter((key): key is string => key !== null);

  if (incomplete.length > 0) {
    fail(
      `${CONFIG_PATH} is not filled in yet — empty: ${incomplete.join(", ")}.\n` +
        "Field ids are instance specific; resolve them with:\n" +
        FIELDS_HELP +
        "\n  leadAccountId: /rest/api/3/user/search?query=<name>",
    );
  }

  return config;
}

// ---------------------------------------------------------------------------
// Credentials
// ---------------------------------------------------------------------------

interface Creds {
  site: string;
  email: string;
  token: string;
}

function loadCreds(): Creds {
  const fromFile: Record<string, string> = {};
  if (existsSync(ENV_PATH)) {
    for (const line of readFileSync(ENV_PATH, "utf8").split("\n")) {
      const trimmed = line.trim();
      if (trimmed.length === 0 || trimmed.startsWith("#")) continue;
      const eq = trimmed.indexOf("=");
      if (eq === -1) continue;
      fromFile[trimmed.slice(0, eq).trim()] = trimmed
        .slice(eq + 1)
        .trim()
        .replace(/^["']|["']$/g, "");
    }
  }

  const site = fromFile.ATLASSIAN_SITE ?? process.env.ATLASSIAN_SITE;
  const email = fromFile.ATLASSIAN_EMAIL ?? process.env.ATLASSIAN_EMAIL;
  const token = fromFile.ATLASSIAN_API_TOKEN ?? process.env.ATLASSIAN_API_TOKEN;

  if (!site || !email || !token) {
    fail(
      `Missing Atlassian credentials. Expected ${ENV_PATH} to define ATLASSIAN_SITE, ` +
        "ATLASSIAN_EMAIL and ATLASSIAN_API_TOKEN (run the /setup-skills skill).",
    );
  }

  return { site, email, token };
}

// ---------------------------------------------------------------------------
// HTTP
// ---------------------------------------------------------------------------

let auth: { site: string; header: string } | null = null;

/** Resolved on first use so --help works without credentials configured. */
function authContext(): { site: string; header: string } {
  if (auth === null) {
    const creds = loadCreds();
    auth = {
      site: creds.site,
      header: `Basic ${Buffer.from(`${creds.email}:${creds.token}`).toString("base64")}`,
    };
  }
  return auth;
}

async function api<T>(path: string, init: RequestInit = {}): Promise<T> {
  const { site, header } = authContext();
  const response = await fetch(`https://${site}${path}`, {
    ...init,
    headers: {
      Authorization: header,
      Accept: "application/json",
      ...(init.body ? { "Content-Type": "application/json" } : {}),
      ...init.headers,
    },
  });

  if (!response.ok) {
    throw new Error(
      `Jira ${init.method ?? "GET"} ${path} failed (HTTP ${response.status}): ${(
        await response.text()
      ).slice(0, 600)}`,
    );
  }

  if (response.status === 204) return undefined as T;

  const text = await response.text();
  return (text ? JSON.parse(text) : undefined) as T;
}

// ---------------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------------

interface Args {
  issueKey: string;
  points: number;
  /** "me" = the authenticated caller; otherwise an accountId. */
  assignee: string;
  sprint: string;
  lead: string;
  /** null = use config.json boardId */
  boardId: number | null;
  json: boolean;
}

function parseArgs(argv: string[]): Args {
  const positional: string[] = [];
  const flags: Record<string, string> = {};

  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg === undefined) continue;
    if (!arg.startsWith("--")) {
      positional.push(arg);
      continue;
    }
    const body = arg.slice(2);
    const eq = body.indexOf("=");
    const name = eq === -1 ? body : body.slice(0, eq);
    if (name === "json" || name === "help") {
      flags[name] = "true";
      continue;
    }
    const inline = eq === -1 ? undefined : body.slice(eq + 1);
    const value = inline ?? argv[++i];
    if (value === undefined) fail(`--${name} requires a value`);
    flags[name] = value;
  }

  if (positional.length === 0 || flags.help !== undefined) {
    process.stdout.write(`${USAGE}\n`);
    process.exit(flags.help !== undefined ? 0 : 1);
  }

  const issueKey = positional[0];
  if (issueKey === undefined) fail("Missing issue key");

  const pointsRaw = flags.points;
  if (pointsRaw === undefined) {
    fail("--points is required: story points are never left empty on a new ticket");
  }
  const points = Number(pointsRaw);
  if (!Number.isFinite(points) || points <= 0) {
    fail(`--points must be a positive number (got "${pointsRaw}")`);
  }

  const boardRaw = flags.board;
  let boardId: number | null = null;
  if (boardRaw !== undefined) {
    boardId = Number(boardRaw);
    if (!Number.isInteger(boardId) || boardId <= 0) {
      fail(`--board must be a numeric board id (got "${boardRaw}")`);
    }
  }

  const sprint = flags.sprint ?? "current";
  if (
    sprint !== "current" &&
    sprint !== "none" &&
    !(Number.isInteger(Number(sprint)) && Number(sprint) > 0)
  ) {
    fail(`--sprint must be "current", "none", or a numeric sprint id (got "${sprint}")`);
  }

  const assignee = flags.assignee ?? "me";
  if (assignee.length === 0) {
    fail(`--assignee must be "me", "none", or an accountId (got "${assignee}")`);
  }

  return {
    issueKey,
    points,
    assignee,
    sprint,
    lead: flags.lead ?? "default",
    boardId,
    json: flags.json !== undefined,
  };
}

// ---------------------------------------------------------------------------
// Jira shapes
// ---------------------------------------------------------------------------

interface JiraField {
  id: string;
  name: string;
}

interface Sprint {
  id: number;
  name: string;
  state: string;
}

interface Actor {
  accountId: string;
  displayName?: string;
  active?: boolean;
}

interface Issue {
  key: string;
  fields: {
    summary?: string;
    issuetype?: { name: string; subtask?: boolean };
    [fieldId: string]: unknown;
  };
}

interface ResolvedField {
  id: string;
  name: string;
  current: string;
  next: string | null;
  applied: boolean;
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Human-readable rendering of an arbitrary Jira field value. */
function describe(value: unknown): string {
  if (value === null || value === undefined) return "—";
  if (Array.isArray(value)) {
    return value.length === 0
      ? "—"
      : value
          .map((entry) =>
            entry !== null && typeof entry === "object" && "name" in entry
              ? String(entry.name)
              : String(entry),
          )
          .join(", ");
  }
  if (typeof value === "object") {
    if ("displayName" in value) return String(value.displayName);
    if ("name" in value) return String(value.name);
    if ("value" in value) return String(value.value);
    return JSON.stringify(value);
  }
  return String(value);
}

/** accountId of an actor-shaped field value (assignee, RnD Lead), or null when unset. */
function accountIdOf(value: unknown): string | null {
  return value !== null && typeof value === "object" && "accountId" in value
    ? String(value.accountId)
    : null;
}

/** Resolve a configured field id to its live name, refusing a stale or mismatched mapping. */
function resolveField(fieldNames: Map<string, string>, id: string, expectedName: string): string {
  const actual = fieldNames.get(id);
  const { site } = authContext();
  if (actual === undefined) {
    fail(
      `Field ${id} does not exist on ${site}. ` +
        `Re-resolve field ids (jira-fields.ts) and update ${CONFIG_PATH} before retrying.`,
    );
  }
  if (actual !== expectedName) {
    fail(
      `Field ${id} is "${actual}" on ${site}, expected "${expectedName}" (${CONFIG_PATH}). ` +
        "Field ids are instance-specific — re-resolve them and update that file.",
    );
  }
  return actual;
}

async function resolveSprint(
  sprintArg: string,
  boardId: number,
  issueKey: string,
): Promise<Sprint | null> {
  if (sprintArg === "none") return null;

  if (sprintArg !== "current") {
    const id = Number(sprintArg);
    if (!Number.isInteger(id) || id <= 0) {
      fail(`--sprint must be "current", "none", or a numeric sprint id (got "${sprintArg}")`);
    }
    return { id, name: `sprint ${id}`, state: "explicit" };
  }

  const board = await api<{ values?: Sprint[] }>(
    `/rest/agile/1.0/board/${boardId}/sprint?state=active`,
  );
  const active = board.values ?? [];

  if (active.length === 0) {
    fail(
      `No active sprint on board ${boardId}. Start a sprint, or pass an explicit ` +
        `--sprint <id> / --sprint none for ${issueKey}.`,
    );
  }
  if (active.length > 1) {
    fail(
      `Board ${boardId} reports ${active.length} active sprints (` +
        `${active.map((s) => `${s.name}=${s.id}`).join(", ")}). Pass --sprint <id> explicitly.`,
    );
  }
  const sprint = active[0];
  if (sprint === undefined) fail(`Board ${boardId} returned an empty active sprint`);
  return sprint;
}

/** Resolve the RnD Lead account, re-validating the configured id against the instance. */
async function resolveLead(leadArg: string, config: SkillConfig): Promise<Actor | null> {
  if (leadArg === "none") return null;
  if (leadArg !== "default") return { accountId: leadArg };

  let lead: Actor;
  try {
    lead = await api<Actor>(
      `/rest/api/3/user?accountId=${encodeURIComponent(config.leadAccountId)}`,
    );
  } catch (error) {
    fail(
      `Configured RnD Lead ${config.leadName} (${config.leadAccountId}) could not be resolved: ` +
        `${error instanceof Error ? error.message : String(error)}`,
    );
  }
  if (lead.active === false) {
    fail(`Configured RnD Lead ${config.leadName} (${config.leadAccountId}) is deactivated.`);
  }
  return lead;
}

/**
 * Resolve the assignee. "me" asks Jira who the authenticated caller is, so the account
 * is never stored in config and follows the token when it is rotated.
 */
async function resolveAssignee(assigneeArg: string): Promise<Actor | null> {
  if (assigneeArg === "none") return null;
  if (assigneeArg !== "me") return { accountId: assigneeArg };

  const me = await api<Actor>("/rest/api/3/myself");
  if (!me.accountId) {
    fail("Jira /myself returned no accountId; pass --assignee <accountId> explicitly.");
  }
  if (me.active === false) {
    fail("The authenticated Jira user is deactivated; pass --assignee <accountId> explicitly.");
  }
  return me;
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

async function main(): Promise<void> {
  const args = parseArgs(process.argv.slice(2));
  const config = loadConfig();
  const boardId = args.boardId ?? config.boardId;

  const allFields = await api<JiraField[]>("/rest/api/3/field");
  const fieldNames = new Map(allFields.map((f) => [f.id, f.name]));

  const storyPointsName = resolveField(
    fieldNames,
    config.fields.storyPoints,
    config.expectedNames.storyPoints,
  );
  const sprintName = resolveField(
    fieldNames,
    config.fields.sprint,
    config.expectedNames.sprint,
  );
  const rndLeadName = resolveField(
    fieldNames,
    config.fields.rndLead,
    config.expectedNames.rndLead,
  );

  const fieldList = [config.fields.storyPoints, config.fields.sprint, config.fields.rndLead].join(
    ",",
  );
  const issuePath = `/rest/api/3/issue/${encodeURIComponent(args.issueKey)}`;

  const issue = await api<Issue>(`${issuePath}?fields=${fieldList},summary,issuetype,assignee`);
  const editMeta = await api<{ fields?: Record<string, unknown> }>(`${issuePath}/editmeta`);

  const sprint = await resolveSprint(args.sprint, boardId, args.issueKey);
  const lead = await resolveLead(args.lead, config);
  const assignee = await resolveAssignee(args.assignee);

  if (!config.pointScale.includes(args.points)) {
    process.stderr.write(
      `warning: ${args.points} is outside the configured scale (${config.pointScale.join("/")})\n`,
    );
  }

  const update: Record<string, unknown> = { [config.fields.storyPoints]: args.points };
  if (sprint) update[config.fields.sprint] = sprint.id;
  if (lead) update[config.fields.rndLead] = { accountId: lead.accountId };
  if (assignee) update.assignee = { accountId: assignee.accountId };

  const editable = new Set(Object.keys(editMeta.fields ?? {}));
  const skippedIds = new Set<string>();
  for (const id of Object.keys(update)) {
    if (editable.has(id)) continue;
    // The assignee is requested explicitly (by default: the caller), so it never disappears
    // silently — an unassignable account is a configuration problem the caller must see.
    if (id === "assignee") {
      fail(
        `Assignee is not editable on ${args.issueKey}: the account behind the API token lacks ` +
          '"Assign Issues" in this project, or is not assignable there. Re-run with ' +
          "--assignee none to skip assignment.",
      );
    }
    skippedIds.add(id);
    delete update[id];
  }

  if (!(config.fields.storyPoints in update)) {
    fail(
      `Story points (${config.fields.storyPoints} → "${storyPointsName}") are not editable on ` +
        `${args.issueKey} (${issue.fields.issuetype?.name ?? "unknown type"}). ` +
        "Estimate the parent issue or the linked Story instead.",
    );
  }

  await api(issuePath, { method: "PUT", body: JSON.stringify({ fields: update }) });

  const after = await api<Issue>(`${issuePath}?fields=${fieldList},assignee`);

  const sprintAfter = describe(after.fields[config.fields.sprint]);
  const leadAfterId = accountIdOf(after.fields[config.fields.rndLead]);
  const assigneeAfterId = accountIdOf(after.fields.assignee);

  const resolved: ResolvedField[] = [
    {
      id: config.fields.storyPoints,
      name: storyPointsName,
      current: describe(issue.fields[config.fields.storyPoints]),
      next: String(args.points),
      applied: Number(after.fields[config.fields.storyPoints]) === args.points,
    },
    {
      id: "assignee",
      name: "Assignee",
      current: describe(issue.fields.assignee),
      next: assignee === null ? null : (assignee.displayName ?? assignee.accountId),
      applied: assignee === null || assigneeAfterId === assignee.accountId,
    },
    {
      id: config.fields.sprint,
      name: sprintName,
      current: describe(issue.fields[config.fields.sprint]),
      next: sprint === null ? null : `${sprint.name} [${sprint.id}]`,
      applied:
        sprint === null ||
        sprintAfter.includes(sprint.name) ||
        sprintAfter.includes(String(sprint.id)),
    },
    {
      id: config.fields.rndLead,
      name: rndLeadName,
      current: describe(issue.fields[config.fields.rndLead]),
      next: lead === null ? null : (lead.displayName ?? config.leadName),
      applied: lead === null || leadAfterId === lead.accountId,
    },
  ];

  for (const field of resolved) {
    if (skippedIds.has(field.id)) {
      field.next = null;
      field.applied = true;
    }
  }

  const failed = resolved.filter((field) => field.next !== null && !field.applied);
  const skipped = [...skippedIds].map((id) => `${id} ("${fieldNames.get(id) ?? "unknown"}")`);

  if (args.json) {
    process.stdout.write(
      `${JSON.stringify(
        {
          key: args.issueKey,
          type: issue.fields.issuetype?.name,
          summary: issue.fields.summary,
          fields: resolved.map((field) => ({
            id: field.id,
            name: field.name,
            from: field.current,
            to: field.next,
            applied: field.applied,
          })),
          skipped,
          ok: failed.length === 0,
        },
        null,
        2,
      )}\n`,
    );
  } else {
    const boardLabel = config.boardName.length > 0 ? `${boardId} (${config.boardName})` : `${boardId}`;
    const lines = [
      `Jira ticket defaults — ${args.issueKey} (${issue.fields.issuetype?.name ?? "?"})`,
      `  ${issue.fields.summary ?? ""}`,
      "",
      ...resolved.map(
        (field) =>
          `  ${field.name.padEnd(13)} ${field.current.padEnd(26)} -> ` +
          `${field.next ?? "(no change)"}${field.next !== null && !field.applied ? "   NOT APPLIED" : ""}`,
      ),
      "",
      `  field mapping: ${resolved.map((field) => `${field.id} → "${field.name}"`).join(", ")}`,
    ];
    if (skipped.length > 0) {
      lines.push(`  skipped (not on the edit screen for this issue type): ${skipped.join(", ")}`);
    }
    if (sprint !== null) {
      lines.push(`  sprint source: board ${boardLabel} active sprint (state=${sprint.state})`);
    }
    lines.push(
      "",
      failed.length === 0 ? "Verified by read-back: ok" : "Verified by read-back: FAILED",
    );
    process.stdout.write(`${lines.join("\n")}\n`);
  }

  if (failed.length > 0) process.exit(1);
}

await main();
