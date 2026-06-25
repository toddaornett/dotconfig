#!/usr/bin/env python3
"""
teamscount.py - Read Microsoft Teams unread and mention counts from local IndexedDB.

Outputs a single JSON object to stdout:
  {"unreads": <int>, "mentions": <int>}

Strategy:
  - Iterate all conversations in the conversation-manager store
  - Keep only the highest-version record per thread ID
  - unreads: only threads where interest != "NotInterested"
  - mentions: all threads regardless of interest, so a direct mention
    in a muted/ignored channel is still surfaced

Exit codes:
  0 - success
  1 - Teams IndexedDB not found or unreadable
"""

import json
import os
import sys

INDEXEDDB_BASE = os.path.expanduser(
    "~/Library/Containers/com.microsoft.teams2/Data/Library/Application Support"
    "/Microsoft/MSTeams/EBWebView/WV2Profile_tfw/IndexedDB"
    "/https_teams.microsoft.com_0.indexeddb.leveldb"
)

BLOB_BASE = os.path.expanduser(
    "~/Library/Containers/com.microsoft.teams2/Data/Library/Application Support"
    "/Microsoft/MSTeams/EBWebView/WV2Profile_tfw/IndexedDB"
    "/https_teams.microsoft.com_0.indexeddb.blob"
)

CONVERSATION_DB_PREFIX = "Teams:conversation-manager"
CONVERSATION_STORE     = "conversations"


def parse_consumption_horizon(horizon: str) -> float:
    """Return the read-up-to timestamp from 'ts1;ts2;msgid'."""
    if not horizon:
        return 0.0
    try:
        return float(horizon.split(";")[0])
    except (ValueError, IndexError):
        return 0.0


def get_user_mri(records: dict) -> str:
    """Extract current user's MRI from the collected records."""
    for v in records.values():
        try:
            users = v.get("chatTitle", {}).get("avatarUsersInfo", [])
            if users and isinstance(users, list):
                mri = users[0].get("mri", "")
                if mri:
                    return mri
        except Exception:
            pass
    return ""


def check_mention(v: dict, horizon_ts: float, user_mri: str) -> bool:
    """Return True if the last unread message in v directly mentions user_mri."""
    if not user_mri:
        return False
    try:
        last_msg = v.get("lastMessage", {})
        if not isinstance(last_msg, dict):
            return False
        msg_time = float(last_msg.get("id", 0) or 0)
        if msg_time <= horizon_ts:
            return False
        msg_props    = last_msg.get("properties", {}) or {}
        raw_mentions = msg_props.get("mentions", [])
        if isinstance(raw_mentions, str):
            try:
                raw_mentions = json.loads(raw_mentions)
            except Exception:
                raw_mentions = []
        if isinstance(raw_mentions, list):
            for m in raw_mentions:
                if isinstance(m, dict) and m.get("mri") == user_mri:
                    return True
    except Exception:
        pass
    return False


def fetch_counts() -> dict:
    try:
        from ccl_chromium_reader import ccl_chromium_indexeddb
    except ImportError:
        print("error: ccl_chromium_reader not installed", file=sys.stderr)
        sys.exit(1)

    if not os.path.isdir(INDEXEDDB_BASE):
        print(f"error: Teams IndexedDB not found at {INDEXEDDB_BASE}", file=sys.stderr)
        sys.exit(1)

    wrapper = ccl_chromium_indexeddb.WrappedIndexDB(INDEXEDDB_BASE, BLOB_BASE)

    # Collect the highest-version record per thread ID
    best: dict[str, tuple[float, dict]] = {}

    for db_info in wrapper.database_ids:
        if not db_info.name.startswith(CONVERSATION_DB_PREFIX):
            continue
        db = wrapper[db_info.dbid_no]
        store = db.get_object_store_by_name(CONVERSATION_STORE)

        for record in store.iterate_records(live_only=False):
            try:
                v = record.value
                if not isinstance(v, dict):
                    continue
                thread_id = v.get("id") or str(record.key)
                version   = float(v.get("version", 0) or 0)
                existing  = best.get(thread_id)
                if existing is None or version > existing[0]:
                    best[thread_id] = (version, v)
            except Exception:
                pass
        break

    records  = {tid: val for tid, (_, val) in best.items()}
    user_mri = get_user_mri(records)

    unreads  = 0
    mentions = 0

    for thread_id, v in records.items():
        try:
            member      = v.get("memberProperties", {}) or {}
            interest    = member.get("interest", "")
            not_interested = interest == "NotInterested"

            last_msg_time = float(v.get("lastMessageTimeUtc", 0) or 0)
            props         = v.get("properties", {}) or {}
            horizon_str   = props.get("consumptionhorizon", "")
            horizon_ts    = parse_consumption_horizon(horizon_str)

            if last_msg_time <= horizon_ts:
                continue  # fully read — skip entirely

            # Always check mentions regardless of interest level
            if check_mention(v, horizon_ts, user_mri):
                mentions += 1

            # Only count as unread if the user hasn't muted this thread
            if not not_interested:
                unreads += 1

        except Exception:
            pass

    return {"unreads": int(unreads), "mentions": int(mentions)}


if __name__ == "__main__":
    counts = fetch_counts()
    print(json.dumps(counts))