#!/usr/bin/env python3
"""
teamscount.py - Read Microsoft Teams unread and mention counts from local IndexedDB.

Outputs a single JSON object to stdout:
  {
    "unreads": <int>,
    "mentions": <int>,
    "threads": [
      {
        "id": "<thread-id>",
        "name": "<channel or chat name>",
        "type": "<Topic|Chat|...>",
        "mention": <bool>,
        "group_id": "<AAD group id or null>",
        "team_id": "<team thread id or null>",
        "oldest_unread_id": "<message id or null>"
      },
      ...
    ]
  }

Exit codes:
  0 - success
  1 - Teams IndexedDB not found or unreadable
"""

import json
import os
import shutil
import sys
import tempfile

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

# Thread types that are internal system entries, not real channels or chats
SKIP_TYPES = {"StreamOfNotes", "Thread"}

# Thread ID prefixes that are internal Teams system threads
SKIP_ID_PREFIXES = ("48:",)


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


def thread_display_name(v: dict) -> str:
    """Return a human-readable name for the thread."""
    tp = v.get("threadProperties", {}) or {}
    topic = tp.get("topic") or tp.get("topicThreadTopic") or tp.get("spaceThreadTopic")
    if topic:
        return topic
    ct = v.get("chatTitle", {}) or {}
    long_title = ct.get("longTitle")
    if long_title:
        return long_title
    return v.get("id", "Unknown")


def check_mention(v: dict, horizon_ts: float, user_mri: str) -> bool:
    """Return True if the last unread message directly mentions user_mri."""
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

    # Copy to a temp dir to avoid conflicts with the live Teams LevelDB lock
    temp_dir  = tempfile.mkdtemp()
    db_copy   = os.path.join(temp_dir, "db")
    blob_copy = os.path.join(temp_dir, "blob")

    try:
        shutil.copytree(INDEXEDDB_BASE, db_copy)
        if os.path.isdir(BLOB_BASE):
            shutil.copytree(BLOB_BASE, blob_copy)

        wrapper = ccl_chromium_indexeddb.WrappedIndexDB(
            db_copy,
            blob_copy if os.path.isdir(blob_copy) else None
        )

        best: dict[str, tuple[float, dict]] = {}

        for db_info in wrapper.database_ids:
            if not db_info.name.startswith(CONVERSATION_DB_PREFIX):
                continue
            db    = wrapper[db_info.dbid_no]
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
            break  # Only one conversation-manager DB

    finally:
        shutil.rmtree(temp_dir, ignore_errors=True)

    records  = {tid: val for tid, (_, val) in best.items()}
    user_mri = get_user_mri(records)

    unreads  = 0
    mentions = 0
    threads  = []

    for thread_id, v in records.items():
        try:
            # Skip internal system thread types and IDs
            ctype = v.get("type", "")
            if ctype in SKIP_TYPES:
                continue
            if any(thread_id.startswith(p) for p in SKIP_ID_PREFIXES):
                continue
            # Skip Space type (team containers) — they show unread when any
            # sub-channel is unread, but the channel itself isn't a destination
            if ctype == "Space":
                continue

            member         = v.get("memberProperties", {}) or {}
            interest       = member.get("interest", "")
            not_interested = interest == "NotInterested"

            last_msg_time = float(v.get("lastMessageTimeUtc", 0) or 0)
            props         = v.get("properties", {}) or {}
            horizon_str   = props.get("consumptionhorizon", "")
            horizon_ts    = parse_consumption_horizon(horizon_str)

            if last_msg_time <= horizon_ts:
                continue  # fully read

            is_mention = check_mention(v, horizon_ts, user_mri)
            if is_mention:
                mentions += 1
            if not not_interested:
                unreads += 1
            else:
                if not is_mention:
                    continue  # muted and no mention — skip from thread list

            tp       = v.get("threadProperties", {}) or {}
            group_id = tp.get("groupId") or v.get("teamId") or None
            team_id  = v.get("teamId") or None

            oldest_unread_id = None
            if horizon_str:
                parts = horizon_str.split(";")
                if len(parts) >= 3 and parts[2]:
                    oldest_unread_id = parts[2]

            threads.append({
                "id":               thread_id,
                "name":             thread_display_name(v),
                "type":             ctype,
                "mention":          is_mention,
                "group_id":         group_id,
                "team_id":          team_id,
                "oldest_unread_id": oldest_unread_id,
            })

        except Exception:
            pass

    threads.sort(key=lambda t: (not t["mention"], t["name"].lower()))
    return {"unreads": int(unreads), "mentions": int(mentions), "threads": threads}


if __name__ == "__main__":
    counts = fetch_counts()
    print(json.dumps(counts))