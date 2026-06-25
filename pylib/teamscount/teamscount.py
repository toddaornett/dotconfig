#!/usr/bin/env python3
"""
teamscount.py - Read Microsoft Teams unread and mention counts from local IndexedDB.
"""

import glob
import json
import os
import shutil
import sys
import tempfile

# 1. Base Teams container profile directory
TEAMS_PROFILE_DIR = os.path.expanduser(
    "~/Library/Containers/com.microsoft.teams2/Data/Library/Application Support"
    "/Microsoft/MSTeams/EBWebView/WV2Profile_tfw"
)

# 2. Dynamically search for the LevelDB folder path across common structural permutations
possible_db_paths = [
    # Search for any numbered WebStorage variations (e.g., .../WebStorage/4/IndexedDB.leveldb)
    *glob.glob(os.path.join(TEAMS_PROFILE_DIR, "WebStorage", "*", "IndexedDB.leveldb")),
    # Fallback to direct IndexedDB paths if structured differently
    *glob.glob(os.path.join(TEAMS_PROFILE_DIR, "IndexedDB", "*.indexeddb.leveldb")),
]

# 3. Choose the first valid directory found
if not possible_db_paths or not os.path.isdir(possible_db_paths[0]):
    print(
        f"error: Teams IndexedDB folder could not be found dynamically inside {TEAMS_PROFILE_DIR}",
        file=sys.stderr,
    )
    sys.exit(1)

ORIGINAL_INDEXEDDB_BASE = possible_db_paths[0]

# 4. Construct matching Blob directory based on what style of path was found
if "WebStorage" in ORIGINAL_INDEXEDDB_BASE:
    ORIGINAL_BLOB_BASE = ORIGINAL_INDEXEDDB_BASE.replace(
        "IndexedDB.leveldb", "IndexedDB.blob"
    )
else:
    ORIGINAL_BLOB_BASE = ORIGINAL_INDEXEDDB_BASE.replace(
        ".indexeddb.leveldb", ".indexeddb.blob"
    )

CONVERSATION_DB_PREFIX = "Teams:conversation-manager"
CONVERSATION_STORE = "conversations"


def parse_consumption_horizon(horizon: str) -> float:
    if not horizon:
        return 0.0
    try:
        return float(horizon.split(";")[0])
    except (ValueError, IndexError):
        return 0.0


def get_user_mri(records: dict) -> str:
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
    if not user_mri:
        return False
    try:
        last_msg = v.get("lastMessage", {})
        if not isinstance(last_msg, dict):
            return False
        msg_time = float(last_msg.get("id", 0) or 0)
        if msg_time <= horizon_ts:
            return False
        msg_props = last_msg.get("properties", {}) or {}
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

    if not os.path.isdir(ORIGINAL_INDEXEDDB_BASE):
        print(
            f"error: Teams IndexedDB not found at {ORIGINAL_INDEXEDDB_BASE}",
            file=sys.stderr,
        )
        sys.exit(1)

    # WORKAROUND FOR LOCKING: Copy data safely to a temporary directory
    temp_dir = tempfile.mkdtemp()
    db_copy_path = os.path.join(temp_dir, "IndexedDB.leveldb")
    blob_copy_path = os.path.join(temp_dir, "IndexedDB.blob")

    try:
        shutil.copytree(
            ORIGINAL_INDEXEDDB_BASE, db_copy_path, symlinks=False, ignore=None
        )
        if os.path.isdir(ORIGINAL_BLOB_BASE):
            shutil.copytree(
                ORIGINAL_BLOB_BASE, blob_copy_path, symlinks=False, ignore=None
            )

        wrapper = ccl_chromium_indexeddb.WrappedIndexDB(
            db_copy_path, blob_copy_path if os.path.isdir(blob_copy_path) else None
        )
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
                    version = float(v.get("version", 0) or 0)
                    existing = best.get(thread_id)
                    if existing is None or version > existing[0]:
                        best[thread_id] = (version, v)
                except Exception as e:
                    print(f"debug: error reading record: {e}", file=sys.stderr)

        # Removed the broken 'break' statement here to allow full database iteration.

    finally:
        # Always clean up temporary copies immediately
        shutil.rmtree(temp_dir, ignore_errors=True)

    records = {tid: val for tid, (_, val) in best.items()}
    user_mri = get_user_mri(records)

    unreads = 0
    mentions = 0
    threads = []

    for thread_id, v in records.items():
        try:
            member = v.get("memberProperties", {}) or {}
            interest = member.get("interest", "")
            not_interested = interest == "NotInterested"

            last_msg_time = float(v.get("lastMessageTimeUtc", 0) or 0)
            props = v.get("properties", {}) or {}
            horizon_str = props.get("consumptionhorizon", "")
            horizon_ts = parse_consumption_horizon(horizon_str)

            if last_msg_time <= horizon_ts:
                continue

            is_mention = check_mention(v, horizon_ts, user_mri)
            if is_mention:
                mentions += 1
            if not not_interested:
                unreads += 1
            else:
                if not is_mention:
                    continue

            tp = v.get("threadProperties", {}) or {}
            group_id = tp.get("groupId") or v.get("teamId") or None
            team_id = v.get("teamId") or None

            oldest_unread_id = None
            if horizon_str:
                parts = horizon_str.split(";")
                if len(parts) >= 3 and parts[2]:
                    oldest_unread_id = parts[2]

            threads.append(
                {
                    "id": thread_id,
                    "name": thread_display_name(v),
                    "type": v.get("type", ""),
                    "mention": is_mention,
                    "group_id": group_id,
                    "team_id": team_id,
                    "oldest_unread_id": oldest_unread_id,
                }
            )
        except Exception:
            pass

    threads.sort(key=lambda t: (not t["mention"], t["name"].lower()))
    return {"unreads": int(unreads), "mentions": int(mentions), "threads": threads}


if __name__ == "__main__":
    counts = fetch_counts()
    print(json.dumps(counts))
