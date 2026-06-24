#!/usr/bin/env python3
"""
teamscount.py - Read Microsoft Teams unread and mention counts from local IndexedDB.

Outputs a single JSON object to stdout:
  {"unreads": <int>, "mentions": <int>}

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

# The store that holds aggregate unread thread counts (chat + channel)
UNREAD_STORE = "threads-internal-items"

# The store that holds aggregate direct mention counts (chat + channel)
MENTIONS_STORE = "mentions-internal-items"

# The DB name prefix for the messaging slice manager
MESSAGING_DB_PREFIX = "Teams:messaging-slice-manager"


def get_charms_value(db, store_name: str) -> dict:
    """Return the 'charms' record value from the given object store, or {}."""
    store = db.get_object_store_by_name(store_name)
    for record in store.iterate_records(live_only=True):
        if str(record.key) == "<IdbKey charms>":
            v = record.value
            if isinstance(v, dict) and "value" in v:
                return v["value"]
    return {}


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

    unreads = 0
    mentions = 0

    for db_info in wrapper.database_ids:
        if not db_info.name.startswith(MESSAGING_DB_PREFIX):
            continue
        db = wrapper[db_info.dbid_no]

        # Unread messages: sum chat + channel unreadCount
        charms = get_charms_value(db, UNREAD_STORE)
        if charms:
            unreads = (
                charms.get("chat", {}).get("unreadCount", 0)
                + charms.get("channel", {}).get("unreadCount", 0)
            )

        # Direct mentions: sum chat + channel totalCount
        charms = get_charms_value(db, MENTIONS_STORE)
        if charms:
            mentions = (
                charms.get("chat", {}).get("totalCount", 0)
                + charms.get("channel", {}).get("totalCount", 0)
            )

        break  # Only one messaging-slice-manager DB

    return {"unreads": int(unreads), "mentions": int(mentions)}


if __name__ == "__main__":
    counts = fetch_counts()
    print(json.dumps(counts))
