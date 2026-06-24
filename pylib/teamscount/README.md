# teamscount

A small utility (and Emacs minor mode) that reads Microsoft Teams unread/mention
counts from the local LevelDB store and displays them in the Emacs mode line.

## Requirements

- macOS with Microsoft Teams 2.0 installed
- Python 3.9+
- Emacs 27.1+ with `nerd-icons`

## Setup

```bash
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

## Usage

Run the probe script to verify Teams data is readable:

```bash
python3 teamscount.py
```

## How it works

Teams 2.0 on macOS stores its state in a LevelDB database inside the app
sandbox at:
~/Library/Containers/com.microsoft.teams2/Data/Library/Application Support/

Microsoft/MSTeams/EBWebView/WV2Profile_tfw/IndexedDB/

https_teams.microsoft.com_0.indexeddb.leveldb/

The Python script opens that database read-only, scans for unread/mention
keys, and prints a JSON result.

## License

MIT
