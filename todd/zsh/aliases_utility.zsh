# utility aliases
alias a=alias
alias ua=unalias
alias c=clear
alias wl="wc -l"

function ltrim () {
  sed -E 's/^[[:blank:]]+//'
}

function rtrim () {
  sed -E 's/[[:blank:]]+$//'
}

function trim () {
  ltrim | rtrim
}

# undo some sql create table and index commands in reverse version order of script files
# Note that this is just a start and not particularly flexible at this stage
# The drop commands are written on stdout.
# Nothing else is handled. So, for example, there is no reversal of ALTER statements.
function stomp {
  # Check if a version number is provided as an argument
  if [ -z "$1" ]; then
    echo "Usage: $(basename $0) <version-number>"
    return 1
  fi

  local TARGET_VERSION="$1"

  # Directory containing the migration files
  local MIGRATION_DIR="./db/sql"

  local version_gt() {
    [ "$(printf '%s\n' "$1" "$2" | sort -V | head -n 1)" != "$1" ]
  }

  # Get a list of migration files sorted by version
  local migration_files=($(ls "$MIGRATION_DIR"/V*.sql | sort -Vr))

  # Loop through the migration files
  local file
  for file in "${migration_files[@]}"; do
    # Extract the version number from the filename
    local filename=$(basename "$file")
    if [[ $filename =~ ^"V" ]]; then
      local version=$(echo "$filename" | awk -F_ '{print $1}' | sed 's/^V//')
      if version_gt "$version" "$TARGET_VERSION"; then
        echo "DELETE FROM flyway_schema_history WHERE version='$version';"
	local OLDIFS=$IFS
        local command=""
        while IFS= read -r line; do
          # Check if the line contains a CREATE TABLE command
          if [[ "$line" =~ CREATE[[:space:]]+TABLE[[:space:]]+([_A-Za-z0-9]+).* ]]; then
	    command=""
            echo "DROP TABLE IF EXISTS ${match[1]};"
          fi

          # Check if the line contains a CREATE INDEX command
          if [[ "$line" =~ CREATE[[:space:]]+INDEX[[:space:]]+(IF[[:space:]]+NOT[[:space:]]+EXISTS[[:space:]]+)?([_A-Za-z0-9]+).* ]]; then
	    command=""
            echo "DROP INDEX IF EXISTS ${match[2]};"
          fi

          # Check if the line contains a CREATE UNIQUE INDEX command
          if [[ "$line" =~ CREATE[[:space:]]+UNIQUE[[:space:]]+INDEX[[:space:]]+(IF[[:space:]]+NOT[[:space:]]+EXISTS[[:space:]]+)?([^ ]+).* ]]; then
	    command=""
            echo "DROP INDEX IF EXISTS ${match[2]};"
          fi

          # Check if the line contains a CREATE TRIGGER command
          if [[ "$line" =~ CREATE[[:space:]]+TRIGGER[[:space:]]+([^ ]+).* ]]; then
            command="DROP TRIGGER IF EXISTS ${match[1]}"
          fi

          # Check if the line contains a CREATE OR REPLACE FUNCTION command
          if [[ "$line" =~ CREATE[[:space:]]+OR[[:space:]]+REPLACE[[:space:]]+FUNCTION[[:space:]]+([^ ]+)(\\\().* ]]; then
	    command=""
            echo "DROP FUNCTION IF EXISTS ${match[1]} CASCADE;"
          fi

          # Check if the line contains a CREATE OR REPLACE PROCEDURE command
          if [[ "$line" =~ CREATE[[:space:]]+OR[[:space:]]+REPLACE[[:space:]]+PROCEDURE[[:space:]]+([^ ]+)(\\\().* ]]; then
	    command=""
            echo "DROP PROCEDURE IF EXISTS ${match[1]} CASCADE;"
	  fi

          # Check there is ON <table name> for completing buffered command string
          if [[ "$line" =~ .*[[:space:]]ON[[:space:]]([^ ]+).* ]]; then
	    if [ "$command" != "" ] ; then
	      echo "$command ON ${match[1]} CASCADE;"
	      command=""
	    fi
          fi
        done <"$file"
	IFS=$OLDIFS
      else
        break
      fi
    fi
  done
  return 0
}

function wips {
  local dest="${HOME}/wip/$(basename `pwd`)"
  mkdir -p "$dest"
  local copy
  if command -v -- gcp > /dev/null 2>&1; then
    copy=gcp
  else
    copy=cp
  fi
  local files=($(git status -s | cut -c 4- | xargs))
  local f
  for f in $files; do
    $copy --parents -r "$f" "$dest"
  done
}

function wipc {
  local src="${HOME}/wip/$(basename `pwd`)"
  local folders=($(find "$src"/* -type d | sed -e "s|$src/||" | xargs))
  local d
  for d in $folders; do
    mkdir -p "$d"
  done
  local files=($(find "$src" -type f | sed -e "s|$src/||" | xargs))
  local f
  for f in $files; do
    cp "$src/$f" "$f"
  done
}

function uuidgen {
  /usr/bin/uuidgen | tr '[:upper:]' '[:lower:]' | tr -d '\n'
}
