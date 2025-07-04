# utility aliases
alias a=alias
alias ua=unalias
alias c=clear
alias wl="wc -l"

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

  version_gt() {
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
            if [ "$command" != "" ]; then
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
  unset -f version_gt
  return 0
}

# Usage: pgd <db schema name>
# Verify environment variables in the command below
# and be ready to enter password when prompted
function pgd {
  pg_dump -h $DB_HOST -U $DB_USER -d $DB_NAME -n $1 -Fc -f ~/db_${MYENV:-local}.dump
}

# Usage: pgr
# Verify environment variables in the command below
function pgr {
  pg_restore -U $USERNAME -d $DB_NAME --no-privileges --no-owner -Fc ~/db_${MYENV:-local}.dump
}

# Switch to main database for this environment
function dbl {
  unset MYENV_DB_TEST
  load_companion_env
  echo "Using $(env | grep DB | grep NAME | head -1)"
}

# Switch to automatic testing database
function dbt {
  export MYENV_DB_TEST=_test
  load_companion_env
  echo "Using $(env | grep DB | grep NAME | head -1)"
}

# save git project unstaged working files
function wips {
  local dest="${HOME}/wip/$(basename $(pwd))"
  mkdir -p "$dest"
  local copy
  if command -v -- gcp >/dev/null 2>&1; then
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

# retrieve files from wips saved files to current directory
function wipc {
  local src="${HOME}/wip/$(basename $(pwd))"
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

function clear_cache_ms_teams {
  if [ -d "~/Library/Application\ Support/Microsoft/Teams" ]; then
    rm -rf "~/Library/Application\ Support/Microsoft/Teams"
  fi
  if [ -d "~/Library/Group Containers/UBF8T346G9.com.microsoft.teams" ]; then
    rm -rf "~/Library/Group Containers/UBF8T346G9.com.microsoft.teams"
  fi
  if [ -d "~/Library/Containers/com.microsoft.teams2" ]; then
    rm -rf "~/Library/Containers/com.microsoft.teams2"
  fi
}

# Remove duplicate entries from a list of strings
#
# Usage: dedupe [<input string> [<separator character]]
#   <input string>         default PATH environment variable
#   <separator character>  default ":"
# Note that if <separator character> is specified, then
# <input path style string> must also be specified.
function dedupe {
  echo "${1:-$PATH}" | awk \
    -v sep="${2:-:}" '
    BEGIN {
      RS=sep
    }
    {
      sub(/\n$/,"")
      if (!A[$0]) {
        A[$0]=1
        printf((NR==1) ? "" : sep)
        printf($0)
      }
    }'
}

# convert arabic number to roman
function d2r() {
  emulate -L zsh
  setopt KSH_ARRAYS
  local num
  local result
  local i
  local values
  local numerals
  values=(1000 900 500 400 100 90 50 40 10 9 5 4 1)
  numerals=("M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I")

  if [[ $# -eq 0 ]]; then
    echo "Usage: d2r <number>"
    return 1
  fi

  num=$1

  if ! [[ "$num" =~ ^[0-9]+$ ]]; then
    echo "Error: provide a positive integer."
    return 1
  fi

  result=""
  for ((i = 0; i < ${#values[@]}; i++)); do
    while ((num >= values[i])); do
      result+="${numerals[i]}"
      ((num -= values[i]))
    done
  done

  echo "$result"
}

# convert roman number to arabic
r2d() {
  if [ $# -eq 0 ]; then
    echo "Usage: r2d <roman numeral>"
    return 1
  fi

  local input result matched sym value i len
  input=$(printf '%s' "$1" | tr '[:lower:]' '[:upper:]')
  result=0

  emulate -L zsh
  setopt KSH_ARRAYS
  local -a symbols
  local -a values
  symbols=("M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I")
  values=(1000 900 500 400 100 90 50 40 10 9 5 4 1)

  while [ -n "$input" ]; do
    matched=0
    i=0
    while [ $i -lt ${#symbols[@]} ]; do
      sym="${symbols[$i]}"
      value="${values[$i]}"
      len=${#sym}
      if [ "${input:0:$len}" = "$sym" ]; then
        result=$((result + value))
        input="${input:$len}"
        matched=1
        break
      fi
      i=$((i + 1))
    done
    if [ "$matched" -eq 0 ]; then
      echo "Error: Invalid Roman numeral."
      return 1
    fi
  done
  echo "$result"
}

# Copy username and password from specified key to system clipboard
function passC {
  local u
  local p
  u="$(pass $1 | grep -i '^username:' | cut -d ' ' -f 2 | tr -d '\n')"
  p="$(pass $1 | head -1 | tr -d '\n')"
  echo -n "$u $p" | pbcopy
}
