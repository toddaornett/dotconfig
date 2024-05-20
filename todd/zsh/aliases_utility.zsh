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
        while IFS= read -r line; do
          # Check if the line contains a CREATE TABLE command
          if [[ "$line" =~ CREATE[[:space:]]+TABLE ]]; then
            # Extract the table name
            local table_name=$(echo "$line" | awk -F' ' '{print $3}' | sed 's/(//')
            if [[ -n "$table_name" ]]; then
              echo "DROP TABLE IF EXISTS $table_name;"
            fi
          fi

          # Check if the line contains a CREATE INDEX command
          if [[ "$line" =~ CREATE[[:space:]]+INDEX ]]; then
            # Extract the index name
            local index_name=$(echo "$line" | awk -F' ' '{print $3}')
            if [[ -n "$index_name" ]]; then
              echo "DROP INDEX IF EXISTS $index_name;"
            fi
          fi

          # Check if the line contains a CREATE INDEX command
          if [[ "$line" =~ CREATE[[:space:]]+UNIQUE[[:space:]]+INDEX ]]; then
            # Extract the index name
            local index_name=$(echo "$line" | awk -F' ' '{print $4}')
            if [[ -n "$index_name" ]]; then
              echo "DROP INDEX IF EXISTS $index_name;"
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
