# Clean Microsoft Teams caches which can crash the app.
# Cleans once per boot and interactively via clear_cache_microsoft_teams

function clear_cache_microsoft_teams {
  local -a targets existing remaining
  targets=(
    "$HOME/Library/Group Containers/UBF8T346G9.com.microsoft.teams"
    "$HOME/Library/Containers/com.microsoft.teams2"
    "$HOME/Library/Caches/com.microsoft.teams"
    "$HOME/Library/Caches/com.microsoft.teams.shipit"
    "$HOME/Library/Application Support/Microsoft/Teams"
    "$HOME/Library/Application Support/Microsoft/Teams/Application Cache/Cache"
    "$HOME/Library/Application Support/Microsoft/Teams/blob/storage"
    "$HOME/Library/Application Support/Microsoft/Teams/Cache"
    "$HOME/Library/Application Support/Microsoft/Teams/databases"
    "$HOME/Library/Application Support/Microsoft/Teams/GPUCache"
    "$HOME/Library/Application Support/Microsoft/Teams/IndexedDB"
    "$HOME/Library/Application Support/Microsoft/Teams/Local Storage"
    "$HOME/Library/Application Support/Microsoft/Teams/tmp"
  )

  local t
  for t in "${targets[@]}"; do
    [[ -e "$t" ]] && existing+=("$t")
  done
  ((${#existing[@]})) || {
    echo "No Microsoft Teams cache directories found."
    return 0
  }

  echo "Cleaning Microsoft Teams caches:"
  for t in "${existing[@]}"; do
    if rm -rf "$t" 2>/dev/null; then
      echo "  removed: $t"
    else
      remaining+=("$t")
    fi
  done
  ((${#remaining[@]})) || return 0

  echo "Some Microsoft Teams cache directories need elevated permissions:"
  for t in "${remaining[@]}"; do
    echo "  $t"
  done
  echo "About to run: sudo rm -rf <those paths>"
  sudo rm -rf "${remaining[@]}"
}

microsoft_teams_cleanup_once_per_boot() {
  [[ -o interactive ]] || return 0

  local stamp_dir="${XDG_CACHE_HOME:-$HOME/.cache}/todd"
  local stamp="$stamp_dir/teams-cache-cleanup.boot"
  local boot_sec
  boot_sec="$(sysctl -n kern.boottime 2>/dev/null | awk '{print $4}' | tr -d ',')"
  [[ -n "$boot_sec" ]] || return 0

  if [[ -f "$stamp" && "$(<"$stamp")" == "$boot_sec" ]]; then
    return 0
  fi

  mkdir -p "$stamp_dir"
  print -r -- "$boot_sec" >"$stamp"

  echo "Running Teams cache cleanup (once since last restart):"
  clear_cache_microsoft_teams
}

microsoft_teams_cleanup_once_per_boot
