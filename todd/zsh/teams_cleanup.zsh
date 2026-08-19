# teams-cleanup-version: 1
# Microsoft Teams leftover caches can crash the app. Clean them once per boot.
# macOS terminals start login shells, so ~/.zlogin runs on every new window —
# do not call sudo there unconditionally.

todd_teams_cleanup_once_per_boot() {
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

  local -a targets existing remaining
  targets=(
    "$HOME/Library/Group Containers/UBF8T346G9.com.microsoft.teams"
    "$HOME/Library/Containers/com.microsoft.teams2"
  )

  local t
  for t in "${targets[@]}"; do
    [[ -e "$t" ]] && existing+=("$t")
  done
  (( ${#existing[@]} )) || return 0

  echo "Cleaning Microsoft Teams caches (once since last restart):"
  for t in "${existing[@]}"; do
    if rm -rf "$t" 2>/dev/null; then
      echo "  removed: $t"
    else
      remaining+=("$t")
    fi
  done
  (( ${#remaining[@]} )) || return 0

  echo "Some Teams cache dirs need elevated permissions:"
  for t in "${remaining[@]}"; do
    echo "  $t"
  done
  echo "About to run: sudo rm -rf <those paths>"
  echo "This is requested at most once per reboot. Ctrl-C skips until the next restart."
  sudo rm -rf "${remaining[@]}"
}

todd_teams_cleanup_once_per_boot
