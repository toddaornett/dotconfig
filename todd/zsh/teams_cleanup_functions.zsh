# teams-cleanup.zsh
#
# Microsoft Teams (macOS) cleanup helpers for zsh.
# Source this from ~/.zshrc:   source ~/path/to/teams-cleanup.zsh
#
# Commands:
#   clear_cache_microsoft_teams   Remove caches only (stays signed in).
#   reset_microsoft_teams         Full reset: also removes app data, so you
#                                 must sign in again. Asks for confirmation.
#
# Automatic behavior:
#   On the first interactive shell after each reboot, caches are cleaned
#   once. It never uses sudo automatically and it skips (and retries in the
#   next shell) if Teams is running or the cleanup fails.
#
# Note: if removal under ~/Library/Containers fails with "Operation not
# permitted", grant your terminal app Full Disk Access (System Settings >
# Privacy & Security). sudo does not bypass SIP/TCC protections.

_teams_is_running() {
  local name
  for name in MSTeams "Microsoft Teams" Teams; do
    pgrep -x "$name" >/dev/null 2>&1 && return 0
  done
  return 1
}

# _teams_purge [--no-sudo] PATH...
# Removes each existing path; optionally offers sudo for any that remain.
_teams_purge() {
  emulate -L zsh
  local allow_sudo=1 t
  local -a existing remaining

  if [[ "$1" == --no-sudo ]]; then
    allow_sudo=0
    shift
  fi

  [[ -n "$HOME" && "$HOME" != "/" ]] || {
    echo "Refusing to run: bad \$HOME." >&2
    return 1
  }

  for t in "$@"; do
    [[ -e "$t" || -L "$t" ]] && existing+=("$t")
  done
  (($#existing)) || {
    echo "Nothing to remove."
    return 0
  }

  for t in "${existing[@]}"; do
    rm -rf -- "$t" 2>/dev/null
    if [[ -e "$t" || -L "$t" ]]; then
      remaining+=("$t")
    else
      echo "  removed: $t"
    fi
  done
  (($#remaining)) || return 0

  echo "Could not remove:" >&2
  printf '  %s\n' "${remaining[@]}" >&2
  ((allow_sudo)) || return 1

  read -q "REPLY?Retry those with sudo? [y/N] " || {
    echo
    return 1
  }
  echo
  sudo rm -rf -- "${remaining[@]}"
}

clear_cache_microsoft_teams() {
  emulate -L zsh
  local flag="$1"

  if _teams_is_running; then
    echo "Quit Microsoft Teams first." >&2
    return 1
  fi

  local lib="$HOME/Library"
  local app="$lib/Application Support/Microsoft/Teams"
  local wv="Data/Library/Application Support/Microsoft/MSTeams/EBWebView"
  local -a targets=(
    # Classic Teams
    "$lib/Caches/"com.microsoft.teams*(N)
    "$app/Cache"(N)
    "$app/Code Cache"(N)
    "$app/GPUCache"(N)
    "$app/Application Cache/Cache"(N)
    "$app/blob_storage"(N)
    "$app/tmp"(N)
    # New Teams: contents of the sandbox/group Caches folders
    "$lib/Containers/"com.microsoft.teams*"/Data/Library/Caches/"*(ND)
    "$lib/Group Containers/"*.com.microsoft.teams*"/Library/Caches/"*(ND)
    # New Teams: WebView2 caches
    "$lib/Containers/"com.microsoft.teams*"/$wv/"{GraphiteDawnCache,component_crx_cache}(N)
    "$lib/Containers/"com.microsoft.teams*"/$wv/"{Default,WV2Profile_*}"/"{Cache,"Code Cache",GPUCache,DawnGraphiteCache,DawnWebGPUCache,"Service Worker/CacheStorage"}(N)
  )

  echo "Cleaning Microsoft Teams caches:"
  _teams_purge ${flag:+"$flag"} "${targets[@]}"
}

reset_microsoft_teams() {
  emulate -L zsh

  if _teams_is_running; then
    echo "Quit Microsoft Teams first." >&2
    return 1
  fi

  echo "This deletes ALL local Teams data (settings, sessions, databases)."
  echo "You will need to sign in again."
  read -q "REPLY?Continue? [y/N] " || {
    echo
    echo "Cancelled."
    return 1
  }
  echo

  local lib="$HOME/Library"
  local -a targets=(
    "$lib/Group Containers/"*.com.microsoft.teams*(N)
    "$lib/Containers/"com.microsoft.teams*(N)
    "$lib/Caches/"com.microsoft.teams*(N)
    "$lib/Application Support/Microsoft/Teams"(N)
  )

  echo "Resetting Microsoft Teams:"
  _teams_purge "${targets[@]}"
}

_teams_cleanup_once_per_boot() {
  emulate -L zsh
  [[ -o interactive ]] || return 0

  local stamp_dir="${XDG_CACHE_HOME:-$HOME/.cache}/teams-cleanup"
  local stamp="$stamp_dir/last-boot"
  local boot_sec
  boot_sec="$(sysctl -n kern.boottime 2>/dev/null | sed -E 's/.*sec = ([0-9]+).*/\1/')"
  [[ "$boot_sec" == <-> ]] || return 0

  [[ -f "$stamp" && "$(<"$stamp")" == "$boot_sec" ]] && return 0

  _teams_is_running && return 0

  echo "Running Teams cache cleanup (once since last restart):"
  if clear_cache_microsoft_teams --no-sudo; then
    mkdir -p "$stamp_dir" && print -r -- "$boot_sec" >"$stamp"
  else
    echo "Cleanup incomplete; will retry in the next shell. Run clear_cache_microsoft_teams to resolve manually." >&2
  fi
}

_teams_cleanup_once_per_boot
