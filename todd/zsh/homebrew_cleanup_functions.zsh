# Clean homebrew cache

function clear_cache_homebrew() {
  if ! command -v brew >/dev/null 2>&1; then
    echo "Error: Homebrew is not installed or not in your PATH." >&2
    return 1
  fi

  echo "Running official Homebrew cache cleanup..."
  brew cleanup --prune=all

  local brew_cache
  brew_cache=$(brew --cache 2>/dev/null)
  local cache_paths=(
    "$HOME/Library/Caches/Homebrew"
    "$brew_cache"
  )
  echo "Purging leftover cache artifacts and lockfiles..."
  for path in "${cache_paths[@]}"; do
    if [[ -n "$path" && -d "$path" ]]; then
      rm -rf "$path"
      mkdir -p "$path"
    fi
  done

  echo "Homebrew cache successfully cleared!"
}
