# Clean homebrew cache

function clear_cache_homebrew {
  rm -rf "$HOME/Library/Caches/Homebrew/*"
  rm -rf $(brew --cache)/*
}
