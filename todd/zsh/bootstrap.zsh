# bootstrap-managed shell config
# Managed by ~/.config/bootstrap.sh — do not edit bootstrap markers by hand.

export LLM_PROVIDERS="ollama"

# Homebrew/macOS build flags (bootstrap)
if [[ "$(uname -s)" == Darwin ]]; then
  [[ -z "$SDKROOT" ]] && export SDKROOT="$(xcrun --sdk macosx --show-sdk-path 2>/dev/null)"
  if [[ -n "$SDKROOT" ]]; then
    [[ " $CFLAGS " != *" -isysroot "* ]] && export CFLAGS="-isysroot $SDKROOT"
    [[ " $LDFLAGS " != *" -isysroot "* ]] && export LDFLAGS="${LDFLAGS:+$LDFLAGS }-isysroot $SDKROOT"
  fi
  [[ " $CPPFLAGS " != *" -I/opt/homebrew/include "* ]] && export CPPFLAGS="${CPPFLAGS:+$CPPFLAGS }-I/opt/homebrew/include"
  [[ " $LDFLAGS " != *" -L/opt/homebrew/lib "* ]] && export LDFLAGS="${LDFLAGS:+$LDFLAGS }-L/opt/homebrew/lib"
  [[ ":$PKG_CONFIG_PATH:" != *":/opt/homebrew/opt/boost/lib/pkgconfig:"* ]] && export PKG_CONFIG_PATH="/opt/homebrew/opt/boost/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
fi

# GCC runtime libs for libgccjit native compilation (bootstrap)
export LIBRARY_PATH="/opt/homebrew/lib/gcc/current/gcc/aarch64-apple-darwin25/16:/opt/homebrew/lib/gcc/current${LIBRARY_PATH:+:$LIBRARY_PATH}"

path+=/Users/todd/.config/doom-emacs/bin
export CARGO_NET_GIT_FETCH_WITH_CLI=true
export PUPPETEER_EXECUTABLE_PATH="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
export DOCKER_CONTEXT=colima

# mise version manager (bootstrap)
if command -v mise >/dev/null 2>&1; then
  eval "$(mise activate zsh)"
fi
path+=/Users/todd.ornett/.config/doom-emacs/bin
