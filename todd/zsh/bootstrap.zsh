# bootstrap-managed shell config
# home .zshrc migration (bootstrap)
# Shell config lives under ~/.config/todd/zsh/ (ZDOTDIR=~/.config/zsh in ~/.zshenv).
# Interactive startup file: ~/.config/zsh/.zshrc (sources ~/.config/todd/zsh/zshrc).
# Bootstrap-managed settings: ~/.config/todd/zsh/bootstrap.zsh
#
# NOTE: do not source $ZDOTDIR/.zshrc from here. bootstrap.zsh is itself
# sourced from the ZDOTDIR chain (.zshrc -> todd/zsh/zshrc -> bootstrap.zsh),
# so re-sourcing .zshrc recurses until zsh hits its recursion limit.

# GCC runtime libs for libgccjit native compilation (bootstrap)
export LIBRARY_PATH="/opt/homebrew/lib/gcc/current/gcc/aarch64-apple-darwin25/16:/opt/homebrew/lib/gcc/current${LIBRARY_PATH:+:$LIBRARY_PATH}"

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
