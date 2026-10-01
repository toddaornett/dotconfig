# bootstrap-managed shell config
# home .zshrc migration (bootstrap)
# Shell config lives under ~/.config/todd/zsh/ (ZDOTDIR=~/.config/zsh in ~/.zshenv).
# Interactive startup file: ~/.config/zsh/.zshrc (sources ~/.config/todd/zsh/zshrc).
# Bootstrap-managed settings: ~/.config/todd/zsh/bootstrap.zsh
#
# NOTE: do not source $ZDOTDIR/.zshrc from here. bootstrap.zsh is itself
# sourced from the ZDOTDIR chain (.zshrc -> todd/zsh/zshrc -> bootstrap.zsh),
# so re-sourcing .zshrc recurses until zsh hits its recursion limit.
alias noav="sudo watch -n 3 'pkill -9 CybereasonAv CybereasonActiveConsole CybereasonSensor'"
export PUSHER_GATEWAY_CLIENT_API_KEY="sadoifkjalksdjfklsdalkfjklasdjfkljaklsdfkjlsladjflkjasldfjlasdfsadfsdasadfert435twrh56e7856trhy4567y45rty"
export PUSHER_GATEWAY_CLIENT_WEBHOOK_URL="https://al-pusher-gateway-dev-ap-tokyo-1.cybereason.net/v1/events"
export PUSHER_GATEWAY_CLIENT_CUSTOMER_ID="alertlogic-log-testing"

for key in ~/.ssh/id_ed25519_toddaornett ~/.ssh/id_ed25519_levelblue; do
	[[ -f "$key" ]] && ssh-add --apple-use-keychain "$key" 2>/dev/null
done

# mise aliases
alias msc='mise start:ci'
alias msi='mise start'
alias mss='mise service:status'
alias mst='mise stop'
path+=/Users/todd.ornett/.config/doom-emacs/bin

# GCC runtime libs for libgccjit native compilation (bootstrap)
export LIBRARY_PATH="/opt/homebrew/lib/gcc/current/gcc/aarch64-apple-darwin25/16:/opt/homebrew/lib/gcc/current${LIBRARY_PATH:+:$LIBRARY_PATH}"

# Homebrew/macOS build flags (bootstrap)
if [[ "$(uname -s)" == Darwin ]]; then
  [[ -z "$SDKROOT" ]] && export SDKROOT="$(xcrun --sdk macosx --show-sdk-path 2>/dev/null)"
  if [[ -n "$SDKROOT" ]]; then
    [[ " $CFLAGS " != *" -isysroot "* ]] && export CFLAGS="-isysroot $SDKROOT"
    [[ " $LDFLAGS " != *" -isysroot "* ]] && export LDFLAGS="${LDFLAGS:+$LDFLAGS }-isysroot $SDKROOT"
  fi
  [[ " $CPPFLAGS " != *" -I/opt/homebrew/include "* ]] &&     export CPPFLAGS="${CPPFLAGS:+$CPPFLAGS }-I/opt/homebrew/include"
  [[ " $LDFLAGS " != *" -L/opt/homebrew/lib "* ]] &&     export LDFLAGS="${LDFLAGS:+$LDFLAGS }-L/opt/homebrew/lib"
  [[ ":$PKG_CONFIG_PATH:" != *":/opt/homebrew/opt/boost/lib/pkgconfig:"* ]] &&     export PKG_CONFIG_PATH="/opt/homebrew/opt/boost/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
fi

# mise version manager (bootstrap)
if command -v mise >/dev/null 2>&1; then
  eval "$(mise activate zsh)"
fi
export CARGO_NET_GIT_FETCH_WITH_CLI=true
export DOCKER_CONTEXT=colima
