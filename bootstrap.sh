#!/usr/bin/env bash
set -euo pipefail
ZSHENV="$HOME/.zshenv"

echo "🧠 Bootstrapping system..."

#################################
# Set defaults on macOS
#################################
if command defaults >/dev/null 2>&1; then
  defaults write com.apple.dock expose-group-apps -bool true && killall Dock
  defaults write com.apple.spaces spans-displays -bool true && killall SystemUIServer
  defaults write com.apple.WindowManager GloballyEnabled -bool false
fi

#################################
# Install Homebrew if missing
#################################
if ! command -v brew >/dev/null 2>&1; then
  echo "🍺 Installing Homebrew..."
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

#################################
# Install Brewfile deps
#################################
echo "📦 Installing Homebrew packages..."
brew bundle --file="./Brewfile"

#################################
# Ensure fonts are registered (macOS)
#################################
echo "🔤 Verifying fonts..."
if system_profiler SPFontsDataType | grep -q "Fira Sans"; then
  echo "✔︎ Fira Sans already detected"
elif ls "$HOME/Library/Fonts"/FiraSans*.otf >/dev/null 2>&1; then
  echo "✔︎ Fira Sans files present; if not visible in apps, open Font Book or log out and back in"
else
  echo "⚠️  Fira Sans not installed — installing font..."
  brew reinstall --cask font-fira-sans
fi

#################################
# Clone Doom Emacs
#################################
DOOM_DIR="$HOME/.config/emacs/doom-emacs"

if [ ! -d "$DOOM_DIR" ]; then
  echo "😈 Cloning Doom Emacs..."
  git clone --depth 1 https://github.com/doomemacs/doomemacs "$DOOM_DIR"
else
  echo "😈 Doom Emacs already present"
fi

#################################
# Install JetBrainsMono Nerd Font (manual fallback)
#################################
echo "🔤 Ensuring JetBrainsMono Nerd Font..."

FONT_DEST_DIR="$HOME/Library/Fonts"
if ls "$FONT_DEST_DIR"/JetBrainsMono*NerdFont*.ttf >/dev/null 2>&1; then
  echo "✅ JetBrainsMono Nerd Font already installed"
else
  TMP_DIR="$(mktemp -d)"
  echo "⬇️  Downloading JetBrainsMono Nerd Font..."
  curl -L -o "$TMP_DIR/font.zip" \
    https://github.com/ryanoasis/nerd-fonts/releases/latest/download/JetBrainsMono.zip

  unzip -q "$TMP_DIR/font.zip" -d "$TMP_DIR/fonts"
  mkdir -p "$FONT_DEST_DIR"
  cp "$TMP_DIR/fonts"/*.ttf "$FONT_DEST_DIR"
  rm -rf "$TMP_DIR"

  echo "✅ JetBrainsMono Nerd Font installed"
fi

#################################
# Optional language runtimes
#################################
read -rp "🌐 Install common language servers (node, python tools)? [y/N] " answer
if [[ "$answer" =~ ^[Yy]$ ]]; then
  echo "📡 Installing LSP helpers..."
  npm install -g pyright typescript typescript-language-server bash-language-server
  pip3 install --user black isort flake8
fi

#################################
# Install mise
#################################
if ! command -v mise >/dev/null 2>&1; then
  echo "🛠️ Installing Mise..."
  curl https://mise.run | sh
  if ! grep -Fqs "MISE_TRUSTED_CONFIG_PATHS" "$ZSHENV" 2>/dev/null; then
    # shellcheck disable=SC2016
    echo 'export MISE_TRUSTED_CONFIG_PATHS="$HOME/Projects"' >>"$ZSHENV"
  fi
fi

#################################
# Doom install + sync
#################################
echo "🔥 Installing Doom packages..."

DOOM_BIN="$DOOM_DIR/bin"

if ! grep -Fqs "$DOOM_BIN" "$ZSHENV" 2>/dev/null; then
  echo "path+=$DOOM_BIN" >>"$ZSHENV"
fi

"$DOOM_BIN/doom" install
"$DOOM_BIN/doom" sync

#################################
# Build emacs-libvterm module
#################################
VTERM_DIR="${DOOM_DIR}/.local/straight/repos/emacs-libvterm"

if [ -d "$VTERM_DIR" ]; then
  echo "🛠️  Building vterm native module..."
  (
    unset CC
    unset CXX
    cd "$VTERM_DIR" || true

    if [ -f Makefile ]; then
      make clean || true
      make || true
    else
      mkdir -p build
      cd build || true
      cmake .. || true
      make || true
    fi
  )
  cp ${DOOM_DIR}/.local/straight/repos/emacs-libvterm/vterm-module.so \
    ${DOOM_DIR}/.local/straight/build-*/vterm/
  echo "✅ vterm module build step finished"
fi

#################################
# Docker configuration for Colima 
#################################
echo "️🐳  Configuring Docker..."
if [ ! -f ~/.docker/config.json ]; then
  mkdir -p ~/.docker
  echo "{}" > ~/.docker/config.json
fi
NEW_PATH="$(brew --prefix)/lib/docker/cli-plugins" 
tmp_config=$(mktemp)
trap 'rm -f "$tmp_config"' EXIT
if jq --arg path "$NEW_PATH" '
  .cliPluginsExtraDirs |= (. // []) |
  if (.cliPluginsExtraDirs | index($path) == null)
  then .cliPluginsExtraDirs += [$path]
  else .
  end
' ~/.docker/config.json > "$tmp_config"; then
  mv "$tmp_config" ~/.docker/config.json
  echo "✅ Docker config updated/verified."
else 
  echo "❌ Failed to update Docker config."
fi

#################################
# Final message
#################################
echo "🎉 Bootstrap complete!"
