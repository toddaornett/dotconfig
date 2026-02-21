#!/usr/bin/env bash

set -euo pipefail

echo "🧠 Bootstrapping..."

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
brew bundle --file="$PWD/Brewfile"

#################################
# Clone Doom Emacs
#################################
DOOM_DIR="$HOME/.config/emacs/doom-emacs"

if [ ! -d "$DOOM_DIR" ]; then
  echo "😈 Cloning Doom Emacs..."
  git clone --depth 1 https://github.com/doomemacs/doomemacs "$DOOM_DIR"
fi

#################################
# Install Nerd Font (JetBrainsMono)
#################################
echo "🔤 Installing JetBrainsMono Nerd Font..."
FONT_DEST_DIR="$HOME/Library/Fonts"
set -- "$FONT_DEST_DIR"/JetBrainsMono*
if [ -e "$1" ]; then
  echo "✅ JetBrainsMono Nerd Font previously installed"
else
  TMP_DIR="$(mktemp -d)"

  curl -L -o "$TMP_DIR/font.zip" \
    https://github.com/ryanoasis/nerd-fonts/releases/latest/download/JetBrainsMono.zip
  unzip -q "$TMP_DIR/font.zip" -d "$TMP_DIR/fonts"

  mkdir -p "$FONT_DEST_DIR"
  cp -n "$TMP_DIR/fonts"/*.ttf "$FONT_DEST_DIR" || true
  
  rm -rf "$TMP_DIR"
  echo "✅ JetBrainsMono Nerd Font installed"
fi

#################################
# Optional language runtimes
#################################
echo 'Install common language servers (node, python tools)? [y/N] '
read answer
if [[ "$answer" =~ ^[Yy]$ ]]; then
  echo "🌐 Installing LSP helpers..."
  npm install -g pyright typescript typescript-language-server bash-language-server
  pip3 install --user black isort flake8
fi

#################################
# Doom install + sync
#################################
echo "🔥 Installing Doom packages..."
if ! grep -Fqs "$DOOM_DIR/bin" "$HOME/.zshenv" 2>/dev/null; then
  echo "path+=$DOOM_DIR/bin" >>"$HOME/.zshenv"
fi
"$DOOM_DIR/bin/doom" install
"$DOOM_DIR/bin/doom" sync

#################################
# Final message
#################################
echo "✅ Done!"
