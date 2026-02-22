#!/usr/bin/env bash
set -euo pipefail

echo "🧠 Bootstrapping system..."

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
echo "🧹 Pre-clean flaky font casks..."
BROKEN_FONTS=(
  font-fira-code-nerd-font
)

for font in "${BROKEN_FONTS[@]}"; do
  brew uninstall --cask --force "$font" >/dev/null 2>&1 || true
done

echo "📦 Installing Homebrew packages..."
brew bundle --file="$PWD/Brewfile"

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
# Doom install + sync
#################################
echo "🔥 Installing Doom packages..."

ZSHENV="$HOME/.zshenv"
DOOM_BIN="$DOOM_DIR/bin"

if ! grep -Fqs "$DOOM_BIN" "$ZSHENV" 2>/dev/null; then
  echo "path+=$DOOM_BIN" >>"$ZSHENV"
fi

"$DOOM_BIN/doom" install
"$DOOM_BIN/doom" sync

#################################
# Final message
#################################
echo "🎉 Bootstrap complete!"
