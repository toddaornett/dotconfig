#!/usr/bin/env bash
set -euo pipefail

ZSHENV="$HOME/.zshenv"
if [ ! -f "$ZSHENV" ]; then
  echo "typeset -U path PATH" >>$ZSHENV
fi

echo "🧠 Bootstrapping system..."

#################################
# Set defaults on macOS
#################################
if command defaults >/dev/null 2>&1; then
  defaults write com.apple.dock expose-group-apps -bool true && killall Dock || true
  defaults write com.apple.spaces spans-displays -bool true && killall SystemUIServer || true
  defaults write com.apple.WindowManager GloballyEnabled -bool false || true
  defaults write -g NSWindowShouldDragOnGesture -bool true || true
  defaults write -g NSAutomaticWindowAnimationsEnabled -bool false || true
fi

#################################
# Install Homebrew if missing
#################################
if ! command -v brew >/dev/null 2>&1; then
  echo "🍺 Installing Homebrew..."
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

#################################
# Detect Homebrew prefix (ARM / Intel safe)
#################################
BREW_PREFIX="$(brew --prefix)"
echo "🍺 Homebrew prefix: $BREW_PREFIX"

#################################
# Install Brewfile deps
#################################
# Ensure emacs-plus tap is available (required for emacs-plus@30)
echo "📌 Ensuring d12frosted/emacs-plus tap..."
brew tap d12frosted/emacs-plus 2>/dev/null || true
echo "🔄 Updating Homebrew..."
brew update
echo "📦 Installing Homebrew packages..."
brew bundle --file="./Brewfile"

#################################
# Ensure Homebrew bin is first in PATH
#################################
echo "🛣️  Ensuring Homebrew is first in PATH..."
if ! grep -Fqs "$BREW_PREFIX/bin" "$ZSHENV" 2>/dev/null; then
  echo "export PATH=\"$BREW_PREFIX/bin:\$PATH\"" >>"$ZSHENV"
fi

#################################
# Verify correct Emacs is used
#################################
echo "🧪 Verifying Emacs path..."
if ! command -v emacs >/dev/null 2>&1; then
  echo "❌ emacs not found in PATH"
  exit 1
fi

EMACS_BIN="$(command -v emacs)"
echo "➡ using emacs at: $EMACS_BIN"

if [[ "$EMACS_BIN" == "/usr/bin/emacs" ]]; then
  echo "❌ Wrong Emacs (system stub). Homebrew Emacs is not first in PATH."
  echo "   Check your shell init files (.zshenv, .zprofile, .zshrc)."
  exit 1
fi

# Link Emacs.app into /Applications if missing (use full formula path for tap)
EMACS_PREFIX="$(brew --prefix d12frosted/emacs-plus/emacs-plus@30 2>/dev/null || brew --prefix emacs-plus@30 2>/dev/null)"
EMACS_APP_SRC="${EMACS_PREFIX}/Emacs.app"
EMACS_APP_DST="/Applications/Emacs.app"

if [ -n "$EMACS_PREFIX" ] && [ -d "$EMACS_APP_SRC" ] && [ ! -e "$EMACS_APP_DST" ]; then
  echo "📎 Linking Emacs.app into /Applications..."
  ln -s "$EMACS_APP_SRC" "$EMACS_APP_DST"
fi

#################################
# Ensure fonts are registered (macOS)
#################################
echo "🔤 Verifying fonts..."
if system_profiler SPFontsDataType | grep -q "Fira Sans"; then
  echo "✔︎ Fira Sans already detected"
elif ls "$HOME/Library/Fonts"/FiraSans*.otf >/dev/null 2>&1; then
  echo "✔︎ Fira Sans files present; if not visible in apps, open Font Book or log out and back in"
else
  echo "⚠️  Fira Sans not installed — reinstalling..."
  brew reinstall --cask font-fira-sans
fi

#################################
# Install Symbola font for Doom Emacs
# (special step since removed from Homebrew)
#################################
echo "🔤 Ensuring Symbola font is installed (for Doom doctor)..."

SYMBOLA_URL="https://dn-works.com/wp-content/uploads/2020/UFAS-Fonts/Symbola.ttf"
FONT_DIR="$HOME/Library/Fonts"
SYMBOLA_PATH="$FONT_DIR/Symbola.ttf"

mkdir -p "$FONT_DIR"

if [ ! -f "$SYMBOLA_PATH" ]; then
  echo "⬇️  Downloading Symbola.ttf..."
  curl -L "$SYMBOLA_URL" -o "$SYMBOLA_PATH"
  echo "✅ Symbola font installed (logout required to activate)"
else
  echo "✅ Symbola font already installed"
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
    echo 'export MISE_TRUSTED_CONFIG_PATHS="${HOME}/dev:${HOME}/Projects"' >>"$ZSHENV"
  fi
fi

#################################
# Install krew a package manager for kubectl
#################################
if ! command -v krew >/dev/null 2>&1; then
  echo "☸️️ Installing Krew..."
  (
    set -x; cd "$(mktemp -d)" &&
    OS="$(uname | tr '[:upper:]' '[:lower:]')" &&
    ARCH="$(uname -m | sed -e 's/x86_64/amd64/' -e 's/\(arm\)\(64\)\?.*/\1\2/' -e 's/aarch64$/arm64/')" &&
    KREW="krew-${OS}_${ARCH}" &&
    curl -fsSLO "https://github.com/kubernetes-sigs/krew/releases/latest/download/${KREW}.tar.gz" &&
    tar zxvf "${KREW}.tar.gz" &&
    ./"${KREW}" install krew
  )
  echo "export KREW_ROOT=$HOME/.krew" >>"$ZSHENV"
  echo "path+=\"${KREW_ROOT}/bin\"" >>"$ZSHENV"
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
    ${DOOM_DIR}/.local/straight/build-*/vterm/ || true

  echo "✅ vterm module build step finished"
fi

#################################
# Docker configuration for Colima
#################################
echo "🐳 Configuring Docker..."

if [ ! -f ~/.docker/config.json ]; then
  mkdir -p ~/.docker
  echo "{}" >~/.docker/config.json
fi

NEW_PATH="$BREW_PREFIX/lib/docker/cli-plugins"

tmp_config=$(mktemp)
trap 'rm -f "$tmp_config"' EXIT

if jq --arg path "$NEW_PATH" '
  .cliPluginsExtraDirs |= (. // []) |
  if (.cliPluginsExtraDirs | index($path) == null)
  then .cliPluginsExtraDirs += [$path]
  else .
  end
' ~/.docker/config.json >"$tmp_config"; then
  mv "$tmp_config" ~/.docker/config.json
  echo "✅ Docker config updated/verified."
else
  echo "❌ Failed to update Docker config."
fi

#################################
# Final message
#################################
echo "🎉 Bootstrap complete!"
echo "➡ Restart your shell, then for emacs verification run:"
echo "   which emacs"
echo "   emacs --version"
echo "   (native-comp-available-p) ;; M-: inside emacs"
