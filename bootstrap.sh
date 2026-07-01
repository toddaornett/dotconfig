#!/usr/bin/env bash
set -euo pipefail

ZSHENV="$HOME/.zshenv"
ZSHRC="$HOME/.zshrc"
BUILD_FLAGS_MARKER="Homebrew/macOS build flags (bootstrap)"

#################################
# Detect Homebrew prefix (ARM / Intel safe)
#################################
BREW_PREFIX="$(brew --prefix)"
echo "🍺 Homebrew prefix: $BREW_PREFIX"

#################################
# Setup and load zshenv
#################################
if [ ! -f "$ZSHENV" ]; then
  echo "typeset -U path PATH" >>$ZSHENV
fi

echo "🛣️  Ensuring Homebrew is first in PATH and configuring Homebrew..."
if ! grep -Fqs "$BREW_PREFIX/bin" "$ZSHENV" 2>/dev/null; then
  echo "export PATH=\"$BREW_PREFIX/bin:\$PATH\"" >>"$ZSHENV"
fi

lines=(
  'export XDG_CONFIG_HOME="$HOME/.config"'
  'export XDG_CACHE_HOME="$HOME/.cache"'
  'export HOMEBREW_CACHE="$XDG_CACHE_HOME/Homebrew"'
)

for line in "${lines[@]}"; do
  grep -Fqx "$line" "$ZSHENV" || echo "$line" >>"$ZSHENV"
done

source "$ZSHENV"

export_macos_build_env() {
  [[ "$(uname -s)" != Darwin ]] && return 0

  local sdk
  sdk="$(xcrun --sdk macosx --show-sdk-path 2>/dev/null || true)"
  if [ -n "$sdk" ]; then
    export SDKROOT="$sdk"
  fi

  export CC="${CC:-$(xcrun --find cc 2>/dev/null || echo clang)}"
  export CXX="${CXX:-$(xcrun --find c++ 2>/dev/null || echo clang++)}"
  export CPPFLAGS="${CPPFLAGS:+$CPPFLAGS }-I${BREW_PREFIX:-$(brew --prefix)}/include"
  export LDFLAGS="${LDFLAGS:+$LDFLAGS }-L${BREW_PREFIX:-$(brew --prefix)}/lib"
  export PKG_CONFIG_PATH="${BREW_PREFIX:-$(brew --prefix)}/opt/boost/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"

  # Deduplicate LIBRARY_PATH to avoid duplicate -rpath warnings from ld
  if [ -n "${LIBRARY_PATH:-}" ]; then
    local deduped
    deduped="$(echo "$LIBRARY_PATH" | tr ':' '\n' | awk '!seen[$0]++' | tr '\n' ':' | sed 's/:$//')"
    export LIBRARY_PATH="$deduped"
  fi
}

verify_macos_compiler() {
  [[ "$(uname -s)" != Darwin ]] && return 0

  if ! xcode-select -p >/dev/null 2>&1; then
    echo "❌ Xcode Command Line Tools are not installed."
    echo "   Run: xcode-select --install"
    note_shell_init_for_builds
    return 1
  fi

  local sdk="${SDKROOT:-$(xcrun --sdk macosx --show-sdk-path 2>/dev/null || true)}"
  if [ -z "$sdk" ] || [ ! -f "$sdk/usr/include/stdlib.h" ]; then
    echo "❌ macOS SDK headers not found (stdlib.h missing)."
    echo "   SDK path: ${sdk:-<none>}"
    note_shell_init_for_builds
    return 1
  fi

  return 0
}

note_shell_init_for_builds() {
  echo "   Update ~/.zshrc with SDKROOT and Homebrew include/lib paths,"
  echo "   then restart your shell: source ~/.zshrc"
}

ensure_macos_build_env_in_shell() {
  echo "🛠️ Verify build env vars ..."
  [[ "$(uname -s)" != Darwin ]] && return 0

  if grep -Fq "$BUILD_FLAGS_MARKER" "$ZSHRC" 2>/dev/null ||
    grep -q 'Homebrew build flags' "$ZSHRC" 2>/dev/null; then
    return 0
  fi

  cat >>"$ZSHRC" <<EOF

# $BUILD_FLAGS_MARKER
if [[ "\$(uname -s)" == Darwin ]]; then
  [[ -z "\$SDKROOT" ]] && export SDKROOT="\$(xcrun --sdk macosx --show-sdk-path 2>/dev/null)"
  [[ " \$CPPFLAGS " != *" -I${BREW_PREFIX}/include "* ]] && \
    export CPPFLAGS="\${CPPFLAGS:+\$CPPFLAGS }-I${BREW_PREFIX}/include"
  [[ " \$LDFLAGS " != *" -L${BREW_PREFIX}/lib "* ]] && \
    export LDFLAGS="\${LDFLAGS:+\$LDFLAGS }-L${BREW_PREFIX}/lib"
  [[ ":\$PKG_CONFIG_PATH:" != *":${BREW_PREFIX}/opt/boost/lib/pkgconfig:"* ]] && \
    export PKG_CONFIG_PATH="${BREW_PREFIX}/opt/boost/lib/pkgconfig\${PKG_CONFIG_PATH:+:\$PKG_CONFIG_PATH}"
fi
EOF
  echo "  Added macOS build env vars to $ZSHRC"
  note_shell_init_for_builds
}

# Ensure gcc and libgccjit are built from source so libemutls_w.a is present.
# Homebrew bottles for Apple Silicon omit this runtime lib, breaking native comp.
# Only rebuilds if libemutls_w.a is genuinely missing to avoid wasting time on
# repeat runs.
ensure_gcc_emutls() {
  [[ "$(uname -s)" != Darwin ]] && return 0

  local gcc_ver
  gcc_ver="$(brew list --versions gcc 2>/dev/null | awk '{print $2}' | cut -d. -f1)"
  if [ -z "$gcc_ver" ]; then
    echo "⚠️  gcc not yet installed — skipping emutls check (will recheck after brew bundle)"
    return 0
  fi

  local gcc_arch
  gcc_arch="$(/opt/homebrew/bin/gcc-${gcc_ver} -dumpmachine 2>/dev/null || true)"
  if [ -z "$gcc_arch" ]; then
    echo "⚠️  Could not determine gcc target arch — skipping emutls check"
    return 0
  fi

  local emutls_path="/opt/homebrew/lib/gcc/current/gcc/${gcc_arch}/${gcc_ver}/libemutls_w.a"

  if [ -f "$emutls_path" ]; then
    echo "✅ libemutls_w.a already present — skipping gcc source build"
    return 0
  fi

  echo "⚠️  libemutls_w.a missing (Homebrew bottle omits it)."
  echo "🔨 Building gcc + libgccjit from source (~30-60 min)..."
  brew reinstall --build-from-source gcc || true
  brew reinstall --build-from-source libgccjit || true
  brew link --overwrite libgccjit || true

  if [ -f "$emutls_path" ]; then
    echo "✅ libemutls_w.a now present after source build"
  else
    echo "❌ libemutls_w.a still missing after source build."
    echo "   Native compilation will likely fail."
    echo "   Check: find /opt/homebrew/Cellar/gcc -name 'libemutls_w.a'"
  fi
}

echo "🧠 Bootstrapping system..."

#################################
# Set defaults on macOS
#################################
if command -v defaults >/dev/null 2>&1; then
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
# Install Brewfile deps
#################################
echo "📌 Ensuring d12frosted/emacs-plus tap..."
brew trust d12frosted/emacs-plus
brew trust nikitabobko/tap
brew tap d12frosted/emacs-plus 2>/dev/null || true

echo "🔄 Updating Homebrew..."
brew update

echo "📦 Installing Homebrew packages..."
# Allow emacs-plus link conflict — we force-link it immediately after
brew bundle --file="./Brewfile" || true

# Force-link emacs-plus@30, overwriting stale symlinks from /Applications/Emacs.app
echo "🔗 Force-linking emacs-plus@30..."
brew link --overwrite emacs-plus@30 || true

#################################
# Ensure gcc has libemutls_w.a
# (Homebrew bottles omit it; source build required for native comp)
#################################
ensure_gcc_emutls

#################################
# Set globals for git
#################################
echo "🔀 Configure git ..."
git config --global status.submoduleSummary true
GITIGNORE_FILEPATH="$HOME/.gitignore_global"
git config --global core.excludesfile "$GITIGNORE_FILEPATH"
if [ ! -f "$GITIGNORE_FILEPATH" ]; then
  echo "  create $GITIGNORE_FILEPATH"
  cat >>"$GITIGNORE_FILEPATH" <<'EOF'
# macOS
.DS_Store

# Emacs
*~
#*#
.elc

# Vim
*.swp

# Local env files
.env
.env.local
EOF
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

# emacs-plus links against keg-only libs that brew cleanup may remove.
ensure_emacs_runtime_deps() {
  [[ "$(uname -s)" != Darwin ]] && return 0

  echo "🔗 Ensuring Emacs runtime libraries..."
  local dep missing=()
  for dep in jpeg zlib tree-sitter@0.25; do
    if ! brew list "$dep" &>/dev/null; then
      missing+=("$dep")
    fi
  done

  if [ "${#missing[@]}" -gt 0 ]; then
    echo "  Installing missing keg-only deps: ${missing[*]}"
    brew install "${missing[@]}"
  fi

  if ! emacs --batch --eval '(message "ok")' &>/dev/null; then
    echo "❌ Emacs failed to launch (missing dylibs?)."
    echo "   Try: brew reinstall emacs-plus@30"
    exit 1
  fi
  echo "✅ Emacs runtime libraries OK"
}

ensure_emacs_runtime_deps

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

FONT_DIR="$HOME/Library/Fonts"
SYMBOLA_PATH="$FONT_DIR/Symbola.ttf"

mkdir -p "$FONT_DIR"

if [ ! -f "$SYMBOLA_PATH" ]; then
  echo "⬇️  Downloading Symbola.ttf..."
  # Try primary source first, fall back to mirror
  curl -fsSL "https://github.com/ChiefMikeK/ttf-symbola/raw/master/Symbola-13.ttf" \
    -o "$SYMBOLA_PATH" 2>/dev/null ||
    curl -fsSL "https://raw.githubusercontent.com/ChiefMikeK/ttf-symbola/master/Symbola-13.ttf" \
      -o "$SYMBOLA_PATH" 2>/dev/null ||
    curl -fsSL "https://dn-works.com/wp-content/uploads/2020/UFAS-Fonts/Symbola.ttf" \
      -o "$SYMBOLA_PATH" 2>/dev/null || true

  if [ -f "$SYMBOLA_PATH" ] && [ -s "$SYMBOLA_PATH" ]; then
    echo "✅ Symbola font installed (logout may be required to activate)"
  else
    echo "⚠️  Symbola download failed — install manually from https://dn-works.com/ufas/"
    rm -f "$SYMBOLA_PATH"
  fi
else
  echo "✅ Symbola font already installed"
fi

#################################
# Clone Doom Emacs
#################################
DOOM_DIR="$HOME/.config/doom-emacs"

if [ ! -d "$DOOM_DIR" ]; then
  echo "😈 Cloning Doom Emacs..."
  git clone --depth 1 https://github.com/doomemacs/doomemacs "$DOOM_DIR"
elif [ ! -f "$DOOM_DIR/early-init.el" ]; then
  echo "⚠️  Doom install appears incomplete (missing early-init.el) — re-cloning..."
  rm -rf "$DOOM_DIR"
  git clone --depth 1 https://github.com/doomemacs/doomemacs "$DOOM_DIR"
else
  echo "😈 Doom Emacs already present"
fi

#################################
# Clone doom-meow
#################################
if [ ! -d "$DOOM_DIR/modules/editor/meow" ]; then
  echo "🐱 Cloning doom-meow module..."
  mkdir -p "$DOOM_DIR/modules/editor"
  git clone https://github.com/meow-edit/doom-meow "$DOOM_DIR/modules/editor/meow"
fi

#################################
# Doom install + sync
#################################
echo "🔥 Installing Doom packages..."

DOOM_BIN="$DOOM_DIR/bin"

if ! grep -Fqs "$DOOM_BIN" "$ZSHRC" 2>/dev/null; then
  echo "path+=$DOOM_BIN" >>"$ZSHRC"
fi

# Set LIBRARY_PATH so libgccjit's embedded gcc driver can find libemutls_w.a.
# The file lives in the arch/version subdir, not the top-level current/ dir.
GCC_VER="$(brew list --versions gcc | awk '{print $2}' | cut -d. -f1)"
GCC_ARCH="$(/opt/homebrew/bin/gcc-${GCC_VER} -dumpmachine)"
GCC_LIB_BASE="/opt/homebrew/lib/gcc/current"
GCC_LIB_FULL="${GCC_LIB_BASE}/gcc/${GCC_ARCH}/${GCC_VER}"

if [ -d "$GCC_LIB_FULL" ]; then
  LIBRARY_PATH="${LIBRARY_PATH:-}"
  export LIBRARY_PATH="${GCC_LIB_FULL}:${GCC_LIB_BASE}${LIBRARY_PATH:+:$LIBRARY_PATH}"
  # Persist both paths to .zshrc so interactive Emacs also gets them
  if ! grep -Fq "gcc/current/gcc/${GCC_ARCH}" "$ZSHRC" 2>/dev/null; then
    cat >>"$ZSHRC" <<EOF

# GCC runtime libs for libgccjit native compilation (bootstrap)
export LIBRARY_PATH="${GCC_LIB_FULL}:${GCC_LIB_BASE}\${LIBRARY_PATH:+:\$LIBRARY_PATH}"
EOF
  fi
  echo "✅ LIBRARY_PATH set for GCC ${GCC_VER} (${GCC_ARCH})"
else
  echo "⚠️  GCC lib dir not found: ${GCC_LIB_FULL}"
  echo "   Native compilation may fail. Try: brew reinstall --build-from-source gcc"
fi

"$DOOM_BIN/doom" install
"$DOOM_BIN/doom" sync

#################################
# Build emacs-libvterm module
#################################
ensure_macos_build_env_in_shell

EMACS_VER="$(emacs --batch --eval '(princ emacs-version)' 2>/dev/null)"
VTERM_BUILD_DIR="${DOOM_DIR}/.local/straight/build-${EMACS_VER}/vterm"
VTERM_REPO_DIR="${DOOM_DIR}/.local/straight/repos/emacs-libvterm"

# If build dir doesn't exist yet, run doom sync to populate it then try again
if [ ! -d "$VTERM_BUILD_DIR" ] && [ -d "$VTERM_REPO_DIR" ]; then
  echo "🔄 vterm repo present but not built — running doom sync to populate build dir..."
  "$DOOM_BIN/doom" sync
fi

if [ -d "$VTERM_BUILD_DIR" ]; then
  if [ -f "$VTERM_BUILD_DIR/vterm-module.so" ]; then
    echo "✅ vterm module already built"
  else
    echo "🛠️ Building vterm native module..."
    vterm_build_ok=0
    if verify_macos_compiler; then
      (
        export_macos_build_env
        cd "$VTERM_BUILD_DIR"
        if [ -f CMakeCache.txt ]; then
          cmake --build . --clean-first
        else
          cmake .
          make
        fi
      ) && vterm_build_ok=1
    fi

    if [ "$vterm_build_ok" -eq 1 ]; then
      echo "✅ vterm module built successfully"
    else
      echo "❌ vterm module build failed"
      echo "   If you saw 'stdlib.h: file not found', install Xcode CLT: xcode-select --install"
      note_shell_init_for_builds
      echo "   Then re-run bootstrap or build manually:"
      echo "     cd \"$VTERM_BUILD_DIR\" && cmake --build . --clean-first"
    fi
  fi
elif [ -d "$VTERM_REPO_DIR" ]; then
  echo "⚠️  vterm build dir still not found after sync — re-run bootstrap once more"
else
  echo "ℹ️  vterm not configured in Doom — skipping build"
fi

#################################
# Install chemacs2
#################################
CHEMACS2_PROFILES_FILE="$HOME/.emacs-profiles.el"
CHEMACS2_DIR="$HOME/.config/emacs"

write_chemacs2_profiles() {
  cat >"$CHEMACS2_PROFILES_FILE" <<'EOF'
(("default" .  ((user-emacs-directory . "~/.config/doom-emacs")))
 ("scratch" . ((user-emacs-directory . "~/.config/scratch-emacs"))))
EOF
}

if [ ! -f "$CHEMACS2_PROFILES_FILE" ]; then
  if [ -d "$CHEMACS2_DIR/.git" ]; then
    echo "✅ chemacs2 repo already present at $CHEMACS2_DIR"
    write_chemacs2_profiles
    echo "✅ Wrote $CHEMACS2_PROFILES_FILE"
  elif [ -e "$CHEMACS2_DIR" ]; then
    echo "⚠️  Cannot install chemacs2: $CHEMACS2_DIR already exists and is not a git repo."
    echo "   Move or rename that directory, then re-run bootstrap."
    echo "   Or create $CHEMACS2_PROFILES_FILE manually if chemacs2 is installed elsewhere."
  else
    echo "🦬 λ Installing chemacs2 with Doom Emacs as default"
    if git clone https://github.com/plexus/chemacs2.git "$CHEMACS2_DIR"; then
      write_chemacs2_profiles
      echo "✅ Wrote $CHEMACS2_PROFILES_FILE"
    else
      echo "❌ chemacs2 clone failed — see errors above"
    fi
  fi
else
  echo "✅ chemacs2 profiles already configured"
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
if [ -t 0 ]; then
  read -rp "🌐 Install common language servers (node, python tools)? [y/N] " answer
else
  answer="n"
fi
if [[ "$answer" =~ ^[Yy]$ ]]; then
  echo "📡 Installing LSP helpers..."

  #################################
  # Node-based language servers
  #################################
  npm install -g \
    pyright \
    typescript \
    typescript-language-server \
    bash-language-server

  #################################
  # Python tooling via pipx
  #################################
  if ! command -v pipx >/dev/null 2>&1; then
    echo "🐍 Installing pipx..."
    brew install pipx
    pipx ensurepath
  fi

  export PATH="$HOME/.local/bin:$PATH"

  install_pipx_tool() {
    local tool="$1"

    if pipx list 2>/dev/null | grep -q "$tool"; then
      echo "✅ $tool already installed"
    else
      echo "📦 Installing $tool via pipx..."
      pipx install "$tool" || {
        echo "⚠️  Failed installing $tool with pipx"
      }
    fi
  }

  install_pipx_tool black
  install_pipx_tool isort
  install_pipx_tool flake8
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
# Install Krew if missing
#################################
if ! command -v kubectl-krew >/dev/null 2>&1; then
  echo "☸️ Installing Krew..."
  export KREW_ROOT="$HOME/.krew"
  (
    set -x
    cd "$(mktemp -d)" &&
      OS="$(uname | tr '[:upper:]' '[:lower:]')" &&
      ARCH="$(uname -m | sed -e 's/x86_64/amd64/' -e 's/\(arm\)\(64\)\?.*/\1\2/' -e 's/aarch64$/arm64/')" &&
      KREW="krew-${OS}_${ARCH}" &&
      curl -fsSLO "https://github.com/kubernetes-sigs/krew/releases/latest/download/${KREW}.tar.gz" &&
      tar zxvf "${KREW}.tar.gz"
    ./"${KREW}" install krew
  )
  echo "export KREW_ROOT=$HOME/.krew" >>"$ZSHENV"
  echo "path+=\"${KREW_ROOT}/bin\"" >>"$ZSHENV"
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
# Deal with Microsoft Teams cleanup to prevent future crashes
#################################
if [ -d "/Applications/Microsoft Teams.app" ]; then
  echo "Sucks to be you dealing with M$ Teams, so we try to prevent future crashes"
  if [ ! -f ~/.zlogin ]; then
    touch ~/.zlogin
    chmod +x ~/.zlogin
  fi
  if ! grep "microsoft.teams" ~/.zlogin; then
    echo "rm -rf ~/Library/Group\ Containers/UBF8T346G9.com.microsoft.teams" >>~/.zlogin
    echo "sudo rm -rf ~/Library/Containers/com.microsoft.teams2" >>~/.zlogin
  fi
fi

#################################
# Configure Boost / native build env
#################################
echo "🚀 Checking Boost cpp library configuration ..."
ensure_macos_build_env_in_shell

#################################
# Configure optional LLM and give hint if not enabled
#################################
if [ -z "${LLM_PROVIDERS:-}" ]; then
  echo "🤖 AI with local LLMs is not enabled. Set LLM_PROVIDERS to 'ollama' (only one supported now) and re-run to enable it..."
else
  if [[ "$LLM_PROVIDERS" == "ollama" ]] && ! command -v "$LLM_PROVIDERS" >/dev/null 2>&1; then
    echo "🦙 Installing Ollama ..."
    curl -fsSL https://ollama.com/install.sh | sh
    echo "🦙 Pulling some language models ..."
    ollama pull llama3
    ollama pull qwen3.6
  fi
  echo "🦙 Verifying Ollama network exposure..."
  ifconfig | grep 192 | cut -d ' ' -f 2 | sed -e 's#^#curl -s http://#' -e s'#$#:11434#' | bash
  echo ""
fi

#################################
# Final message
#################################
echo "🎉 Bootstrap complete!"
echo "➡ Restart your shell, then for emacs verification run:"
echo "   which emacs"
echo "   emacs --version"
echo "   (native-comp-available-p) ;; M-: inside emacs"
