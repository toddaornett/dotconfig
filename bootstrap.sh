#!/usr/bin/env bash
set -euo pipefail

ZSHENV="$HOME/.zshenv"
ZSHRC="$HOME/.config/todd/zsh/zshrc"
ZSH_BOOTSTRAP="$HOME/.config/todd/zsh/bootstrap.zsh"
HOME_ZSHRC="$HOME/.zshrc"
ZDOTDIR_DIR="$HOME/.config/zsh"
BUILD_FLAGS_MARKER="Homebrew/macOS build flags (bootstrap)"
MISE_MARKER="mise version manager (bootstrap)"

note_shell_init_for_builds() {
  echo "   Update $ZSH_BOOTSTRAP with SDKROOT and Homebrew include/lib paths,"
  echo "   then restart your shell: source ~/.config/zsh/.zshrc"
}

ensure_zdotdir_in_zshenv() {
  local line='export ZDOTDIR=~/.config/zsh'
  grep -Fqx "$line" "$ZSHENV" 2>/dev/null || echo "$line" >>"$ZSHENV"
}

# After ZDOTDIR is set, zsh reads $ZDOTDIR/.zsh{env,profile,rc,login} and ignores
# the same files in $HOME. Nested shells already have ZDOTDIR exported, so they
# skip ~/.zshenv unless $ZDOTDIR/.zshenv bridges back to it.
ensure_zdotdir_file_sources() {
  local dest="$1"
  local src="$2"
  local comment="$3"
  local src_home="${src/#$HOME/\$HOME}"

  [[ -f "$src" ]] || return 0
  mkdir -p "$(dirname "$dest")"

  if [[ -f "$dest" ]] && { grep -Fq "$src" "$dest" || grep -Fq "$src_home" "$dest"; }; then
    return 0
  fi

  local line="[[ -f \"$src_home\" ]] && source \"$src_home\""
  if [[ ! -f "$dest" ]]; then
    {
      echo "# $comment"
      echo "# Managed by ~/.config/bootstrap.sh — do not remove the source line."
      echo "$line"
    } >"$dest"
    echo "  Created $dest → $src"
  else
    {
      echo ""
      echo "# $comment"
      echo "$line"
    } >>"$dest"
    echo "  Added source of $src to $dest"
  fi
}

ensure_zdotdir_home_zshrc_bridge() {
  local zdot_rc="$ZDOTDIR_DIR/.zshrc"
  local marker='_TODD_SOURCING_HOME_ZSHRC'
  mkdir -p "$ZDOTDIR_DIR"
  [[ -f "$zdot_rc" ]] || return 0
  grep -Fq "$marker" "$zdot_rc" 2>/dev/null && return 0
  cat >>"$zdot_rc" <<'EOF'

# ~/.zshrc is skipped when ZDOTDIR is set. Load it for extra home-level aliases.
# The guard prevents recursion when ~/.zshrc sources this file.
if [[ -z ${_TODD_SOURCING_HOME_ZSHRC:-} && -o interactive && -f "$HOME/.zshrc" ]]; then
  _TODD_SOURCING_HOME_ZSHRC=1
  source "$HOME/.zshrc"
  unset _TODD_SOURCING_HOME_ZSHRC
fi
EOF
  echo "  Added ~/.zshrc bridge to $zdot_rc"
}

ensure_home_zshrc_sources_zdotdir() {
  local marker='_TODD_SOURCING_HOME_ZSHRC'
  touch "$HOME_ZSHRC"
  grep -Fq "$marker" "$HOME_ZSHRC" 2>/dev/null && return 0
  local tmp
  tmp="$(mktemp)"
  cat >"$tmp" <<'EOF'
# Shell config lives under ~/.config/todd/zsh/ (ZDOTDIR=~/.config/zsh in ~/.zshenv).
# Interactive startup file: ~/.config/zsh/.zshrc (sources ~/.config/todd/zsh/zshrc).
# Bootstrap-managed settings: ~/.config/todd/zsh/bootstrap.zsh
#
# New shells with ZDOTDIR set read ~/.config/zsh/.zshrc, not this file.
# Source that file so `source ~/.zshrc` and ZDOTDIR-unset shells still load
# aliases*.zsh. The guard in ~/.config/zsh/.zshrc prevents recursion.
if [[ -z ${_TODD_SOURCING_HOME_ZSHRC:-} && -f "${ZDOTDIR:-$HOME/.config/zsh}/.zshrc" ]]; then
  source "${ZDOTDIR:-$HOME/.config/zsh}/.zshrc"
fi
EOF
  cat "$HOME_ZSHRC" >>"$tmp"
  mv "$tmp" "$HOME_ZSHRC"
  echo "  Added ZDOTDIR .zshrc source to $HOME_ZSHRC"
}

ensure_zdotdir_startup_files() {
  echo "🐚 Ensuring ZDOTDIR ($ZDOTDIR_DIR) sources your real zsh config..."
  mkdir -p "$ZDOTDIR_DIR"
  ensure_zdotdir_file_sources \
    "$ZDOTDIR_DIR/.zshrc" \
    "$ZSHRC" \
    "ZDOTDIR entry point. Real interactive config: ~/.config/todd/zsh/zshrc"
  ensure_zdotdir_home_zshrc_bridge
  ensure_home_zshrc_sources_zdotdir
  ensure_zdotdir_file_sources \
    "$ZDOTDIR_DIR/.zprofile" \
    "$HOME/.zprofile" \
    "ZDOTDIR login profile. Bridges to ~/.zprofile (Homebrew/mise PATH)"
  ensure_zdotdir_file_sources \
    "$ZDOTDIR_DIR/.zshenv" \
    "$ZSHENV" \
    "ZDOTDIR env. Nested zsh already has ZDOTDIR set, so ~/.zshenv would be skipped"
  ensure_zdotdir_file_sources \
    "$ZDOTDIR_DIR/.zlogin" \
    "$HOME/.zlogin" \
    "ZDOTDIR login. Bridges to ~/.zlogin"
}

# Old ~/.zlogin ran `sudo rm -rf` on every login shell. macOS terminals are login
# shells, so that prompted on every new window. Replace with once-per-boot cleanup.
ensure_teams_cleanup() {
  local script="$HOME/.config/todd/zsh/teams_cleanup.zsh"
  local zlogin="$HOME/.zlogin"
  local version_marker="teams-cleanup-version: 1"
  local source_line='[[ -f "$HOME/.config/todd/zsh/teams_cleanup.zsh" ]] && source "$HOME/.config/todd/zsh/teams_cleanup.zsh"'

  echo "🧹 Ensuring Teams cache cleanup is once-per-boot (no unconditional sudo)..."

  if [[ -f "$zlogin" ]] && grep -qE '^sudo rm -rf .*microsoft\.teams' "$zlogin"; then
    sed -i '' -e '/^sudo rm -rf .*microsoft\.teams/d' "$zlogin"
    echo "  Removed unconditional Teams sudo from $zlogin"
  fi

  mkdir -p "$(dirname "$script")"
  if [[ ! -f "$script" ]] || ! grep -Fq "$version_marker" "$script"; then
    cat >"$script" <<'EOF'
# teams-cleanup-version: 1
# Microsoft Teams leftover caches can crash the app. Clean them once per boot.
# macOS terminals start login shells, so ~/.zlogin runs on every new window —
# do not call sudo there unconditionally.

todd_teams_cleanup_once_per_boot() {
  [[ -o interactive ]] || return 0

  local stamp_dir="${XDG_CACHE_HOME:-$HOME/.cache}/todd"
  local stamp="$stamp_dir/teams-cache-cleanup.boot"
  local boot_sec
  boot_sec="$(sysctl -n kern.boottime 2>/dev/null | awk '{print $4}' | tr -d ',')"
  [[ -n "$boot_sec" ]] || return 0

  if [[ -f "$stamp" && "$(<"$stamp")" == "$boot_sec" ]]; then
    return 0
  fi

  mkdir -p "$stamp_dir"
  print -r -- "$boot_sec" >"$stamp"

  local -a targets existing remaining
  targets=(
    "$HOME/Library/Group Containers/UBF8T346G9.com.microsoft.teams"
    "$HOME/Library/Containers/com.microsoft.teams2"
  )

  local t
  for t in "${targets[@]}"; do
    [[ -e "$t" ]] && existing+=("$t")
  done
  (( ${#existing[@]} )) || return 0

  echo "Cleaning Microsoft Teams caches (once since last restart):"
  for t in "${existing[@]}"; do
    if rm -rf "$t" 2>/dev/null; then
      echo "  removed: $t"
    else
      remaining+=("$t")
    fi
  done
  (( ${#remaining[@]} )) || return 0

  echo "Some Teams cache dirs need elevated permissions:"
  for t in "${remaining[@]}"; do
    echo "  $t"
  done
  echo "About to run: sudo rm -rf <those paths>"
  echo "This is requested at most once per reboot. Ctrl-C skips until the next restart."
  sudo rm -rf "${remaining[@]}"
}

todd_teams_cleanup_once_per_boot
EOF
    echo "  Wrote $script"
  fi

  if [[ ! -f "$zlogin" ]] || ! grep -Fq "teams_cleanup.zsh" "$zlogin"; then
    {
      echo "# Microsoft Teams cache cleanup (once per boot). Managed by ~/.config/bootstrap.sh"
      echo "$source_line"
    } >>"$zlogin"
    echo "  Ensured $zlogin sources teams_cleanup.zsh"
  fi
}

ensure_no_sdkroot_in_zshenv() {
  [[ -f "$ZSHENV" ]] || return 0
  if grep -qE '^export SDKROOT=|^export CFLAGS=.*isysroot|^export LDFLAGS=.*isysroot' "$ZSHENV" 2>/dev/null; then
    sed -i '' \
      -e '/^export SDKROOT=/d' \
      -e '/^export CFLAGS=.*isysroot/d' \
      -e '/^export LDFLAGS=.*isysroot/d' \
      "$ZSHENV"
    echo "  Removed SDKROOT/isysroot from $ZSHENV (interactive shell init only)"
  fi
}

ensure_bootstrap_sourced_in_zshrc() {
  local source_line='[[ -f "$HOME/.config/todd/zsh/bootstrap.zsh" ]] && source "$HOME/.config/todd/zsh/bootstrap.zsh"'
  grep -Fq "bootstrap.zsh" "$ZSHRC" 2>/dev/null && return 0
  mkdir -p "$(dirname "$ZSHRC")"
  echo "  Adding bootstrap.zsh source to $ZSHRC"
  if [[ -f "$ZSHRC" ]]; then
    sed -i '' "/^export OPENPROJECTS_PATH=/a\\
\\
# bootstrap-managed machine setup (see ~/.config/bootstrap.sh)\\
$source_line
" "$ZSHRC"
  else
    echo "$source_line" >>"$ZSHRC"
  fi
}

migrate_home_zshrc_to_bootstrap() {
  [[ -f "$HOME_ZSHRC" ]] || return 0
  if ! grep -qE 'bootstrap|Homebrew/macOS build flags|Homebrew build flags' "$HOME_ZSHRC" 2>/dev/null; then
    return 0
  fi
  if ! grep -Fq "$BUILD_FLAGS_MARKER" "$ZSH_BOOTSTRAP" 2>/dev/null; then
    echo "📦 Migrating shell config from ~/.zshrc to $ZSH_BOOTSTRAP ..."
    mkdir -p "$(dirname "$ZSH_BOOTSTRAP")"
    {
      echo "# bootstrap-managed shell config"
      echo "# Migrated from ~/.zshrc by bootstrap.sh"
      cat "$HOME_ZSHRC"
    } >>"$ZSH_BOOTSTRAP"
    cat >"$HOME_ZSHRC" <<'EOF'
# Shell config lives under ~/.config/todd/zsh/ (ZDOTDIR=~/.config/zsh in ~/.zshenv).
# Interactive startup file: ~/.config/zsh/.zshrc (sources ~/.config/todd/zsh/zshrc).
# Bootstrap-managed settings: ~/.config/todd/zsh/bootstrap.zsh
#
# New shells with ZDOTDIR set read ~/.config/zsh/.zshrc, not this file.
# Source that file so `source ~/.zshrc` and ZDOTDIR-unset shells still load
# aliases*.zsh. The guard in ~/.config/zsh/.zshrc prevents recursion.
if [[ -z ${_TODD_SOURCING_HOME_ZSHRC:-} && -f "${ZDOTDIR:-$HOME/.config/zsh}/.zshrc" ]]; then
  source "${ZDOTDIR:-$HOME/.config/zsh}/.zshrc"
fi
EOF
    echo "  Replaced ~/.zshrc with pointer stub"
  fi
}

ensure_mise_in_shell() {
  grep -Fq "$MISE_MARKER" "$ZSH_BOOTSTRAP" 2>/dev/null && return 0
  mkdir -p "$(dirname "$ZSH_BOOTSTRAP")"
  cat >>"$ZSH_BOOTSTRAP" <<'EOF'

# mise version manager (bootstrap)
if command -v mise >/dev/null 2>&1; then
  eval "$(mise activate zsh)"
fi
EOF
  echo "  Added mise activation to $ZSH_BOOTSTRAP"
}

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

ensure_zdotdir_in_zshenv
ensure_zdotdir_startup_files
ensure_teams_cleanup
ensure_no_sdkroot_in_zshenv
migrate_home_zshrc_to_bootstrap
ensure_bootstrap_sourced_in_zshrc

source "$ZSHENV"

resolve_macos_compiler() {
  local role="$1"
  local current=""

  if [ "$role" = "CC" ]; then
    current="${CC:-}"
  else
    current="${CXX:-}"
  fi

  if [ -n "$current" ] && [ -x "$current" ]; then
    printf '%s\n' "$current"
    return 0
  fi

  local gcc_ver
  gcc_ver="$(brew list --versions gcc 2>/dev/null | awk '{print $2}' | cut -d. -f1)"
  if [ -n "$gcc_ver" ]; then
    local brew_bin="${BREW_PREFIX}/bin/$([ "$role" = "CC" ] && echo "gcc-${gcc_ver}" || echo "g++-${gcc_ver}")"
    if [ -x "$brew_bin" ]; then
      printf '%s\n' "$brew_bin"
      return 0
    fi
  fi

  if [ "$role" = "CC" ]; then
    xcrun --find cc 2>/dev/null || echo clang
  else
    xcrun --find c++ 2>/dev/null || echo clang++
  fi
}

export_macos_build_env() {
  [[ "$(uname -s)" != Darwin ]] && return 0

  local sdk
  sdk="$(xcrun --sdk macosx --show-sdk-path 2>/dev/null || true)"
  if [ -n "$sdk" ]; then
    export SDKROOT="$sdk"
  fi

  export CC="$(resolve_macos_compiler CC)"
  export CXX="$(resolve_macos_compiler CXX)"
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

ensure_macos_build_env_in_shell() {
  echo "🛠️ Verify build env vars ..."
  [[ "$(uname -s)" != Darwin ]] && return 0

  if grep -Fq "$BUILD_FLAGS_MARKER" "$ZSH_BOOTSTRAP" 2>/dev/null ||
    grep -q 'Homebrew build flags' "$ZSH_BOOTSTRAP" 2>/dev/null; then
    return 0
  fi

  mkdir -p "$(dirname "$ZSH_BOOTSTRAP")"
  cat >>"$ZSH_BOOTSTRAP" <<EOF

# $BUILD_FLAGS_MARKER
if [[ "\$(uname -s)" == Darwin ]]; then
  [[ -z "\$SDKROOT" ]] && export SDKROOT="\$(xcrun --sdk macosx --show-sdk-path 2>/dev/null)"
  if [[ -n "\$SDKROOT" ]]; then
    [[ " \$CFLAGS " != *" -isysroot "* ]] && export CFLAGS="-isysroot \$SDKROOT"
    [[ " \$LDFLAGS " != *" -isysroot "* ]] && export LDFLAGS="\${LDFLAGS:+\$LDFLAGS }-isysroot \$SDKROOT"
  fi
  [[ " \$CPPFLAGS " != *" -I${BREW_PREFIX}/include "* ]] && \
    export CPPFLAGS="\${CPPFLAGS:+\$CPPFLAGS }-I${BREW_PREFIX}/include"
  [[ " \$LDFLAGS " != *" -L${BREW_PREFIX}/lib "* ]] && \
    export LDFLAGS="\${LDFLAGS:+\$LDFLAGS }-L${BREW_PREFIX}/lib"
  [[ ":\$PKG_CONFIG_PATH:" != *":${BREW_PREFIX}/opt/boost/lib/pkgconfig:"* ]] && \
    export PKG_CONFIG_PATH="${BREW_PREFIX}/opt/boost/lib/pkgconfig\${PKG_CONFIG_PATH:+:\$PKG_CONFIG_PATH}"
fi
EOF
  echo "  Added macOS build env vars to $ZSH_BOOTSTRAP"
  note_shell_init_for_builds
}

# Keep ~/.zshenv CC/CXX and gcc lib paths aligned with the installed Homebrew gcc.
ensure_gcc_build_env_in_zshenv() {
  [[ "$(uname -s)" != Darwin ]] && return 0

  local gcc_ver
  gcc_ver="$(brew list --versions gcc 2>/dev/null | awk '{print $2}' | cut -d. -f1)"
  if [ -z "$gcc_ver" ]; then
    return 0
  fi

  local gcc_bin="${BREW_PREFIX}/bin/gcc-${gcc_ver}"
  local gxx_bin="${BREW_PREFIX}/bin/g++-${gcc_ver}"
  local gcc_lib="${BREW_PREFIX}/lib/gcc/${gcc_ver}"
  if [ ! -x "$gcc_bin" ]; then
    return 0
  fi

  local old_cc="${CC:-}"
  if [ -f "$ZSHENV" ] && grep -q '^export CC=' "$ZSHENV" 2>/dev/null; then
    sed -i '' \
      -e "s|^export CC=.*|export CC=${gcc_bin}|" \
      -e "s|^export CXX=.*|export CXX=${gxx_bin}|" \
      -e "s|${BREW_PREFIX}/lib/gcc/[0-9][0-9]*|${gcc_lib}|g" \
      "$ZSHENV"
  fi

  export CC="$gcc_bin"
  export CXX="$gxx_bin"
  if [ -n "${LIBRARY_PATH:-}" ]; then
    export LIBRARY_PATH="$(echo "$LIBRARY_PATH" | sed "s|${BREW_PREFIX}/lib/gcc/[0-9][0-9]*|${gcc_lib}|g")"
  else
    export LIBRARY_PATH="$gcc_lib"
  fi
  if [ -n "${LD_LIBRARY_PATH:-}" ]; then
    export LD_LIBRARY_PATH="$(echo "$LD_LIBRARY_PATH" | sed "s|${BREW_PREFIX}/lib/gcc/[0-9][0-9]*|${gcc_lib}|g")"
  fi

  if [ -n "$old_cc" ] && [ "$old_cc" != "$gcc_bin" ] && [ ! -x "$old_cc" ]; then
    echo "⚠️  Stale CC ($old_cc) — updated to $gcc_bin"
  fi
  echo "✅ GCC build env synced to gcc-${gcc_ver}"
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

# Homebrew only ever creates symlinks directly under opt/ (into Cellar). A real
# directory there — e.g. a leftover manual shim like opt/jpeg/lib — is never
# managed by brew and makes upgrades die with
# "Error: Directory not empty @ dir_s_rmdir - <prefix>/opt/<formula>".
prune_stale_brew_opt_dirs() {
  local opt_root="$BREW_PREFIX/opt"
  [ -d "$opt_root" ] || return 0

  local path
  while IFS= read -r -d '' path; do
    echo "⚠️  Removing stale directory $path (opt entries must be symlinks; real dirs break brew upgrades)"
    rm -rf "$path"
  done < <(find "$opt_root" -mindepth 1 -maxdepth 1 -type d -print0)
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
  defaults write org.hammerspoon.Hammerspoon MJConfigFile "~/.config/todd/hammerspoon/init.lua" || true
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

echo "🧹 Pruning stale Homebrew opt directories..."
prune_stale_brew_opt_dirs

echo "📦 Installing Homebrew packages..."
# Allow emacs-plus link conflict — we force-link it immediately after
brew bundle --file="./Brewfile" || true

# Force-link emacs-plus@30, overwriting stale symlinks from /Applications/Emacs.app
if ! brew link --dry-run emacs-plus@30 &>/dev/null; then
  echo "🔗 Relinking emacs-plus@30 to overwrite stale symblinks..."
  brew unlink emacs-plus@30 &>/dev/null || true
  brew link --overwrite emacs-plus@30 || true
else
  echo "✅ emacs-plus@30 is already successfully linked."
fi

#################################
# Install ClickHouse
#################################
if ! command -v clickhouse >/dev/null 2>&1; then
  echo "🗄️ Installing ClickHouse..."
  curl https://clickhouse.com/cli | sh
  clickhousectl local use stable
else
  echo "🗄️ clickhouse command is available"
fi
# silently deal with any quarantine of clickhouse binary if it exists
CLICKHOUSE_BIN="$(command -v clickhouse 2>/dev/null || true)"
if [ -n "$CLICKHOUSE_BIN" ]; then
  xattr -d com.apple.quarantine "$CLICKHOUSE_BIN" &>/dev/null || true
fi

#################################
# Ensure gcc has libemutls_w.a
# (Homebrew bottles omit it; source build required for native comp)
#################################
ensure_gcc_emutls
ensure_gcc_build_env_in_zshenv

#################################
# Set globals for git
#################################
echo "🔀 Configure git ..."
git config --global status.submoduleSummary true
git config --global push.autoSetupRemote true
GITIGNORE_FILEPATH="$HOME/.gitignore_global"
git config --global core.excludesfile "$GITIGNORE_FILEPATH"
if [ ! -f "$GITIGNORE_FILEPATH" ]; then
  echo "  create $GITIGNORE_FILEPATH"
  cat >>"$GITIGNORE_FILEPATH" <<'EOF'
# ==========================================
# Operating System Temporary / Junk Files
# ==========================================

# macOS Specific
.DS_Store
.AppleDouble
.LSOverride
._*

# Windows Specific
Thumbs.db
Thumbs.db:encryptable
ehthumbs.db
Desktop.ini
$RECYCLE.BIN/
*.lnk

# Linux Specific
*~
.directory
.Trash-*
.fuse_hidden*

# ==========================================
# Optional: Common IDE / Editor Junk
# ==========================================
.vscode/
.idea/
*.swp
*.swo
*~
#*#
.elc
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
  echo "   Check your shell init files (.zshenv, .config/zsh/.zprofile, .config/todd/zsh/)."
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

is_valid_font_file() {
  local f="$1"
  [ -s "$f" ] || return 1
  file "$f" 2>/dev/null | grep -qiE 'truetype|opentype|font data|sfnt'
}

download_symbola() {
  local url="$1"
  echo "⬇️  Trying $url ..."
  curl -fsSL "$url" -o "$SYMBOLA_PATH.tmp" 2>/dev/null &&
    mv "$SYMBOLA_PATH.tmp" "$SYMBOLA_PATH"
}

if [ ! -f "$SYMBOLA_PATH" ]; then
  # Primary: a real raw file path on GitHub — NOT the bare domain
  download_symbola "https://raw.githubusercontent.com/zhm/symbola/master/fonts/Symbola.ttf"

  if ! is_valid_font_file "$SYMBOLA_PATH"; then
    echo "⚠️  Primary source failed or returned an invalid file — trying fallback..."
    rm -f "$SYMBOLA_PATH" "$SYMBOLA_PATH.tmp"
    # Fallback: official dn-works.com zip
    TMP_ZIP="$(mktemp -d)/symbola.zip"
    if curl -fsSL "https://dn-works.com/wp-content/uploads/2020/UFAS-Fonts/Symbola.zip" -o "$TMP_ZIP" &&
      unzip -qo "$TMP_ZIP" -d "$(dirname "$TMP_ZIP")"; then
      find "$(dirname "$TMP_ZIP")" -iname "Symbola.ttf" -exec cp {} "$SYMBOLA_PATH" \;
    fi
  fi

  if is_valid_font_file "$SYMBOLA_PATH"; then
    echo "✅ Symbola font downloaded successfully"
    echo "🔄 Rebuilding macOS font cache..."
    sudo atsutil databases -remove >/dev/null 2>&1
    atsutil server -shutdown >/dev/null 2>&1
    atsutil server -ping >/dev/null 2>&1
    echo "✅ Symbola font activated"
  else
    echo "❌ All downloads failed — please manually download from https://dn-works.com"
    rm -f "$SYMBOLA_PATH" "$SYMBOLA_PATH.tmp"
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

if ! grep -Fqs "$DOOM_BIN" "$ZSH_BOOTSTRAP" 2>/dev/null; then
  echo "path+=$DOOM_BIN" >>"$ZSH_BOOTSTRAP"
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
  # Persist both paths to bootstrap.zsh so interactive Emacs also gets them
  if ! grep -Fq "gcc/current/gcc/${GCC_ARCH}" "$ZSH_BOOTSTRAP" 2>/dev/null; then
    cat >>"$ZSH_BOOTSTRAP" <<EOF

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
# Configure support for pdf tools
#################################
EPDFINFO_BIN="${DOOM_DIR}/.local/straight/build-${EMACS_VER}/pdf-tools/epdfinfo"
PDF_TOOLS_AUTOBUILD="${DOOM_DIR}/.local/straight/repos/pdf-tools/server/autobuild"
if [ ! -f "$EPDFINFO_BIN" ]; then
  echo "🛠️ Building epdfinfo server (pdf-tools autobuild)..."
  epdfinfo_build_ok=0
  if verify_macos_compiler; then
    (
      export_macos_build_env
      sh "$PDF_TOOLS_AUTOBUILD" -i "$(dirname "$EPDFINFO_BIN")" -D
    ) && epdfinfo_build_ok=1
  fi
  if [ "$epdfinfo_build_ok" -eq 1 ] && [ -f "$EPDFINFO_BIN" ]; then
    echo "✅ epdfinfo built successfully"
  else
    echo "❌ epdfinfo build failed — see output above"
    note_shell_init_for_builds
  fi
else
  echo "✅ epdfinfo already built"
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
  install_pipx_tool grip
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
ensure_mise_in_shell

#################################
# Install goimports
#################################
if ! command -v goimports >/dev/null 2>&1; then
  echo "🐹 Installing goimports ..."
  if ! command -v go >/dev/null 2>&1; then
    echo "🐹 Go toolchain not found — installing globally via mise..."
    mise use -g go@latest
  fi
  # mise's shell hook can't activate tools mid-script, so fall back to
  # `mise exec` when go isn't on PATH in this shell context. Note mise sets
  # GOBIN to the toolchain's bin dir, so goimports lands on PATH automatically
  # in shells where the go tool is active — no ~/go/bin PATH hacks needed.
  if command -v go >/dev/null 2>&1; then
    go install golang.org/x/tools/cmd/goimports@latest
  else
    mise exec go@latest -- go install golang.org/x/tools/cmd/goimports@latest
  fi
else
  echo "🐹 goimports is already installed"
fi

#################################
# Rust configuration
#################################
if ! grep -Fqs "CARGO_NET_GIT_FETCH_WITH_CLI" "$ZSH_BOOTSTRAP" 2>/dev/null; then
  echo 'export CARGO_NET_GIT_FETCH_WITH_CLI=true' >>"$ZSH_BOOTSTRAP"
fi

#################################
# omp configuration
#################################
OMP_CONFIG="$HOME/.omp/agent/config.yml"
if [ ! -f "$OMP_CONFIG" ]; then
  echo "📁 Creating oh-my-pi config directory ..."
  mkdir -p "$(dirname "$OMP_CONFIG")"
  echo "📄 Creating oh-my-pi config file ..."
  touch "$OMP_CONFIG"
fi
if ! grep -q '^export BROWSER=' "$OMP_CONFIG"; then
  echo "🌐 Adding BROWSER environment variable for oh-my-pi ..."
  echo 'export BROWSER="com.google.Chrome"' >>"$OMP_CONFIG"
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

if ! grep -Fq 'DOCKER_CONTEXT' "$ZSH_BOOTSTRAP" 2>/dev/null; then
  echo 'export DOCKER_CONTEXT=colima' >>"$ZSH_BOOTSTRAP"
fi

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
# Microsoft Teams cache cleanup is handled by ensure_teams_cleanup
# (once per boot; see ~/.config/todd/zsh/teams_cleanup.zsh)
#################################

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
