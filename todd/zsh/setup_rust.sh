#!/usr/bin/env bash
##########
# install rust and diesel
#
# rust-analyzer must come from rustup, matching the active rustc. Homebrew's
# rust-analyzer would win on PATH (/opt/homebrew/bin is ahead of ~/.cargo/bin)
# and Emacs treats the rustup proxy as "installed" even when the component is
# missing — that produces the lsp-mode startup/Connected crash loop.
#
#   setup_rust.sh           full install (components + cargo tools + diesel)
#   setup_rust.sh --ensure  non-interactive: rustup + rust-analyzer + rust-src
##########

ensure_cargo_env() {
  if [[ -f "$HOME/.cargo/env" ]]; then
    # shellcheck source=/dev/null
    . "$HOME/.cargo/env"
  fi

  local cargo_bin="$HOME/.cargo/bin"
  if [[ -d "$cargo_bin" ]]; then
    case ":$PATH:" in
      *":$cargo_bin:"*) ;;
      *) export PATH="$cargo_bin:$PATH" ;;
    esac
    if [[ "$(type -t append_path 2>/dev/null)" == "function" ]]; then
      append_path "\$HOME/.cargo/bin"
    fi
  fi
}

ensure_cargo_env_in_zshenv() {
  local zshenv="${ZSHENV:-$HOME/.zshenv}"
  local line
  printf -v line '. "%s/.cargo/env"' "\$HOME"
  if [[ -f "$HOME/.cargo/env" ]]; then
    mkdir -p "$(dirname "$zshenv")"
    touch "$zshenv"
    grep -Fqs "$line" "$zshenv" || echo "$line" >>"$zshenv"
  fi
}

ensure_no_homebrew_rust_analyzer() {
  if command -v brew >/dev/null 2>&1 && brew list rust-analyzer &>/dev/null; then
    echo "🍺 Uninstalling Homebrew rust-analyzer (it shadows the rustup proxy on PATH)"
    brew uninstall rust-analyzer
  fi
}

ensure_rustup() {
  ensure_cargo_env
  if ! command -v rustup >/dev/null 2>&1; then
    echo "🦀 Installing rustup..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    ensure_cargo_env
    ensure_cargo_env_in_zshenv
  fi
  if ! command -v rustup >/dev/null 2>&1; then
    echo "❌ rustup not found after install" >&2
    return 1
  fi
}

ensure_rust_analyzer() {
  ensure_rustup
  ensure_no_homebrew_rust_analyzer

  echo "🦀 Ensuring rust-analyzer + rust-src on the default toolchain"
  rustup component add rust-analyzer rust-src

  local default_tc toolchain
  default_tc="$(rustup show active-toolchain 2>/dev/null | awk '{print $1}')"
  while IFS= read -r toolchain; do
    [[ -z "$toolchain" || "$toolchain" == */* ]] && continue
    [[ -n "$default_tc" && "$toolchain" == "$default_tc" ]] && continue
    echo "🦀 Ensuring rust-analyzer + rust-src on $toolchain"
    rustup component add rust-analyzer rust-src --toolchain "$toolchain" || \
      echo "⚠️  skipped rust-analyzer on $toolchain"
  done < <(rustup toolchain list | awk '{print $1}')

  if ! rust-analyzer --version >/dev/null 2>&1; then
    echo "❌ rust-analyzer is not runnable (rustup component missing?)" >&2
    rust-analyzer --version >&2 || true
    return 1
  fi
  echo "✅ $(rust-analyzer --version)"
}

ensure_rust_completions() {
  if ! command -v rustup >/dev/null 2>&1; then
    return 0
  fi
  if [[ ! -e ~/.config/zsh/_rustup.zsh || ! -e ~/.config/zsh/_cargo.zsh ]]; then
    mkdir -p ~/.config/zsh 2>/dev/null
    rustup completions zsh cargo >> ~/.config/zsh/_cargo.zsh
    rustup completions zsh > ~/.config/zsh/_rustup.zsh
    if command -v diesel >/dev/null 2>&1; then
      diesel completions zsh > ~/.config/zsh/_diesel.zsh
    fi
  fi
}

ensure_rust_analyzer
ensure_rust_completions

if [[ "${1:-}" == "--ensure" ]]; then
  exit 0
fi

# Cargo tools menu
if type "fzf" > /dev/null 2>&1; then
  CARGO_TOOLS=(
    "cargo-edit"
    "cargo-nextest"
    "cargo-llvm-cov"
    "cargo-audit --locked --features=fix"
    "rusty-hook"
  )

  SELECTED=$(printf "%s\n" "${CARGO_TOOLS[@]}" | fzf \
    --multi \
    --prompt="Select cargo tools to install (TAB to select, ENTER to confirm): " \
    --header="Cargo Tools Installer" \
    --marker="✓" \
    --pointer="▶" \
    --height=~100%)

  if [[ -z "$SELECTED" ]]; then
    echo "No cargo tools selected, skipping."
  else
    while IFS= read -r tool; do
      package=$(echo "$tool" | awk '{print $1}')
      flags=$(echo "$tool" | cut -d' ' -f2-)
      echo "Installing $package..."
      if [[ "$package" == "$flags" ]]; then
        cargo install "$package"
      else
        # shellcheck disable=SC2086
        cargo install "$package" $flags
      fi
    done <<< "$SELECTED"
  fi
else
  echo "Warning: fzf not found, installing all cargo tools. Install fzf (brew install fzf) to enable selection menu."
  cargo install cargo-edit
  cargo install cargo-nextest
  cargo install cargo-llvm-cov
  cargo install cargo-audit --locked --features=fix
  cargo install rusty-hook
fi

if [[ ! -f ~/.cargo/config.toml ]]; then
  if [[ ! -d ~/.cargo ]]; then
    mkdir ~/.cargo
  fi
  touch ~/.cargo/config.toml
fi
if ! grep -q clean-rdkafka-sys ~/.cargo/config.toml; then
  if ! grep -q '^\[alias\]' ~/.cargo/config.toml; then
    echo '[alias]' >>~/.cargo/config.toml
  fi
  echo '# Clears cmake output for rdkafka-sys (fixes stale CMAKE_INSTALL_PREFIX → /usr/local on macOS)' >>~/.cargo/config.toml
  echo 'clean-rdkafka-sys = ["clean", "-p", "rdkafka-sys"]' >>~/.cargo/config.toml
fi

read -rp "Install Rust Diesel CLI? [y/N] " reply
if [[ "$reply" =~ ^[Yy]$ ]]; then
  if ! type "diesel" > /dev/null 2>&1; then
    if brew list libpq &>/dev/null 2>&1; then
      RUSTFLAGS='-L /opt/homebrew/opt/libpq/lib' cargo install diesel_cli --no-default-features --features postgres
    else
      cargo install diesel_cli --no-default-features --features postgres
    fi
  fi
fi
