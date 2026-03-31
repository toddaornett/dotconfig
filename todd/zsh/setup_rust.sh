#!/usr/bin/env bash
##########
# install rust and diesel
##########
if ! type "rustup" > /dev/null 2>&1; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
  append_path '$HOME/.cargo/bin'
  rustup component add rust-src
  rustup component add rust-analyzer
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

##########
# autocomplete for rust tools
##########
if [[ ! -e ~/.config/zsh/_rustup.zsh || ! -e ~/.config/zsh/_cargo.zsh ]] && type "rustup" > /dev/null 2>&1; then
  mkdir -p ~/.config/zsh 2>/dev/null
  rustup completions zsh cargo >> ~/.config/zsh/_cargo.zsh
  rustup completions zsh > ~/.config/zsh/_rustup.zsh
  if type "diesel" > /dev/null 2>&1; then
    diesel completions zsh > ~/.config/zsh/_diesel.zsh
  fi
fi
