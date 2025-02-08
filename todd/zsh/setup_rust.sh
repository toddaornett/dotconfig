#!/usr/bin/env bash

##########
# install rust and diesel
##########
if ! type "rustup" > /dev/null; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
  append_path '$HOME/.cargo/bin'
  rustup component add rust-src
  rustup component add rust-analyzer
  cargo install cargo-edit
  cargo install cargo-nextest
  cargo install cargo-llvm-cov
  cargo install cargo-audit --locked --features=fix
  cargo install rusty-hook
fi
if ! type "diesel" > /dev/null; then
  RUSTFLAGS='-L /opt/homebrew/opt/libpq/lib' cargo install diesel_cli --no-default-features --features postgres
fi

##########
# autocomplete for rust tools
##########
if [ ! -e ~/.config/zsh/_rustup.zsh -o ! -e ~/.config/zsh/_cargo.zsh -a $commands[rustup] ]; then
  mkdir -p ~/.config/zsh 2>&1 >/dev/null
  rustup completions zsh cargo >> ~/.config/zsh/_cargo.zsh
  rustup completions zsh > ~/.config/zsh/_rustup.zsh
  diesel completions zsh > ~/.config/zsh/_diesel.zsh
fi
