#!/usr/bin/env bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup component add rust-src
rustup component add rust-analyzer
cargo install cargo-edit
cargo install cargo-audit
cargo install cargo-llvm-cov
cargo install cargo-nextest
cargo install diesel_cli
cargo install rusty-hook
