# rust aliases and functions for development

##########
# add hook for building relevant run aliases
##########
autoload -U add-zsh-hook

create_run_aliases() {
  if [ -f Cargo.toml ]; then
    local idx=0
    # rust release binaries
    if [ -d target/release ]; then
      for f in target/release/*; do
        if [ -f "$f" ] && [ -x "$f" ]; then
          idx=$((idx + 1))
          alias crr${idx}="cargo run --release --bin ${f##*/}"
          alias crr${idx}d="LOGGING_LEVEL=debug && cargo run --release --bin ${f##*/}"
          alias crr${idx}i="LOGGING_LEVEL=info && cargo run --release --bin ${f##*/}"
        fi
      done
    fi
    # rust debug binaries
    if [ -d target/debug ]; then
      idx=0
      for f in target/debug/*; do
        if [ -f "$f" ] && [ -x "$f" ]; then
          idx=$((idx + 1))
          alias crd${idx}="cargo run --bin ${f##*/}"
          alias crd${idx}d="LOGGING_LEVEL=debug && cargo run --bin ${f##*/}"
          alias crd${idx}i="LOGGING_LEVEL=info && cargo run --bin ${f##*/}"
        fi
      done
    fi
  fi
}

add-zsh-hook chpwd create_run_aliases

alias cbd='cargo build'
alias cbr='cargo build --release'
alias crr='cargo run --release'
alias crrd='LOGGING_LEVEL=debug && cargo run --release'
alias crri='LOGGING_LEVEL=info && cargo run --release'
alias crd='cargo run'
alias crdd='LOGGING_LEVEL=debug && cargo run'
alias crdi='LOGGING_LEVEL=info && cargo run'
alias ct='cargo test'
alias cti='LOGGING_LEVEL=info && cargo test -- --nocapture'
alias ctd='LOGGING_LEVEL=debug && cargo test -- --nocapture'
