# rust aliases and functions for development

##########
# add hook for building relevant run aliases
##########
autoload -U add-zsh-hook

create_run_aliases() {
  local idx=0 
  # rust release binaries
  if [ -d target/release ]; then
    for f in target/release/*; do
      if [ -f "$f" ] && [ -x "$f" ]; then
        idx=$((idx+1))
	alias crr${idx}="cargo run --release --bin ${f##*/}";
      fi
    done; 
  fi
  # rust debug binaries
  if [ -d target/debug ]; then
    idx=0
    for f in target/debug/*; do
      if [ -f "$f" ] && [ -x "$f" ]; then
        idx=$((idx+1))
        alias crd${idx}="cargo run --bin ${f##*/}";
      fi
    done; 
  fi
}

add-zsh-hook chpwd create_run_aliases

alias cbd='cargo build'
alias cbr='cargo build --release'
alias crr='cargo run --release'
alias crd='cargo run'
alias ct='cargo test'
alias cti='LOGGING_LEVEL=info && cargo test -- --nocapture'
alias ctd='LOGGING_LEVEL=debug && cargo test -- --nocapture'
