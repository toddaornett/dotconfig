# rust aliases and functions for development

##########
# add hook on chpwd for building relevant run aliases
##########
autoload -U add-zsh-hook

create_run_aliases() {
  if [ -f Cargo.toml ]; then
    local suffix=""
    local cmd
    if [ -d src/openapi_spec ]; then
      suffix=" --features openapi-spec"
    else
      suffix=""
    fi
    cmd="cargo run${suffix}"
    alias crd="echo \"$cmd\" && $cmd"
    local idx=0
    # rust release binaries
    if [ -d target/release ]; then
      for f in target/release/*; do
        if [ -f "$f" ] && [ -x "$f" ]; then
          idx=$((idx + 1))
          cmd="cargo run --release --bin ${f##*/}${suffix}"
          alias crr${idx}="echo \"$cmd\" && $cmd"
        fi
      done
    fi
    for (( ; ; )); do
      idx=$((idx + 1))
      if alias crr${idx} >/dev/null 2>&1; then
        unalias crr${idx}
      else
        break
      fi
    done
    # rust debug binaries
    idx=0
    if [ -d target/debug ]; then
      for f in target/debug/*; do
        if [ -f "$f" ] && [ -x "$f" ]; then
          idx=$((idx + 1))
          cmd="cargo run --bin ${f##*/}${suffix}"
          alias crd${idx}="echo \"$cmd\" && $cmd"
        fi
      done
    fi
    for (( ; ; )); do
      idx=$((idx + 1))
      if alias crd${idx} >/dev/null 2>&1; then
        unalias crd${idx}
      else
        break
      fi
    done
  fi
}

add-zsh-hook chpwd create_run_aliases

# static aliases
local cmd='cargo audit'
alias ca="echo \"$cmd\" && $cmd"

cmd='cargo fmt && cargo clippy --all-features -- -D warnings && cargo build'
alias cbd="echo \"$cmd\" && $cmd"

cmd='cargo build --release'
alias cbr="echo \"$cmd\" && $cmd"

cmd='cargo add'
alias cda="echo \"$cmd\" && $cmd"

cmd='cargo remove'
alias cdr="echo \"$cmd\" && $cmd"

cmd='cargo fmt'
alias cf="echo \"$cmd\" && $cmd"

cmd='cargo check'
alias ck="echo \"$cmd\" && $cmd"

cmd='cargo run'
alias crd="echo \"$cmd\" && $cmd"

cmd='cargo run --release'
alias crr="echo \"$cmd\" && $cmd"

cmd='cargo test'
alias ct="echo \"$cmd\" && $cmd"

cmd='cargo test -- --nocapture'
alias ctp="echo \"$cmd\" && $cmd"

cmd='cargo nextest run'
alias cnt="echo \"$cmd\" && $cmd"

cmd='cargo nextest run --no-capture'
alias cntp="echo \"$cmd\" && $cmd"

# list mostly cargo related aliases
alias acargo="alias | egrep '^c.+='"
