# rust aliases and functions for development

##########
# add hook on chpwd for building relevant run aliases
##########
autoload -U add-zsh-hook

churlpr() {
  if [[ ! -f "Cargo.toml" ]]; then
    echo "Aborting, this is not a Rust project"
    return 1;
  fi

  # config from environment variables
  local url_from=$CHURLPR_FROM
  local url_to=$CHURLPR_TO
  local commit_check_limit=${CHURLPR_COMMIT_CHECK_LIMIT:-10}
  local commit_title=${CHURLPR_TITLE:-"chore: update url"}
  local branch_name=$(echo "$commit_title" | sed -E 's/([^:]+): (([^ ]+ )*url).*/\1\/\2/; s/ /-/g')
  local cfg_file_path=${CHURLPR_FILE_PATH:-.cargo/config.toml}

  # make sure are intended branch does not remotely exist
  # and create it locally
  git fetch origin
  if git show-ref --verify --quiet refs/remotes/origin/$branch_name; then
    echo "Aborting, the branch '$branch_name' exists on the remote origin."
  fi
  if ! git checkout -b $branch_name; then
    return $?
  fi

  # get latest code from the main branch
  git checkout $(git_main_branch)
  git pull

  # abort if this change was already merged
  if git log -n $commit_check_limit --pretty=format:%s | grep -q "$commit_title"; then
    echo "Aborting, change has already been made"
    return 1
  fi

  # make the url change
  if sed -i '' -e "s!$url_from!$url_to!" $cfg_file_path; then
    cargo clean
    cargo build
  fi

  if git diff --quiet; then
    echo "No changes to commit."
  else
    # switch to the new branch and create git commit
    if git checkout $branch_name; then
      local commit_message_file=$(mktemp -t tmp_cuupr_message)
      echo "$CHURLPR_TITLE" >"$commit_message_file"
      git add .
      git commit -F "$commit_message_file"
      echo "Url updated and git committed with any lock file changes"
      rm -f "$commit_message_file"
    fi
  fi
}

cuupr() {
  if [[ ! -f "Cargo.toml" ]]; then
    echo "Aborting, this is not a Rust project"
    return 1;
  fi

  # first make sure are intended branch does not remotely exist
  # and create it locally
  local branch_name="build/deps"
  git fetch origin
  if git show-ref --verify --quiet refs/remotes/origin/$branch_name; then
    echo "Aborting, the branch '$branch_name' exists on the remote origin."
  fi
  if ! git checkout -b $branch_name; then
    return $?
  fi

  # get latest code from the main branch
  git checkout $(git_main_branch)
  git pull

  # create temporary file for git commit message body that
  # includes cargo command output
  local temp_file=$(mktemp -t tmp_cuupr)
  if [[ ! -e $temp_file ]]; then
    echo "Failed to create temporary file."
    return 1
  fi

  echo "cargo upgrade and update" >"$temp_file"
  echo "\`\`\`sh" >>"$temp_file"

  local prompt="➜  $(basename "$PWD") git:($(git_main_branch)) ✗"
  echo "$prompt cargo upgrade" >>"$temp_file"
  cargo upgrade &>>"$temp_file"

  # support -C option to provide a view of incompatible changes without updating them
  if grep -q "note: Re-run with \`--incompatible\` to upgrade incompatible version requirements" "$temp_file"; then
    local compatibility="--incompatible"
    if [ "$1" = "-C" ]; then
      compatibility="--dry-run --incompatible"
    fi
    echo "$prompt cargo upgrade $compatibility" >>"$temp_file"
    cargo upgrade $(echo $compatibility | xargs) &>>"$temp_file"
  fi

  # cargo update for packages in special registries that need explicit updating
  for line in ${(f)"$(cargo update --dry-run |& grep 'Updating' | grep '(registry')"}
  do
    parts=(${(s: :)line})
    package_name=${parts[2]}
    version=${parts[-1]#v}
    echo "$prompt cargo upgrade -p ${package_name}@${version}" >>"$temp_file"
    cargo upgrade -p ${package_name}@${version} &>>"$temp_file"
  done

  echo "$prompt cargo update" >>"$temp_file"
  cargo update &>>"$temp_file"

  echo "\`\`\`" >>"$temp_file"

  if git diff --quiet; then
    echo "No changes to commit."
  else
    # switch to the new branch and create git commit
    if git checkout $branch_name; then
      local commit_message_file=$(mktemp -t tmp_cuupr_message)
      echo "build(deps): update all dependencies" >"$commit_message_file"
      echo "" >>"$commit_message_file"
      cat "$temp_file" >>"$commit_message_file"
      git add .
      git commit -F "$commit_message_file"
      echo "Dependencies upgraded, updated and git committed"
      rm -f "$commit_message_file"
    fi
  fi
  rm -f "$temp_file"
}

cruupr() {
  if [[ ! -f "Cargo.toml" ]]; then
    echo "Aborting, this is not a Rust project"
    return 1;
  fi

  # switch to build/deps
  local branch_name="build/deps"
  if ! git checkout $branch_name; then
    return $?
  fi

  # create temporary file for git commit message body that
  # includes cargo command output
  local temp_file=$(mktemp -t tmp_cuupr)
  if [[ ! -e $temp_file ]]; then
    echo "Failed to create temporary file."
    return 1
  fi

  echo "cargo upgrade and update" >"$temp_file"
  echo "\`\`\`sh" >>"$temp_file"

  local prompt="➜  $(basename "$PWD") git:($(git_main_branch)) ✗"
  echo "$prompt cargo upgrade" >>"$temp_file"
  cargo upgrade &>>"$temp_file"

  # support -C option to provide a view of incompatible changes without updating them
  if grep -q "note: Re-run with \`--incompatible\` to upgrade incompatible version requirements" "$temp_file"; then
    local compatibility="--incompatible"
    if [ "$1" = "-C" ]; then
      compatibility="--dry-run --incompatible"
    fi
    echo "$prompt cargo upgrade $compatibility" >>"$temp_file"
    cargo upgrade $(echo $compatibility | xargs) &>>"$temp_file"
  fi

  # cargo update for packages in special registries that need explicit updating
  for line in ${(f)"$(cargo update --dry-run |& grep 'Updating' | grep '(registry')"}
  do
    parts=(${(s: :)line})
    package_name=${parts[2]}
    version=${parts[-1]#v}
    echo "$prompt cargo upgrade -p ${package_name}@${version}" >>"$temp_file"
    cargo upgrade -p ${package_name}@${version} &>>"$temp_file"
  done

  echo "$prompt cargo update" >>"$temp_file"
  cargo update &>>"$temp_file"

  echo "\`\`\`" >>"$temp_file"

  if git diff --quiet; then
    echo "No changes to commit."
  else
    # switch to the new branch and create git commit
    if git checkout $branch_name; then
      local commit_message_file=$(mktemp -t tmp_cuupr_message)
      echo "build(deps): update all dependencies" >"$commit_message_file"
      echo "" >>"$commit_message_file"
      cat "$temp_file" >>"$commit_message_file"
      git add .
      git commit -F "$commit_message_file"
      echo "Dependencies upgraded, updated and git committed"
      rm -f "$commit_message_file"
    fi
  fi
  rm -f "$temp_file"
}

cclippyfix() {
  if [[ ! -f "Cargo.toml" ]]; then
    echo "Aborting, this is not a Rust project"
    return 1;
  fi

  # first make sure are intended branch does not remotely exist
  # and create it locally
  local branch_name="refactor/clippy"
  git fetch origin
  if git show-ref --verify --quiet refs/remotes/origin/$branch_name; then
    echo "Aborting, the branch '$branch_name' exists on the remote origin."
  fi
  if ! git checkout -b $branch_name; then
    return $?
  fi

  # get latest code from the main branch
  git checkout $(git_main_branch)
  git pull

  # create temporary file for git commit message body that
  # includes cargo command output
  local temp_file=$(mktemp -t tmp_cclippyfix)
  if [[ ! -e $temp_file ]]; then
    echo "Failed to create temporary file."
    return 1
  fi

  echo "\`\`\`sh" >"$temp_file"

  local prompt="➜  $(basename "$PWD") git:($(git_main_branch)) ✗"
  echo "$prompt cargo clippy --fix" >>"$temp_file"
  cargo clippy --fix &>>"$temp_file"

  echo "$prompt cargo clippy --all-targets --all-features -- -D warnings" >>"$temp_file"
  cargo clippy --all-targets --all-features -- -D warnings &>>"$temp_file"

  echo "\`\`\`" >>"$temp_file"

  if git diff --quiet; then
    echo "No changes to commit."
  else
    # switch to the new branch and create git commit
    if git checkout $branch_name; then
      local commit_message_file=$(mktemp -t tmp_cclippyfix_message)
      echo "refactor: clippy fixes" >"$commit_message_file"
      echo "" >>"$commit_message_file"
      cat "$temp_file" >>"$commit_message_file"
      git add .
      git commit -F "$commit_message_file"
      echo "Ran cargo clippy --fix and git committed"
      rm -f "$commit_message_file"
    fi
  fi
  rm -f "$temp_file"
}

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

cmd='cargo fmt && cargo clippy --all-targets --all-features -- -D warnings && cargo build'
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

cmd='cargo llvm-cov nextest --all-features'
alias cnt="echo \"$cmd\" && $cmd"

cmd='cargo llvm-cov nextest --all-features --no-capture'
alias cntp="echo \"$cmd\" && $cmd"

cmd='cargo llvm-cov report --html --output-dir coverage'
alias cntr="echo \"$cmd\" && $cmd"

# list mostly cargo related aliases
alias acargo="alias | egrep '^c.+='"
