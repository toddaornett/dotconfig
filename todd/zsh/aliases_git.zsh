# Git version checking
autoload -Uz is-at-least
git_version=$(git version 2>/dev/null | awk '{print $3}')

# Auto-format git config indentation after git config writes.
git() {
  command git "$@"
  local exit_status=$?
  if [[ $exit_status -eq 0 && "${1:-}" == "config" ]]; then
    command sed -i '' $'s/^\t/  /' ~/.gitconfig ~/.gitconfig-*(N) >/dev/null 2>&1 || true
  fi
  return $exit_status
}

function git_current_branch () {
  local ref
  ref="$(git symbolic-ref --quiet HEAD 2>/dev/null)"
  local ret=$?
  if [[ $ret != 0 ]]
  then
    [[ $ret == 128 ]] && return 0
      ref=$(git rev-parse --short HEAD 2>/dev/null)  || return 0
  fi
  echo "${ref#refs/heads/}"
}

function git_main_branch () {
  command git rev-parse --git-dir &> /dev/null || return
  local branch
  for branch in main trunk
  do
    if command git show-ref -q --verify refs/heads/$branch
    then
      echo $branch
      return
    fi
  done
  echo master
}

# automatically add upstream branch based on origin name.
# This assumes that s simple sed expression for changing name
# in env $GIT_UPSTREAM_NAME_REPLACE_PATTERN will suffice. For
# example:
# export GIT_UPSTREAM_NAME_REPLACE_PATTERN='s/my_fork_of_something_fabulous/something_fabulous/'
function grau {
  if ! git remote -v | grep -q "^upstream"
then
    source=$(git remote -v | grep "^origin" | head -1 | cut -f 2 | cut -d ' ' -f 1 | sed -e $GIT_UPSTREAM_NAME_REPLACE_PATTERN)
    git remote add upstream $source
  fi
}

# Pretty log messages
function _git_log_prettily(){
  if ! [ -z $1 ]; then
    git log --pretty=$1
  fi
}

function git_cleanup_branches {
  git remote prune origin
  git fetch --prune
  git branch --merged $(git_main_branch) | grep -v "^[ *]*$(git_main_branch)$" | grep -v '[ *]*release.*$'| xargs git branch -d
}

function gstA {
  if (($# > 0))
  then
    git stash apply "$@"
  else
    git stash apply
  fi
}

function gstP {
  if (($# > 0))
  then
    git stash pop "$@"
  else
    git stash pop
  fi
}

function gstp {
  if (($# > 0))
  then
    git stash push "$@"
  else
    print 'Usage: gstp [-u] -m "<description for change to stash>"'
  fi
}

function gstD {
  if (($# > 0))
  then
    git stash drop "$@"
  else
    git stash drop
  fi
}

function prs {
  local repo prs pr_number approvals sha checks
  # Get the current repository's full name (e.g., owner/repo)
  repo=$(gh repo view --json nameWithOwner --jq '.nameWithOwner')

  # Fetch all open PRs created by you in the current repo
  prs=$(gh api -H "Accept: application/vnd.github+json" \
    "/repos/$repo/pulls?state=open&author=$(gh api user --jq .login)" \
    --jq '.[] | {number: .number}')

  # Loop through each PR to check approvals and status checks
  echo "$prs" | jq -c '.' | while read -r pr; do
    pr_number=$(echo "$pr" | jq -r '.number')

    # Get the number of approvals
    approvals=$(gh api -H "Accept: application/vnd.github+json" \
      "/repos/$repo/pulls/$pr_number/reviews" \
      --jq '[.[] | select(.state == "APPROVED")] | length')

    # Get the head commit SHA using headRefOid
    sha=$(gh pr view $pr_number --repo $repo --json headRefOid --jq '.headRefOid')

    # Check if all required status checks have passed
    checks=$(gh api -H "Accept: application/vnd.github+json" \
      "/repos/$repo/commits/$sha/check-runs" \
      --jq '.check_runs | all(.status == "completed" and .conclusion == "success")')

    if [ "$checks" = "true" ]; then
      echo "PR #$pr_number: $approvals approvals"
    fi
  done
}

# delete specified branch locally and on remote
function gbdr() {  # Check if branch name is provided
  if [ -z "$1" ]; then
    echo "Error: Branch name must be provided"
    return 1
  fi

  if git branch -D "$1" > /dev/null 2>&1; then

  fi
}

function gbdr() {
  local branch="$1"
  local remote="${2:-origin}"

  # Check if branch name is provided
  if [[ -z "$branch" ]]; then
    echo "Error: Branch name required"
    return 1
  fi

  # 1. Delete local branch
  if git show-ref --quiet "refs/heads/$branch"; then
    git branch -D "$branch" || {
      echo "Failed to delete local branch $branch"
      return 1
    }
    echo "Deleted local branch $branch"
  else
    echo "Local branch $branch does not exist"
  fi

  # 2. Delete remote branch
  if git ls-remote --quiet "$remote" "$branch" | grep -q .; then
    git push "$remote" --delete "$branch" || {
      echo "Failed to delete remote branch $remote/$branch"
      return 1
    }
    echo "Deleted remote branch $remote/$branch"
  else
    echo "Remote branch $remote/$branch does not exist"
  fi

  # 3. Delete remote-tracking branch
  if git show-ref --quiet "refs/remotes/$remote/$branch"; then
    git branch -r -D "$remote/$branch" || {
      echo "Failed to delete remote-tracking branch $remote/$branch"
      return 1
    }
    echo "Deleted remote-tracking branch $remote/$branch"
  else
    echo "Remote-tracking branch $remote/$branch does not exist"
  fi
}

# delete a specified branch in Git repositories
function nuke_branch() {
  # Check if branch name is provided
  if [ -z "$1" ]; then
    echo "Error: Branch name must be provided"
    return 1
  fi

  # Default to ~/Projects if no directory provided
  local root_dir="${2:-$HOME/Projects}"

  # Iterate through all subdirectories
  for dir in "$root_dir"/* ; do
    # Check if directory exists and contains .git
    if [ -d "$dir" ] && [ -d "$dir/.git" ]; then
      echo "Processing repository: $(basename $dir)"
      cd "$dir" || continue

      # Determine main branch (try main, then master)
      local main_branch
      if git show-ref --quiet refs/heads/main; then
        main_branch="main"
      elif git show-ref --quiet refs/heads/master; then
        main_branch="master"
      else
        echo "No main or master branch found in $dir, skipping"
        continue
      fi

      # Switch to main branch
      if ! git checkout "$main_branch" > /dev/null 2>&1; then
        echo "Failed to switch to $main_branch in $dir, skipping"
        continue
      fi

      # Delete requested branch if it exists
      if git show-ref --quiet "refs/heads/$1"; then
        if git branch -D "$1" > /dev/null 2>&1; then
          echo "Deleted branch $1 in $dir"
        else
          echo "Failed to delete $1 in $dir"
        fi
      else
        echo "No branch $1 found in $dir"
      fi
    fi
  done
}

# find a specified branch in Git repositories
function find_branch() {
  if [ -z "$1" ]; then
    echo "Error: Branch name must be provided"
    return 1
  fi
  local root_dir="${2:-$HOME/Projects}"
  for dir in "$root_dir"/* ; do
    # TODO: remove this name check when no longer relevant
    if [[ "$(basename $dir)" == "j"* ]]; then
      continue
    fi
    if [ -d "$dir" ] && [ -d "$dir/.git" ]; then
      cd "$dir" || continue
      if git show-ref --quiet "refs/heads/$1"; then
        echo "Found branch $1 in $(basename $dir)"
      fi
    fi
  done
}

#
# Aliases
#
alias g='git'
alias lg='lazygit'

alias ga='git add'
alias gaa='git add --all'
alias gap='git apply'
alias gapa='git add --patch'
alias gapt='git apply --3way'
alias gau='git add --update'
alias gav='git add --verbose'

alias gBB='git blame -b -w'

alias gb='git branch'
alias gba='git branch -a'
alias gbd='git branch -d'
alias gbda='git branch --no-color --merged | command grep -vE "^(\+|\*|\s*($(git_main_branch)|development|develop|devel|dev)\s*$)" | command xargs -n 1 git branch -d'
alias gbD='git branch -D'
alias gbm='git branch -m'
alias gbnm='git branch --no-merged'
alias gbl='git branch --list'
alias gbr='git branch --remote'
alias gbs='git bisect'
alias gbsb='git bisect bad'
alias gbsg='git bisect good'
alias gbsr='git bisect reset'
alias gbss='git bisect start'
alias gbp='git branch --merged | grep -v "^\*\\|main" | xargs -n 1 git branch -d && git remote prune origin'
alias gc='git commit -v'
alias gc!='git commit -v --amend'
alias gcn!='git commit -v --no-edit --amend'
alias gca='git commit -v -a'
alias gca!='git commit -v -a --amend'
alias gcan!='git commit -v -a --no-edit --amend'
alias gcans!='git commit -v -a -s --no-edit --amend'
alias gcam='git commit -a -m'
alias gcsm='git commit -s -m'
alias gcb='git checkout -b'
alias gcf='git config --list'
alias gcl='git clone --recurse-submodules'
alias gclean='git clean -id'
alias gpristine='git reset --hard && git clean -dffx'
alias gcmsg='git commit -m'
alias gcr='git checkout release'
alias gcm='git checkout $(git_main_branch)'
alias gco='git checkout'
alias gcount='git shortlog -sn'
alias gkp='git cherry-pick'
alias gkpa='git cherry-pick --abort'
alias gkpc='git cherry-pick --continue'
alias gcs='git commit -S'

alias gd='git diff'
alias gdca='git diff --cached'
alias gdcw='git diff --cached --word-diff'
alias gdct='git describe --tags $(git rev-list --tags --max-count=1)'
alias gds='git diff --staged'
alias gdt='git diff-tree --no-commit-id --name-only -r'
alias gdw='git diff --word-diff'

alias mycommits='git log --pretty=format:"%h%x09%an%x09%ad%x09%s" 2>&1 | grep Todd | grep -v Merge'

function gdnolock() {
  git diff "$@" ":(exclude)package-lock.json" ":(exclude)*.lock"
}

function gdv() { git diff -w "$@" | view - }

alias gf='git fetch'
# --jobs=<n> was added in git 2.8
is-at-least 2.8 "$git_version" \
  && alias gfa='git fetch --all --prune --jobs=10' \
  || alias gfa='git fetch --all --prune'
alias gfo='git fetch origin'

alias gfg='git ls-files | grep'

alias gg='git gui citool'
alias gga='git gui citool --amend'

function ggf() {
  [[ "$#" != 1 ]] && local b="$(git_current_branch)"
  git push --force origin "${b:=$1}"
}

function ggfl() {
  [[ "$#" != 1 ]] && local b="$(git_current_branch)"
  git push --force-with-lease origin "${b:=$1}"
}

function ggp() {
  if [[ "$#" != 0 ]] && [[ "$#" != 1 ]]; then
    git pull origin "${*}"
  else
    [[ "$#" == 0 ]] && local b="$(git_current_branch)"
    git pull origin "${b:=$1}"
  fi
}

function ggP() {
  if [[ "$#" != 0 ]] && [[ "$#" != 1 ]]; then
    git push origin "${*}"
  else
    [[ "$#" == 0 ]] && local b="$(git_current_branch)"
    git push origin "${b:=$1}"
  fi
}

function ggpnp() {
  if [[ "$#" == 0 ]]; then
    ggp && ggP
  else
    ggp "${*}" && ggP "${*}"
  fi
}

function ggu() {
  [[ "$#" != 1 ]] && local b="$(git_current_branch)"
  git pull --rebase origin "${b:=$1}"
}

alias ggpur='ggu'
alias ggpull='git pull origin "$(git_current_branch)"'
alias ggpush='git push origin "$(git_current_branch)"'

alias ggsup='git branch --set-upstream-to=origin/$(git_current_branch)'

alias ghh='git help'

alias gignored='git ls-files -v | grep "^[[:lower:]]"'
alias git-svn-dcommit-push='git svn dcommit && git push github $(git_main_branch):svntrunk'

alias gk='\gitk --all --branches'
alias gke='\gitk --all $(git log -g --pretty=%h)'

alias gp='git pull'
alias gl1='git log --pretty=format:"%H %ad %an %s" --date=short'
alias glg='git log --stat'
alias glgp='git log --stat -p'
alias glgg='git log --graph'
alias glgga='git log --graph --decorate --all'
alias glgm='git log --graph --max-count=10'
alias glo='git log --oneline --decorate'
alias glol="git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'"
alias glols="git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --stat"
alias glod="git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset'"
alias glods="git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset' --date=short"
alias glola="git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --all"
alias glog='git log --oneline --decorate --graph'
alias gloga='git log --oneline --decorate --graph --all'
alias glp="_git_log_prettily"

alias gm='git merge'
alias gmom='git merge origin/$(git_main_branch)'
alias gmt='git mergetool --no-prompt'
alias gmtvim='git mergetool --no-prompt --tool=vimdiff'
alias gmum='git merge upstream/$(git_main_branch)'
alias gma='git merge --abort'

alias gP='git push'
alias gPU='git push --set-upstream origin "$(git_current_branch)"'
alias gPd='git push --dry-run'
alias gPf='git push --force-with-lease'
alias gPf!='git push --force'
alias gPv='git push -v'

alias gr='git remote'
alias gra='git remote add'
alias grb='git rebase'
alias grba='git rebase --abort'
alias grbc='git rebase --continue'
alias grbd='git rebase develop'
alias grbi='git rebase -i'
alias grbm='git rebase $(git_main_branch)'
alias grbs='git rebase --skip'
alias grev='git revert'
alias grh='git reset'
alias grhh='git reset --hard'
alias groh='git reset origin/$(git_current_branch) --hard'
alias grm='git rm'
alias grmc='git rm --cached'
alias grmv='git remote rename'
alias grrm='git remote remove'
alias grs='git restore'
alias grset='git remote set-url'
alias grss='git restore --source'
alias grt='cd "$(git rev-parse --show-toplevel || echo .)"'
alias gru='git reset --'
alias grup='git remote update'
alias grv='git remote -v'

alias gsb='git status -sb'
alias gsd='git svn dcommit'
alias gsf='git fetch origin && git status'
alias gsh='git show'
alias gsi='git submodule init'
alias gsps='git show --pretty=short --show-signature'
alias gsr='git svn rebase'
alias gss='git status -s'
alias gs='git status'

# use the default stash push on git 2.13 and newer
#is-at-least 2.13 "$git_version" \
#  && alias gsta='git stash push -u' \
#  || a

alias gstC='git stash clear'
alias gstl='git stash list'
alias gsts='git stash show --text'
alias gstu='git stash --include-untracked'
alias gstall='git stash --all'
alias gSu='git submodule update'
alias gsw='git switch'
alias gswc='git switch -c'

alias gtD='gtD(){ git tag -d "${1}" && git push --delete origin refs/tags/"${1}" && git push --delete upstream refs/tags/"${1}" }; noglob gtD'
alias gts='git tag -s'
alias gtv='git tag | sort -V'
alias gtl='gtl(){ git tag --sort=-v:refname -n -l "${1}*" }; noglob gtl'

alias gtrackoff='git update-index --assume-unchanged'
alias gtrackon='git update-index --no-assume-unchanged'
alias gunwip='git log -n 1 | grep -q -c "\-\-wip\-\-" && git reset HEAD~1'
alias gup='git pull --rebase'
alias gupv='git pull --rebase -v'
alias gupa='git pull --rebase --autostash'
alias gupav='git pull --rebase --autostash -v'
alias glum='git pull upstream $(git_main_branch)'

alias gwch='git whatchanged -p --abbrev-commit --pretty=medium'
alias gwip='git add -A; git rm $(git ls-files --deleted) 2> /dev/null; git commit --no-verify --no-gpg-sign -m "--wip-- [skip ci]"'

alias gam='git am'
alias gamc='git am --continue'
alias gams='git am --skip'
alias gama='git am --abort'
alias gamscp='git am --show-current-patch'

# Todd's special ones
alias greset='branchname=$(git rev-parse --abbrev-ref HEAD); git fetch origin; git reset --hard origin/$branchname; unset branchname'
alias gpro='git config --get remote.origin.url'
alias gpru='git config --get remote.upstream.url'

alias agit="( alias | grep '^g'; typeset +f | grep '^g' ) | sort"

# cleanup git branches and artifacts for rust and node projects
function pclean {
  local p
  local current_branch_name
  local main_branch_name
  local remote_main_branch_name
  local artifacts=("build" "dist" "distro" "target" "node_modules")
  local projects_dir="${HOME}/Projects"

  if [ ! -d "$projects_dir" ]; then
    echo "Error: ${projects_dir} does not exist"
    return 1
  fi

  for p in "$projects_dir"/*; do
    if [ -d "$p" ]; then
      if [ -d "$p/.git" ]; then
        main_branch_name=$(git -C "$p" rev-parse --abbrev-ref origin/HEAD 2>/dev/null | cut -d '/' -f 2)

        if [ -z "$main_branch_name" ]; then
          echo "Error: Failed to determine main branch for $(basename "$p") - invalid or missing origin/HEAD"
          continue
        fi

        if [[ -n $(git -C "$p" status --porcelain) ]]; then
          echo "$(basename "$p"): Local changes, not pulling ${main_branch_name}"
        else
          current_branch_name=$(git -C "$p" rev-parse --abbrev-ref HEAD)
          if [[ "$current_branch_name" != "$main_branch_name" ]]; then
            git -C "$p" checkout "${main_branch_name}"
          fi

          remote_main_branch_name=$(git -C "$p" ls-remote --symref origin HEAD | awk '$1 == "ref:" {print $2}' | cut -d '/' -f 3)
          if [[ "$main_branch_name" != "$remote_main_branch_name" ]]; then
            git -C "$p" branch -m "$main_branch_name" "$remote_main_branch_name"
          fi

          git -C "${p}" fetch origin

          if git -C "${p}" status --porcelain -b | grep -q "behind"; then
            echo "$(basename "${p}"): Remote changes detected, pulling ${main_branch_name}"
            git -C "${p}" pull
          else
            echo "$(basename "${p}"): No remote changes for ${main_branch_name}, skipping pull"
          fi
        fi

        if git -C "$p" show-ref -q --verify refs/heads/release; then
          echo "Deleting local release branch in $(basename "$p")"
          git -C "$p" branch -D release
        fi
      fi

      for artifact in "${artifacts[@]}"; do
        if [ -d "$p/$artifact" ]; then
          echo "Deleting ${artifact} folder in $(basename "$p")"
          rm -rf "$p/$artifact"
        fi
      done
    fi
  done
}

function gForceSsh {
    git config --global url."git@github.com:".insteadOf "https://github.com/"
}

function gsearch {
  git log -g --grep="$1"
}

function gau_gitlab() {
  p=$(pwd | sed -e s%$HOME/Projects/%%)
  d=${p%/*}
  u=$(git config --get remote.origin.url | sed -e s%Todd.Ornett%${d:l}% -e "s%[ ]%-%g")
  git remote add upstream $u
}

function gau_bitbucket() {
  u=$(git config --get remote.origin.url | sed -e s%~todd.ornett/sj/%% -e "s%[ ]%-%g")
  git remote add upstream $u
}

function grename() {
  if [[ -z "$1" || -z "$2" ]]; then
    echo "Usage: $0 old_branch new_branch"
    return 1
  fi

  # Rename branch locally
  git branch -m "$1" "$2"
  # Rename branch in origin remote
  if git push origin :"$1"; then
    git push --set-upstream origin "$2"
  fi
}

function forceSsh() {
  git config --global url."git@github.com:".insteadOf "https://github.com/"
}

function gnewproj {
  if [ -z "$1" ]; then
    echo "Usage: $(basename $0) <project directory path>"
    return 1
  fi

  if [ -d "$HOME/Projects/$1" ]; then
    echo "$1 already exists, cannot create new git project"
    return 1
  fi

  mkdir "$HOME/Projects/$1"
  cd "$HOME/Projects/$1"

  echo "# $1" >> README.md
  git init
  git add README.md
  git commit -m "first commit"
  git branch -M main
  git remote add origin "git@github.com:toddaornett/$1.git"
  git push -u origin main
}

#####
# gh command helpers
#####
function ghcl() {
  gh repo clone $(gh search repos $1 | head -1 | cut -f 1)
}

function ghs() {
  gh search repos $1
}

unset git_version
