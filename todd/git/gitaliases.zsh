# Git version checking
autoload -Uz is-at-least
git_version="${${(As: :)$(git version 2>/dev/null)}[3]}"

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
compdef _git _git_log_prettily=git-log

# Warn if the current branch is a WIP
function work_in_progress() {
  if $(git log -n 1 2>/dev/null | grep -q -c "\-\-wip\-\-"); then
    echo "WIP!!"
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

#
# Aliases
#

alias g='git'

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
alias gcp='git cherry-pick'
alias gcpa='git cherry-pick --abort'
alias gcpc='git cherry-pick --continue'
alias gcs='git commit -S'

alias gd='git diff'
alias gdca='git diff --cached'
alias gdcw='git diff --cached --word-diff'
alias gdct='git describe --tags $(git rev-list --tags --max-count=1)'
alias gds='git diff --staged'
alias gdt='git diff-tree --no-commit-id --name-only -r'
alias gdw='git diff --word-diff'

alias mycommits='git log --pretty=format:"%h%x09%an%x09%ad%x09%s" 2>&1 | grep Todd | grep -v Merge'
alias teamcommits='git log --pretty=format:"%h%x09%an%x09%ad%x09%s" 2>&1 | grep -i takumi | grep -v Merge'

function gdnolock() {
  git diff "$@" ":(exclude)package-lock.json" ":(exclude)*.lock"
}
compdef _git gdnolock=git-diff

function gdv() { git diff -w "$@" | view - }
compdef _git gdv=git-diff

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
compdef _git ggf=git-checkout
function ggfl() {
  [[ "$#" != 1 ]] && local b="$(git_current_branch)"
  git push --force-with-lease origin "${b:=$1}"
}
compdef _git ggfl=git-checkout

function ggp() {
  if [[ "$#" != 0 ]] && [[ "$#" != 1 ]]; then
    git pull origin "${*}"
  else
    [[ "$#" == 0 ]] && local b="$(git_current_branch)"
    git pull origin "${b:=$1}"
  fi
}
compdef _git ggp=git-checkout

function ggP() {
  if [[ "$#" != 0 ]] && [[ "$#" != 1 ]]; then
    git push origin "${*}"
  else
    [[ "$#" == 0 ]] && local b="$(git_current_branch)"
    git push origin "${b:=$1}"
  fi
}
compdef _git ggP=git-checkout

function ggpnp() {
  if [[ "$#" == 0 ]]; then
    ggp && ggP
  else
    ggp "${*}" && ggP "${*}"
  fi
}
compdef _git ggpnp=git-checkout

function ggu() {
  [[ "$#" != 1 ]] && local b="$(git_current_branch)"
  git pull --rebase origin "${b:=$1}"
}
compdef _git ggu=git-checkout

alias ggpur='ggu'
alias ggpull='git pull origin "$(git_current_branch)"'
alias ggpush='git push origin "$(git_current_branch)"'

alias ggsup='git branch --set-upstream-to=origin/$(git_current_branch)'

alias ghh='git help'

alias gignore='git update-index --assume-unchanged'
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
alias gsh='git show'
alias gsi='git submodule init'
alias gsps='git show --pretty=short --show-signature'
alias gsr='git svn rebase'
alias gss='git status -s'
alias gs='git status'

# use the default stash push on git 2.13 and newer
#is-at-least 2.13 "$git_version" \
#  && alias gsta='git stash push -u' \
#  || alias gsta='git stash save -u'

alias gstC='git stash clear'
alias gstl='git stash list'
alias gsts='git stash show --text'
alias gstu='git stash --include-untracked'
alias gstall='git stash --all'
alias gsu='git submodule update'
alias gsw='git switch'
alias gswc='git switch -c'

alias gtD='gtD(){ git tag -d "${1}" && git push --delete origin refs/tags/"${1}" && git push --delete upstream refs/tags/"${1}" }; noglob gtD'
alias gts='git tag -s'
alias gtv='git tag | sort -V'
alias gtl='gtl(){ git tag --sort=-v:refname -n -l "${1}*" }; noglob gtl'

alias gunignore='git update-index --no-assume-unchanged'
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
