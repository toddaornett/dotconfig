#!/usr/bin/env sh

##########
# install the missing package manager brew if missing
##########
if ! type brew &>/dev/null
then
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  echo 'path=("/opt/homebrew/bin" $path)' >> $HOME/.zprofile
  path=("/opt/homebrew/bin" $path)
fi

##########
# install the fast node manager
##########
if ! type fnm &>/dev/null
then
  brew install fnm
fi
eval "$(fnm env --use-on-cd --version-file-strategy=recursive)"

##########
# terminal (alacritty and tmux)
##########

# Nerd Fonts
if ! brew list | grep nerd-font &>/dev/null
then
  brew tap homebrew/cask-fonts
  brew search '/font-.*-nerd-font/' | awk '{ print $1 }' | xargs -I{} brew install --cask {} || true
fi

# emacs key bindings
bindkey -e

# setup terminfo if needed and launch tmux
# (for one thing, this was needed to make my backspace key work)
if [ ! -f $(brew --prefix)/opt/ncurses/bin/infocmp ] || [ ! $(brew --prefix)/opt/ncurses/bin/infocmp tmux-256color &>/dev/null ]
then
  if ! brew list | grep ncurses
  then
    brew install ncurses
  fi
  $(brew --prefix)/opt/ncurses/bin/infocmp tmux-256color > /tmp/tmux-256color
  tic -xe tmux-256color /tmp/tmux-256color
  rm /tmp/tmux-256color
fi
if ! command -v tmux &>/dev/null
then
  brew install tmux
fi

##########
# hammerspoon config
##########
defaults write org.hammerspoon.Hammerspoon MJConfigFile "~/.config/todd/hammerspoon/init.lua"

##########
# enhance cd and use zoxide with fzf
##########
setopt auto_cd
if ! type zoxide >/dev/null
then
  brew install zoxide
  rm ~/.zcompdump*; compinit
fi
if [ ! -d $(brew --prefix)/opt/zsh-autocomplete ]
then
  brew install zsh-autocomplete
fi

##########
# rust
##########
if ! command -v rustup | grep rustup &>/dev/null
then
  brew install rustup
  rustup-init
  rustc --version
fi

#########
# node.js
##########
if ! command -v fnm >/dev/null
then
  brew install fnm
fi
eval $(fnm env)

##########
# install marksman lsp for markdown
##########
if ! type marksman &>/dev/null
then
  brew install marksman
fi

##########
# spaceship prompt
##########
if ! brew list spaceship &>/dev/null
then
  brew install spaceship
fi

##########
# install ripgrep
##########
if ! type rg &>/dev/null
then
  brew install rg
fi

##########
# install fd
##########
if ! type fd &>/dev/null
then
  brew install fd
fi

##########
# install coreutils
##########
if [ ! -d $(brew --prefix)/opt/coreutils ]
then
  brew install coreutils
fi

##########
# install doom emacs and some dependent external tools
# see also: https://github.com/doomemacs/doomemacs
#   https://github.com/d12frosted/homebrew-emacs-plus
#   https://emacsformacosx.com/
#   https://github.com/jimeh/build-emacs-for-macos
##########
if ! type shfmt &>/dev/null
then
  brew install shfmt
fi
if ! type shellcheck &>/dev/null
then
  brew install shellcheck
fi
if ! type editorconfig &>/dev/null
then
  brew install editorconfig
fi
if ! type doom &>/dev/null
then
  echo 'path=("$HOME/.config/emacs/bin" $path)' >> $HOME/.zprofile
fi
if ! brew list | grep libvterm &>/dev/null
then
  brew install libvterm
fi
if [ ! -d ~/.config/emacs ]
then
  git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.config/emacs
  $HOME/.config/emacs/bin/doom install
  $HOME/.config/emacs/bin/doom sync
fi

##########
# install moreutils which includes sponge command
##########
if ! type sponge &>/dev/null
then
  brew install moreutils
fi

##########
# install libpq
##########
if [ ! -d $(brew --prefix)/opt/libpq ]
then
  brew install libpq
  echo "export CPPFLAGS=\"-I$(brew --prefix)/opt/libpq/include\"" | cat - $HOME/.zprofile | sponge $HOME/.zprofile
  echo "export LDFLAGS=\"-L$(brew --prefix)/opt/libpq/lib\"" | cat - $HOME/.zprofile | sponge $HOME/.zprofile
fi

##########
# install lazygit
##########
if ! type lazygit &>/dev/null
then
  brew install jesseduffield/lazygit/lazygit
  brew install lazygit
fi

##########
# install and configure direnv
##########
if ! type direnv &>/dev/null
then
  brew install direnv
fi
