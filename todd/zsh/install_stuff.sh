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

brew bundle

echo "export CPPFLAGS=\"-I$(brew --prefix)/opt/libpq/include\"" | cat - $HOME/.zprofile | sponge $HOME/.zprofile
echo "export LDFLAGS=\"-L$(brew --prefix)/opt/libpq/lib\"" | cat - $HOME/.zprofile | sponge $HOME/.zprofile

# setup terminfo if needed and launch tmux
# (for one thing, this was needed to make my backspace key work)
if [ ! -f $(brew --prefix)/opt/ncurses/bin/infocmp ] || [ ! $(brew --prefix)/opt/ncurses/bin/infocmp tmux-256color &>/dev/null ]
then
  $(brew --prefix)/opt/ncurses/bin/infocmp tmux-256color > /tmp/tmux-256color
  tic -xe tmux-256color /tmp/tmux-256color
  rm /tmp/tmux-256color
fi

##########
# hammerspoon config
##########
defaults write org.hammerspoon.Hammerspoon MJConfigFile "~/.config/todd/hammerspoon/init.lua"
