#!/usr/bin/env bash
brew install git ripgrep coreutils fd
xcode-select --install
brew tap d12frosted/emacs-plus
brew reinstall emacs-plus@30
ln -sfn $HOMEBREW/opt/emacs-plus/Emacs.app /Applications/Emacs.app
