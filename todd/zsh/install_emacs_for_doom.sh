#!/usr/bin/env bash
brew tap railwaycat/emacsmacport
brew reinstall emacs-mac --with-dbus\
   --with-modules\
   --with-starter\
   --with-imagemagick\
   --with-no-title-bars\
   --with-emacs-icons-project-EmacsIcon7\
   --with-native-comp\
   --with-mac-metal\
   --with-xwidgets\
  --with-tree-sitter
ln -sfn $HOMEBREW/opt/emacs-mac/Emacs.app /Applications/Emacs.app
