#!/usr/bin/env bash
brew install autoconf
brew install automake
brew install cmake
brew install coreutils
brew install dbus
brew install expat
brew install gcc
brew install gmp
brew install gnu-sed
brew install gnutls
brew install jansson
brew install libffi
brew install libgccjit
brew install libiconv
brew install librsvg
brew install libtasn1
brew install libunistring
brew install libxml2
brew install little-cms2
brew install mailutils
brew install make
brew install ncurses
brew install nettle
brew install pkg-config
brew install sqlite
brew install texinfo
brew install tree-sitter
brew install zlib
brew install libvterm
cd ~/Projects
git clone https://github.com/emacs-mirror/emacs.git
cd emacs
autoreconf -isvf
./configure --with-native-compilation --prefix=/opt/emacs --disable-ns-self-contained
make -j10
make check
