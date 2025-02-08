#!/usr/bin/env bash
cd $OPENPROJECTS_PATH
git clone https://github.com/emacs-mirror/emacs.git
cd emacs
./autogen.sh
./configure --prefix=/opt/emacs \
            --with-native-compilation \
            --with-json \
            --with-ns \
            --with-xwidgets \
            --without-dbus \
            --without-compress-install \
            --disable-silent-rules
make
make install
mv nextstep/Emacs.app /Applications/
