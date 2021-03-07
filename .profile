#!/bin/bash

export BROWSER=/usr/bin/google-chrome-stable
export EDITOR=/usr/bin/vim
export GPG_TTY=$(tty)
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export QT_QPA_PLATFORMTHEME="qt5ct"
export XDG_CONFIG_HOME=$HOME/.config
export XMODIFIERS=@im=ibus

ibus-daemon -drx &
