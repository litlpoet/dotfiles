export BROWSER=/usr/bin/firefox
export EDITOR=/usr/bin/vim
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export QT_QPA_PLATFORMTHEME="qt5ct"
export XMODIFIERS=@im=ibus

ibus-daemon -drx
