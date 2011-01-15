#!/usr/bin/env bash
# slim uses exec /bin/bash -login ~/.xinitrc %session
# so bash is started as a non interactive login shell.
# bash will only search for ~/.bash_profile, ~/.bash_login, or ~/.profile
# The interpreter of this script is then a non interactive shell,
# reading only $ZDOTDIR/.zshenv (if zsh) or $BASH_ENV (if bash)


session=${1:-xmonad}
# before starting xmonad, we want a correct X db and a running urxvt daemon
xrdb -merge ~/.Xresources
urxvtd -q -f -o
# if nitrogen/conky are launched after, the status bar won't start...
nitrogen --restore # wait for the wp before lauching conky
conky &

case $session in
  fluxbox)
    startfluxbox & wmpid=$!
    ;;
  xmonad)
    xsetroot -cursor_name left_ptr &
    FIFO=~/.xmonad/output
    [ ! -p ${FIFO} ] && mkfifo ${FIFO}
    statusbar start
    xmonad> ${FIFO} & wmpid=$!
    ;;
esac

xscreensaver -no-splash &

# terminus is in /usr/share/fonts/local/
[ -d /usr/share/fonts/local/ ] && xset +fp /usr/share/fonts/local/ && xset fp rehash

wait $wmpid # like exec myWM but more flexible
