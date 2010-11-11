#!/usr/bin/env bash
# slim uses exec /bin/bash -login ~/.xinitrc %session
# so bash is started as a non interactive login shell.
# bash will only search for ~/.bash_profile, ~/.bash_login, or ~/.profile
# The interpreter of this script is then a non interactive shell,
# reading only $ZDOTDIR/.zshenv (if zsh) or $BASH_ENV (if bash)


xrdb -merge ~/.Xresources
xscreensaver -no-splash &
session=${1:-xmonad}

case $session in
  fluxbox)
    ck-launch-session&
    startfluxbox & wmpid=$!
    ;;
  xmonad)
    xsetroot -cursor_name left_ptr &
    ck-launch-session&
    FIFO=~/.xmonad/output
    [ ! -p ${FIFO} ] && mkfifo ${FIFO}
    statusbar start
    xmonad> ${FIFO} & wmpid=$!
    ;;
esac

conky &
nitrogen --restore &

# terminus is in /usr/share/fonts/local/
[ -d /usr/share/fonts/local/ ] && xset +fp /usr/share/fonts/local/ && xset fp rehash

wait $wmpid # like exec myWM but more flexible
