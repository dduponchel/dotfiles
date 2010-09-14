#!/usr/bin/env bash

xrdb -merge ~/.Xresources
xscreensaver -no-splash &

# bar :    |--   xmonad bar    --||-- conky bar --||-trayer-|
# lenght : | X percent of screen ||  what's left  || fixed  |

screen_width=$(xrandr | grep '*'|head -n 1|sed -r "s/[[:space:]]*//;s/x.*//")

systray_width_param=5
# 1 "trayer width" ~= 10px
systray_width=$(($systray_width_param * 10))

wm_bar_ratio_percent=50
wm_bar_width=$(($screen_width * $wm_bar_ratio_percent / 100))

status_bar_width=$(($screen_width - $wm_bar_width - $systray_width))


session=${1:-xmonad}

case $session in
fluxbox)
	ck-launch-session&
	startfluxbox & wmpid=$!
	;;
xmonad)
	xsetroot -cursor_name left_ptr &
	trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width $systray_width_param --height 18 --transparent true --tint 0x000000 &
	ck-launch-session&
	xmonad|dzen2 -ta l -w ${wm_bar_width} & wmpid=$!
	conky -c ~/.conkystatusbarrc|zsh|dzen2 -ta r -w $status_bar_width -x $wm_bar_width &
	;;
esac

conky &
nitrogen --restore &

# terminus is in /usr/share/fonts/local/
[ -d /usr/share/fonts/local/ ] && xset +fp /usr/share/fonts/local/ && xset fp rehash

wait $wmpid # like exec myWM but more flexible
