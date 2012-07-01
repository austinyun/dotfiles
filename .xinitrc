#!/bin/sh

setxkbmap -option ctrl:nocaps
xsetroot -cursor_name left_ptr &
xrdb -merge ~/.Xresources
feh --bg-tile ~/Pictures/defaultdesktop_solarized.png
xscreensaver -no-splash &
exec ck-launch-session dbus-launch --sh-syntax --exit-with-session xmonad
