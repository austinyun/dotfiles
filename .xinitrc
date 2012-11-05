#!/bin/sh

setxkbmap -option ctrl:nocaps
xsetroot -cursor_name left_ptr &
xrdb -merge ~/.Xresources
exec dbus-launch --sh-syntax --exit-with-session xmonad
