#!/bin/sh

xmodmap ~/.Xmodmap &

xset r rate 300 50 &
setxkbmap -option "caps:ctrl_modifier" &
xcape -e 'Caps_Lock=Escape' &

xsetroot -cursor_name left_ptr 

dunst &

easyeffects --gapplication-service &

~/.screenlayout/pure_landscape.sh &

exec ~/.local/bin/xmonad
