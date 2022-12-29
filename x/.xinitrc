#!/bin/sh

xmodmap ~/.Xmodmap &

xset r rate 300 50 &
setxkbmap -option "caps:ctrl_modifier" &
xcape -e 'Caps_Lock=Escape' &

exec awesome
