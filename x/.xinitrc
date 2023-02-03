#!/usr/bin/env bash

unclutter &
dunst &
openrazer-daemon &
eww daemon &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

xset r rate 350 35
xsetroot -cursor_name left_ptr

[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
xcape -e 'Control_L=Escape' -t 1000

systemctl --user import-environment DISPLAY XAUTHORITY
dbus-update-activation-environment --systemd --all
dbus-launch --exit-with-session ~/.local/bin/xmonad
