#!/bin/sh

unclutter &
dunst &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

xset r rate 350 35
xsetroot -cursor_name left_ptr

[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
xcape -e 'Control_L=Escape'

systemctl --user import-environment DISPLAY XAUTHORITY
dbus-update-activation-environment --systemd --all
dbus-launch --exit-with-session ~/.local/bin/xmonad
