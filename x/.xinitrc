#!/bin/sh

xmodmap ~/.Xmodmap
xset r rate 300 50
setxkbmap -option "caps:ctrl_modifier"
xcape -e 'Caps_Lock=Escape'

xsetroot -cursor_name left_ptr 

~/scripts/.fix-wifi.sh

export _JAVA_AWT_WM_NONREPARENTING=1

/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

systemctl --user import-environment DISPLAY XAUTHORITY;
dbus-update-activation-environment --systemd --all
dbus-launch --exit-with-session ~/.local/bin/xmonad
