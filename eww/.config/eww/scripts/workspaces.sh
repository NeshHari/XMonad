#!/usr/bin/env bash
# Author: github.com/NeshHari
# inspired by: https://github.com/rxyhn
workspaces() {
	ws1=1
	ws2=2
	ws3=3
	ws4=4
	ws5=5

	# target workspace class prefix (eww.scss)
	wsc="wsc"

	# Check if workspace is occupied and return workspace number
	o1=$(wmctrl -l | awk '{print $2}' | sort | uniq | grep 0)
	o2=$(wmctrl -l | awk '{print $2}' | sort | uniq | grep 1)
	o3=$(wmctrl -l | awk '{print $2}' | sort | uniq | grep 2)
	o4=$(wmctrl -l | awk '{print $2}' | sort | uniq | grep 3)
	o5=$(wmctrl -l | awk '{print $2}' | sort | uniq | grep 4)

	# Check if workspace is focused and return 0 (false) or 1 (true)
	f1=$(xprop -root _NET_CURRENT_DESKTOP | grep -oP '\d+' | grep -c 0)
	f2=$(xprop -root _NET_CURRENT_DESKTOP | grep -oP '\d+' | grep -c 1)
	f3=$(xprop -root _NET_CURRENT_DESKTOP | grep -oP '\d+' | grep -c 2)
	f4=$(xprop -root _NET_CURRENT_DESKTOP | grep -oP '\d+' | grep -c 3)
	f5=$(xprop -root _NET_CURRENT_DESKTOP | grep -oP '\d+' | grep -c 4)

	# Output
	echo "(box :class \"workspace-container\" :orientation \"v\" :space-evenly \"false\" (button :onclick \"wmctrl -s 0\" :class \"$wsc$o1$f1\" \"\") (button :onclick \"wmctrl -s 1\" :class \"$wsc$o2$f2\" \"\") (button :onclick \"wmctrl -s 2\" :class \"$wsc$o3$f3\" \"\") (button :onclick \"wmctrl -s 3\" :class \"$wsc$o4$f4\" \"\") (button :onclick \"wmctrl -s 4\" :class \"$wsc$o5$f5\" \"\"))"
}

xprop -spy -root _NET_CURRENT_DESKTOP | while read -r _; do
	workspaces
done
