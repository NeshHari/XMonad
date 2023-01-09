#!/usr/bin/env sh

# kill existing
killall polybar

while pgrep -x polybar >/dev/null; do sleep 1; done

main=HDMI-0
left=DP-1

if xrandr | grep "$main connected" && xrandr | grep "$left disconnected"; then
    polybar main &
fi

if xrandr | grep "$main connected" && xrandr | grep "$left connected"; then
    polybar main &
    polybar left &
fi

# might not execute xdo lower without sleep
sleep 1

#lower polybar for monocle layout
xdo lower -N "Polybar"
