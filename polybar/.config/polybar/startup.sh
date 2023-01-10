#!/usr/bin/env sh

while pgrep -x polybar >/dev/null; do sleep 1; done

main=HDMI-0
left=DP-1

if xrandr | grep "$main connected"; then
    polybar main
fi

if xrandr | grep "$left connected"; then
    polybar left
fi
