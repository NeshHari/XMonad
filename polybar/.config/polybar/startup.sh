#!/usr/bin/env bash
#
export DEFAULT_NETWORK_INTERFACE=$(ip route | grep '^default' | awk '{print $5}')

notify-send -u low " Rescreening in progress ";

killall -q polybar;

while pgrep -x polybar >/dev/null; do sleep 1; done;

if autorandr --detected | grep "single"; then
    polybar main &
fi
if autorandr --detected | grep "dual"; then
    polybar main &
    polybar left &
fi
sleep 1;
xdo lower -N "Polybar"
