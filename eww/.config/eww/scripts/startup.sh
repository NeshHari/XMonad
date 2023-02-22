#!/usr/bin/env bash

killall -9 eww
rm ~/.cache/eww*
eww daemon
eww open bar
eww open showvolume
eww open mynotifications
sleep 3
xdo lower -n eww-bar
xdo lower -n eww-showvolume
xdo lower -n eww-mynotifications
