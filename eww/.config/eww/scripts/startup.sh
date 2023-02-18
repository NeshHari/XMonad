#!/usr/bin/env bash

killall -9 eww
rm ~/.cache/eww*
sleep 0.5

eww daemon
sleep 1

eww open-many quotetoggler showlayout showfocusedclass datetoggler sysdatatoggler powertoggler
sleep 1
xdo lower -n eww-quotetoggler
xdo lower -n eww-showlayout
xdo lower -n eww-showfocusedclass
xdo lower -n eww-datetoggler
xdo lower -n eww-sysdatatoggler
xdo lower -n eww-powertoggler
