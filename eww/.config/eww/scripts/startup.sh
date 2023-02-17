#!/usr/bin/env bash

killall -9 eww
rm ~/.cache/eww*
sleep 0.5

eww daemon
sleep 1

eww open-many quotetoggler showlayout showfocusedclass datetoggler sysdatatoggler powertoggler
