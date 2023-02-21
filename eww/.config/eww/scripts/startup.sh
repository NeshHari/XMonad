#!/usr/bin/env bash

killall -9 eww
rm ~/.cache/eww*
eww daemon
eww open bar
xdo lower -n bar
