#!/usr/bin/env bash

killall -9 eww
eww daemon
sleep 1

eww open-many showlayout showfocusedclass powertoggler datetoggler
