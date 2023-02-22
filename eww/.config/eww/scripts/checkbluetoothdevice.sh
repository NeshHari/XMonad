#!/usr/bin/env bash
bluetoothctl devices | cut -f2 -d' ' | while read uuid; do bluetoothctl info $uuid; done | grep -e "Name\|Connected: yes" | grep -B1 "yes" | head -n 1 | cut -d\  -f2-
