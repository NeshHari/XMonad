#!/usr/bin/env bash
# Author: github.com/NeshHari

if [[ $(bluetoothctl show | awk '/Powered:/ {print $2}') == "no" ]]; then
	echo "OFF"
elif [[ $(bluetoothctl info | grep -e "Name" -e "Connected: yes") == *"Connected: yes"* ]]; then
	echo $(bluetoothctl info | grep -e "Name" -e "Connected: yes" | grep -B1 "yes" | head -n 1 | cut -d\  -f2-)
else
	echo "DISCONNECTED"
fi
#
