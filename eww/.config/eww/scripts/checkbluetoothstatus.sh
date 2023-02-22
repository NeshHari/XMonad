#!/usr/bin/env bash
if [ $(bluetoothctl show | grep "Powered: yes" | wc -c) -eq 0 ]; then
	echo "OFF"
else
	echo "ON"
fi
