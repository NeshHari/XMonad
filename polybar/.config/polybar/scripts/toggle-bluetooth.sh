#!/usr/bin/env bash
if [ $(bluetoothctl show | grep "Powered: yes" | wc -c) -eq 0 ]; then
	bluetoothctl power on
	sleep 1
	bluetooth-autoconnect -d
else
	bluetoothctl power off
fi
