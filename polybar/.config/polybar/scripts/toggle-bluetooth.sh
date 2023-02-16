#!/usr/bin/env bash
if [ $(bluetoothctl show | grep "Powered: yes" | wc -c) -eq 0 ]; then
	bluetoothctl power on
	notify-send "Bluetooth Powered ON"
	sleep 1
	bluetooth-autoconnect -d
else
	notify-send "Bluetooth Powered OFF"
	bluetoothctl power off
fi
