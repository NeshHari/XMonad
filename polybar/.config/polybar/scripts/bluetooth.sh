#!/usr/bin/env bash

if bluetoothctl show | grep -q "Powered: yes" && bluetoothctl info | grep -q 'Device'; then
	echo "󰂰"
else
	echo "󰂲"
fi
