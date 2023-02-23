#!/usr/bin/env bash

modules=("bar" "showvolume" "quotetoggler" "mynotifications")

killall -9 eww
rm ~/.cache/eww*

eww daemon
for module in "${modules[@]}"; do
	eww open "$module"
done

sleep 3

for module in "${modules[@]}"; do
	xdo lower -n "eww-$module"
done
