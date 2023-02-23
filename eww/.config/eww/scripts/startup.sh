#!/usr/bin/env bash

modules=("bar" "showvolume" "quotetoggler" "mynotifications")

killall -9 eww
rm ~/.cache/eww*
pkill -f "logger.py init"

eww daemon
sleep 1

cd ~/.config/eww && python ./scripts/logger.py init &

for module in "${modules[@]}"; do
	eww open "$module"
done

sleep 3

for module in "${modules[@]}"; do
	xdo lower -n "eww-$module"
done
