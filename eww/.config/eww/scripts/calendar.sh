#!/usr/bin/env bash

CACHE="$HOME/.cache/eww_temp"

if [[ ! $(pidof eww) ]]; then
	eww daemon
	sleep 1
fi

run() {
	eww open mycal
}

if [[ ! -f "$CACHE" ]]; then
	touch "$CACHE"
	run
else
	eww close mycal
	rm "$CACHE"
fi
