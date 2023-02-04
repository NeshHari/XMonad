#!/usr/bin/env bash

CACHE="$HOME/.cache/eww_temp2"

if [[ ! $(pidof eww) ]]; then
	eww daemon
	sleep 1
fi

run() {
	eww open profile
}

if [[ ! -f "$CACHE" ]]; then
	touch "$CACHE"
	run
else
	eww close profile
	rm "$CACHE"
fi
