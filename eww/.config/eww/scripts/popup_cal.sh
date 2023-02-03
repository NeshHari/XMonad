#!/usr/bin/env bash

calendar() {
	LOCK_FILE="$HOME/.cache/eww-cal.lock"

	run() {
		eww open calendar
	}

	if [[ ! $(pidof eww) ]]; then
		eww daemon
		sleep 1
	fi

	if [[ ! -f "$LOCK_FILE" ]]; then
		touch "$LOCK_FILE"
		run
	else
		eww close calendar
		killall calendar
		rm "$LOCK_FILE"
	fi
}

if [ "$1" = "calendar" ]; then
	calendar
fi
