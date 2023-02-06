#!/usr/bin/env bash
export DEFAULT_NETWORK_INTERFACE=$(ip route | grep '^default' | awk '{print $5}')
notify-send -u low " Rescreening in progress "
killall -q polybar
while pgrep -x polybar >/dev/null; do sleep 1; done

THEDARKARTS="$HOME/.config/polybar/TheDarkArts/config.ini"
THESTAINSOFPURPLE="$HOME/.config/polybar/TheStainsOfPurple/config.ini"

PATH_TO_CONFIG=$THEDARKARTS

if [ "$PATH_TO_CONFIG" = "$THESTAINSOFPURPLE" ]; then
	if autorandr --detected | grep -q "single"; then
		polybar -c $PATH_TO_CONFIG main &
	elif autorandr --detected | grep -q "dual"; then
		polybar -c $PATH_TO_CONFIG main &
		polybar -c $PATH_TO_CONFIG left &
	fi
elif [ "$PATH_TO_CONFIG" = "$THEDARKARTS" ]; then
	if autorandr --detected | grep -q "single"; then
		polybar -c $PATH_TO_CONFIG main-left &
		polybar -c $PATH_TO_CONFIG main-left-extended &
		polybar -c $PATH_TO_CONFIG main-left-links &
		# polybar -c $PATH_TO_CONFIG main-middle &
		polybar -c $PATH_TO_CONFIG main-right &
		polybar -c $PATH_TO_CONFIG main-right-extended &
		polybar -c $PATH_TO_CONFIG main-tray &
		polybar -c $PATH_TO_CONFIG main-profile &
	elif autorandr --detected | grep -q "dual"; then
		polybar -c $PATH_TO_CONFIG main-left &
		polybar -c $PATH_TO_CONFIG main-left-extended &
		polybar -c $PATH_TO_CONFIG main-left-links &
		# polybar -c $PATH_TO_CONFIG main-middle &
		polybar -c $PATH_TO_CONFIG main-right &
		polybar -c $PATH_TO_CONFIG main-right-extended &
		polybar -c $PATH_TO_CONFIG main-tray &
		polybar -c $PATH_TO_CONFIG left &
		polybar -c $PATH_TO_CONFIG main-profile &
	fi
else
	notify-send -u low "Failed to launch polybar."
fi

# adjust sleep duration as required to ensure polybar is hidden (i.e., lowered) after restart in monocle layout
sleep 3
xdo lower -N "Polybar"
