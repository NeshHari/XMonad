#!/usr/bin/env bash

spotify &
pid_spotify=$!

THEDARKARTS="$HOME/.config/polybar/TheDarkArts/config.ini"
THESTAINSOFPURPLE="$HOME/.config/polybar/TheStainsOfPurple/config.ini"
EWWIFICATION="$HOME/.config/polybar/Ewwification/config.ini"

PATH_TO_CONFIG=$EWWIFICATION

polybar -c $PATH_TO_CONFIG -r spotify &
pid_polybar=$!

wait $pid_spotify

kill $pid_polybar
