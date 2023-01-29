#!/usr/bin/env bash

spotify &
pid_spotify=$!

polybar -c ~/.config/polybar/TheDarkArts/config.ini -r main-middle &
pid_polybar=$!

wait $pid_spotify

kill $pid_polybar
