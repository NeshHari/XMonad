#!/usr/bin/env bash

if [[ $1 == "--kitty" ]]; then
	cd
	kitty
fi

if [[ $1 == "--spotify" ]]; then
	~/.config/polybar/scripts/poly_spoti.sh
fi

if [[ $1 == "--git" ]]; then
	brave-nightly -e "https://github.com/NeshHari/XMonad#xmonad-starter-kit"
fi

if [[ $1 == "--edge" ]]; then
	microsoft-edge-beta
fi

if [[ $1 == "--files" ]]; then
	thunar
fi

if [[ $1 == "--discord" ]]; then
	discord
fi

if [[ $1 == "--brave" ]]; then
	brave-nightly
fi
