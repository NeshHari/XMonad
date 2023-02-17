#!/usr/bin/env bash

if [[ $1 == "--kitty" ]]; then
	cd
	kitty
fi

if [[ $1 == "--spotify" ]]; then
	~/.config/polybar/scripts/poly_spoti.sh
fi

if [[ $1 == "--git" ]]; then
	microsoft-edge-beta -e "https://github.com/NeshHari/XMonad#xmonad-starter-kit"
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
