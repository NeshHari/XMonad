#!/usr/bin/env bash

# Author: github.com/NeshHari

file_path="$HOME/.config/rofi/themes/custom.rasi"

colors=("#f38ba8" "#f9e2af" "#a6e3a1" "#89b4fa" "#cba6f7" "#fab387" "#b4befe" "#eba0ac" "#f5e0dc" "#89dceb" "#cdd6f4")

last_color=$(grep -oP 'randomcolor:\s*#\K[0-9a-f]{6}(?=;)' "$file_path" | tail -n 1)

while true; do
	new_color=${colors[RANDOM % ${#colors[@]}]}
	if [[ $new_color != $last_color ]]; then
		break
	fi
done

sed -i -E "s/randomcolor:\s*#[0-9a-f]{6};/randomcolor: ${new_color};/" "$file_path"
