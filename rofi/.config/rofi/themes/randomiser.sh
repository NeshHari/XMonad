#!/usr/bin/env bash

# Author: github.com/NeshHari

file_path="$HOME/.config/rofi/themes/custom.rasi"

colors=("#d27e99" "#e6c384" "#97bb6c" "#7fb4ca" "#957fb8" "#ffa066" "#9cabca" "#ff5d62" "#a3d4d5")

last_color=$(grep -oP 'randomcolor:\s*#\K[0-9a-f]{6}(?=;)' "$file_path" | tail -n 1)

while true; do
	new_color=${colors[RANDOM % ${#colors[@]}]}
	if [[ $new_color != $last_color ]]; then
		break
	fi
done

sed -i -E "s/randomcolor:\s*#[0-9a-f]{6};/randomcolor: ${new_color};/" "$file_path"

sed -i -E "s/fg-col2:\s*#[0-9a-f]{6};/fg-col2: ${new_color};/" "$file_path"
