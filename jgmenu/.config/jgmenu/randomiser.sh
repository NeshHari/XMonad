#!/usr/bin/env bash
# Author: github.com/NeshHari

file_path="$HOME/.config/jgmenu/jgmenurc"

colors=("#d27e99" "#e6c384" "#97bb6c" "#7fb4ca" "#957fb8" "#ffa066" "#9cabca" "#ff5d62" "#a3d4d5")

# Get the last color used from the file
last_color=$(grep -oP 'color_sel_bg\s*=\s*#\K[0-9a-f]{6}\s*100' "$file_path" | tail -n 1 | cut -d ' ' -f 1)

# Choose a new color that is different from the last one
while true; do
	new_color=${colors[RANDOM % ${#colors[@]}]}
	if [[ $new_color != $last_color ]]; then
		break
	fi
done

# Replace the color_sel_bg value with the new color
sed -i -E "s/color_sel_bg\s*=\s*#[0-9a-f]{6}\s*100/color_sel_bg = ${new_color} 100/" "$file_path"
