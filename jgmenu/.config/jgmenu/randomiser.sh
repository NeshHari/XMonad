#!/usr/bin/env bash
# Author: github.com/NeshHari

file_path="$HOME/.config/jgmenu/jgmenurc"

colors=("#f38ba8" "#f9e2af" "#a6e3a1" "#89b4fa" "#cba6f7" "#fab387" "#b4befe" "#eba0ac")

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
