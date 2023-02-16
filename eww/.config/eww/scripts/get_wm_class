#!/usr/bin/env bash
output=$(xprop -id $(xdotool getwindowfocus) WM_CLASS | grep -o '".*"' | cut -d '"' -f 2)
if [[ $output == steam_app_* ]]; then
	output=$(xprop -id $(xdotool getwindowfocus) WM_NAME | grep -o '".*"' | cut -d '"' -f 2)
fi
output=$(echo $output | sed 's/-/ /g' | sed -E 's/(^| )([a-z])/\1\u\2/g')
if [ -z "$output" ]; then
	echo "Desktop"
else
	echo $output
fi
