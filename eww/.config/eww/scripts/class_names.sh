#!/usr/bin/env bash
#Author: github.com/NeshHari

# Listen for changes to the WM_CLASS property of the focused window
xprop -root -spy _NET_ACTIVE_WINDOW | while read line; do
	# Get the current window ID and WM_CLASS
	curr_wid=$(echo "$line" | awk '{print $5}')
	curr_class=$(xprop -id "$curr_wid" WM_CLASS | grep -o '".*"' | cut -d '"' -f 2)
	curr_class=$(echo $curr_class | sed 's/-/ /g' | sed -E 's/(^| )([a-z])/\1\u\2/g')

	# Check if the focus has changed
	if [[ "$prev_wid" != "$curr_wid" || "$prev_class" != "$curr_class" ]]; then

		# Echo the new WM_CLASS
		echo "(box :class \"class-names\" :halign \"center\" :orientation \"h\" :space-evenly \"false\" (label :class \"class-names\" :text \"$curr_class\" :angle \"90\" :show-truncated \"false\" :limit-width \"13\"))"

		# Update the previous window ID and WM_CLASS
		prev_wid="$curr_wid"
		prev_class="$curr_class"
	fi
done
