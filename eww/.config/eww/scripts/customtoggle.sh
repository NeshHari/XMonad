#!/usr/bin/env bash
# Author: github.com/NeshHari

# Define the list of modules to close
modules=(applications showquote showfocusedclass spotify mycal showsysdata powermenu bluetooth notifications)

# Get the argument passed to the script
module="$1"

# Check if the module is valid
if [[ " ${modules[@]} " =~ " ${module} " ]]; then
	# Check if the module is already open, and if so, close it
	if eww query -m "$module" >/dev/null 2>&1; then
		eww close "$module"
	fi

	# Close all modules
	for mod in "${modules[@]}"; do
		if [[ "$mod" != "$module" ]]; then
			eww close "$mod"
		fi
	done

	# Open the specified module
	eww open --toggle "$module"
else
	exit 1
fi
