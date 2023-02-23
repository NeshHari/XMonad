#!/usr/bin/env bash
# Author: github.com/NeshHari

packages=$(paru -Qu | cut -d' ' -f1)

update_all_option="Update all"
selected_packages=$(echo -e "$update_all_option\n${packages}" | rofi -dmenu -i -multi-select -markup-rows -matching fuzzy -p "Select packages to update:")

if [ -z "${selected_packages}" ]; then
	exit 0
fi

if [[ "$selected_packages" == *"$update_all_option"* ]]; then
	update_cmd="paru -S --noconfirm $(echo ${packages})"
else
	update_cmd="paru -S --noconfirm $(echo ${selected_packages})"
fi

echo "Updating packages: ${selected_packages}"
alacritty -e fish -c "${update_cmd}"
