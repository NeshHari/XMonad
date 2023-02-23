#!/usr/bin/env bash
# Author: github.com/NeshHari

duckduckgo="https://duckduckgo.com/?q="
google="https://www.google.com/search?q="
wikipedia="https://en.wikipedia.org/w/index.php?title=Special:Search&search="
bing="https://www.bing.com/search?q="
archwiki="https://wiki.archlinux.org/index.php?search="
aur="https://aur.archlinux.org/packages/?O=0&K="
hackage="https://hackage.haskell.org/packages/search?terms="
stackoverflow="https://stackoverflow.com/search?q="

back_button="<-- Change search engine"

while true; do
	engine=$(echo -e "DuckDuckGo\nGoogle\nWikipedia\nBing\nArchWiki\nAUR\nHackage\nStack Overflow" | rofi -dmenu -i -matching fuzzy -p "Select search engine:")

	if [ -z "$engine" ]; then
		exit 0
	fi

	if [ "$engine" == "$back_button" ]; then
		continue
	fi

	query=""
	while true; do
		query=$(echo -e "$back_button" | rofi -dmenu -p "Search on $engine:")

		if [ "$query" == "$back_button" ]; then
			break
		fi

		if [ -z "$query" ]; then
			continue
		fi

		case $engine in
		"DuckDuckGo")
			xdg-open "$duckduckgo$query"
			exit
			;;
		"Google")
			xdg-open "$google$query"
			exit
			;;
		"Wikipedia")
			xdg-open "$wikipedia$query"
			exit
			;;
		"Bing")
			xdg-open "$bing$query"
			exit
			;;
		"ArchWiki")
			xdg-open "$archwiki$query"
			exit
			;;
		"AUR")
			xdg-open "$aur$query"
			exit
			;;
		"Hackage")
			xdg-open "$hackage$query"
			exit
			;;
		"Stack Overflow")
			xdg-open "$stackoverflow$query"
			exit
			;;
		esac
	done
done
