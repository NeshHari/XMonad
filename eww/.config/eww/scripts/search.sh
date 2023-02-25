#!/usr/bin/env bash
# Author: github.com/NeshHari

ddg="https://duckduckgo.com/?q="
google="https://www.google.com/search?q="
wiki="https://en.wikipedia.org/w/index.php?title=Special:Search&search="
bing="https://www.bing.com/search?q="
arch="https://wiki.archlinux.org/index.php?search="
aur="https://aur.archlinux.org/packages/?O=0&K="
hackage="https://hackage.haskell.org/packages/search?terms="
so="https://stackoverflow.com/search?q="
reddit="https://www.reddit.com/search/?q="
protondb="https://www.protondb.com/search?q="
steamdb="https://steamdb.info/search/?a=app&q="

back="<-- Change search engine"

while true; do
	engine=$(echo -e "DuckDuckGo\nGoogle\nWikipedia\nBing\nArchWiki\nAUR\nHackage\nStack Overflow\nReddit\nProtonDB\nSteamDB" | rofi -dmenu -i -matching fuzzy -p "Select search engine:")
	if [ -z "$engine" ]; then exit 0; fi
	if [ "$engine" == "$back" ]; then continue; fi

	query=""
	while true; do
		query=$(echo -e "$back" | rofi -dmenu -p "Search on $engine:")
		if [ "$query" == "$back" ]; then break; fi
		if [ -z "$query" ]; then continue; fi

		case $engine in
		"DuckDuckGo")
			xdg-open "$ddg$query"
			exit
			;;
		"Google")
			xdg-open "$google$query"
			exit
			;;
		"Wikipedia")
			xdg-open "$wiki$query"
			exit
			;;
		"Bing")
			xdg-open "$bing$query"
			exit
			;;
		"ArchWiki")
			xdg-open "$arch$query"
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
			xdg-open "$so$query"
			exit
			;;
		"Reddit")
			xdg-open "$reddit$query"
			exit
			;;
		"ProtonDB")
			xdg-open "$protondb$query"
			exit
			;;
		"SteamDB")
			xdg-open "$steamdb$query"
			exit
			;;
		esac
	done
done
