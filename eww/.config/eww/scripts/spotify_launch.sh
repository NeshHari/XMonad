#!/usr/bin/env bash

# Launch Spotify
spotify &

# Run eww command to show Spotify
eww open --toggle spotify_standalone

# Wait for Spotify to close
wait $!

# Run eww command to hide Spotify
eww close spotify_standalone
eww close spotify
