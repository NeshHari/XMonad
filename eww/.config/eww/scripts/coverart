#!/usr/bin/env python3
# Author: github.com/NeshHari

import os
import sys
import subprocess

if len(sys.argv) >= 2:
    if sys.argv[1] == "--art":
        tmp_dir = os.path.expanduser("~/.config/eww/images")
        tmp_temp_path = os.path.join(tmp_dir, "temp.png")

        if not os.path.exists(tmp_dir):
            os.makedirs(tmp_dir)

        artlink = subprocess.check_output(["playerctl", "-p", "spotify,mpd", "metadata", "mpris:artUrl"]).decode().strip()

        if artlink:
            subprocess.run(["curl", "-s", artlink, "--output", tmp_temp_path], check=True)
            print(tmp_temp_path)
        else:
            print("images/default.png")
    elif sys.argv[1] == "--player-name":
        player = subprocess.check_output(["playerctl", "--list-all"]).decode().strip().split("\n")[0].title()
        print(player)

