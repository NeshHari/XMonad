#!/usr/bin/env python3
# Author: github.com/NeshHari

import subprocess
import sys

PLAYER = "playerctld"
FORMAT = "{{ title }}"

try:
    status = subprocess.check_output(
        f"playerctl --player={PLAYER} status 2>/dev/null", shell=True, text=True
    ).strip()
except subprocess.CalledProcessError:
    status = "Nothing is playing"

if len(sys.argv) >= 2:
    if sys.argv[1] == "--status":
        if status == "Stopped":
            print("Nothing is playing")
        elif status == "Paused":
            print(
                subprocess.check_output(
                    f"playerctl --player={PLAYER} metadata --format '{FORMAT}'",
                    shell=True,
                    text=True,
                ).strip()
            )
        elif status == "Nothing is playing":
            print(status)
        else:
            print(
                subprocess.check_output(
                    f"playerctl --player={PLAYER} metadata --format '{FORMAT}'",
                    shell=True,
                    text=True,
                ).strip()
            )
    elif sys.argv[1] == "--icon":
        if status == "Playing":
            print("")
        else:
            print("")
else:
    if status == "Stopped":
        print("Nothing is playing")
    elif status == "Paused":
        print(
            subprocess.check_output(
                f"playerctl --player={PLAYER} metadata --format '{FORMAT}'",
                shell=True,
                text=True,
            ).strip()
        )
    else:
        print(
            subprocess.check_output(
                f"playerctl --player={PLAYER} metadata --format '{FORMAT}'",
                shell=True,
                text=True,
            ).strip()
        )
