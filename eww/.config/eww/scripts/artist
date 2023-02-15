#!/usr/bin/env python3
# Author: github.com/NeshHari

import subprocess
import sys

PLAYER = "playerctld"
FORMAT = "{{ artist }}"

try:
    status = subprocess.check_output(
        f"playerctl --player={PLAYER} status 2>/dev/null", shell=True, text=True
    ).strip()
except subprocess.CalledProcessError:
    status = "~"

if status == "Stopped":
    print("~")
elif status == "Paused":
    print(
        subprocess.check_output(
            f"playerctl --player={PLAYER} metadata --format '{FORMAT}'",
            shell=True,
            text=True,
        ).strip()
    )
elif status == "~":
    print(status)
else:
    print(
        subprocess.check_output(
            f"playerctl --player={PLAYER} metadata --format '{FORMAT}'",
            shell=True,
            text=True,
        ).strip()
    )
