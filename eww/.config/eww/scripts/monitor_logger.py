#!/usr/bin/env python

import subprocess
import os


def launch_logger():
    logger_path = os.path.join("scripts", "logger.py")
    try:
        # Check if logger.py is already running
        subprocess.check_output(["pidof", "-x", "python", logger_path])
        print("logger.py is already running")
    except subprocess.CalledProcessError:
        # logger.py is not running, launch it
        subprocess.Popen(["python", logger_path, "init", "&"])
        print("logger.py has been launched")


if __name__ == "__main__":
    launch_logger()
