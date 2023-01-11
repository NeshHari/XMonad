module Custom.MyStartupApps where

import XMonad
import XMonad.Util.SpawnOnce

myStartupHook :: X ()
myStartupHook = do
  spawn "~/scripts/screenlayout/switch.sh"
  spawn "~/.feh --bg-scale ~/wallpapers/stains_of_purple.jpg"
  spawnOnce "xmodmap ~/.Xmodmap"
  spawnOnce "dunst &"
  spawnOnce "picom -b"
  spawnOnce "easyeffects --gapplication-service &"
