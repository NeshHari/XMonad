module Custom.MyStartupApps where

import XMonad
import XMonad.Util.SpawnOnce

myStartupHook :: X ()
myStartupHook = do
--  spawn "~/scripts/screenlayout/switch.sh"
  spawn "feh --bg-scale ~/wallpapers/stains_of_purple.jpg"
  spawn "~/scripts/feh-blur.sh -s; ~/scripts/feh-blur.sh -d"
  spawnOnce "xmodmap ~/.Xmodmap"
  spawnOnce "dunst &"
  spawn "killall picom; picom -b"
  spawnOnce "easyeffects --gapplication-service &"
