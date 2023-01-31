module Custom.MyStartupApps where

import XMonad
import XMonad.Util.SpawnOnce

myStartupHook :: X ()
myStartupHook = do
  spawn "feh --bg-scale ~/wallpapers/dark_arts_custom.png"
  spawn "~/scripts/feh-blur.sh -s; ~/scripts/feh-blur.sh -d"
  spawn "killall picom; picom -b"
  spawnOnce "easyeffects --gapplication-service &"
