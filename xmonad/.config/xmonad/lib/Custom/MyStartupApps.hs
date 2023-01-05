module Custom.MyStartupApps where

import XMonad
import XMonad.Hooks.SetWMName  
import XMonad.Util.SpawnOnce

myStartupHook :: X ()
myStartupHook = do
  spawn "~/.screenlayout/pure_landscape.sh"
  spawn "~/.fehbg"
  spawnOnce "xmodmap ~/.Xmodmap"
  spawn "killall picom; picom -b"
  spawn "~/feh-blur.sh -s; ~/feh-blur.sh -d"
  spawn "easyeffects --gapplication-service &"
  setWMName "LG3D"
