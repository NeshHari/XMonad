module Custom.MyStartupApps where

import XMonad
import XMonad.Util.SpawnOnce

myWallpaperPath :: String
myWallpaperPath = "~/wallpapers/digital_blend.jpg"

myStartupHook :: X ()
myStartupHook = do
  let wallpaperCmd = "feh --bg-scale " ++ myWallpaperPath
      blurCmd = "~/scripts/feh-blur.sh -s; ~/scripts/feh-blur.sh -d"
      picomCmd = "killall -9 picom; sleep 2 && picom -b &"
      easyeffectsCmd = "easyeffects --gapplication-service &"
  sequence_ [spawn wallpaperCmd, spawn blurCmd, spawn picomCmd, spawnOnce easyeffectsCmd]
