module Custom.MyScreen where

import XMonad
import XMonad.Hooks.Rescreen

myAfterRescreenHook :: X ()
myAfterRescreenHook = spawn "sleep 1; xmonad --restart"

myRandrChangeHook :: X ()
myRandrChangeHook = spawn "autorandr --change"

rescreenCfg :: RescreenConfig
rescreenCfg =
  def
    { afterRescreenHook = myAfterRescreenHook,
      randrChangeHook = myRandrChangeHook
    }
