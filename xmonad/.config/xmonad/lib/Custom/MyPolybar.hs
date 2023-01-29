module Custom.MyPolybar where

import XMonad (spawn)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

myPolybar :: StatusBarConfig
myPolybar =
  def
    { sbLogHook =
        xmonadPropLog
          =<< dynamicLogString polybarPP,
      sbStartupHook = spawn "~/.config/polybar/launch.sh",
      sbCleanupHook = spawn "killall polybar"
    }

polybarPP :: PP
polybarPP =
  def
    { ppCurrent = textColor "" . wrap "" "",
      ppOrder = \(_ : l : _ : _) -> [l]
    }

textColor :: String -> String -> String
textColor color = wrap ("%{F" <> color <> "}") " %{F-}"
