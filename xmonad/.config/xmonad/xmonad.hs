import Custom.MyKeys
import Custom.MyLayouts
import Custom.MyMouse
import Custom.MyWorkspaces
import Data.Map qualified as M
import Data.Monoid
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.OnPropertyChange
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing (swallowEventHook)
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

myTerminal = "kitty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth = 3

myModMask = mod4Mask

myNormalBorderColor = "#1e1e2e"

myFocusedBorderColor = "#cba6f7"

myManageHook =
  composeAll
    [ className =? "MPlayer" --> doFloat,
      className =? "Gimp" --> doFloat,
      resource =? "desktop_window" --> doIgnore,
      resource =? "kdesktop" --> doIgnore
    ]

myStartupHook :: X ()
myStartupHook = do
  spawn "~/.fehbg"
  spawnOnce "xmodmap ~/.Xmodmap"
  spawn "killall picom; picom -b"
  spawn "~/feh-blur.sh -s; ~/feh-blur.sh -d"
  spawn "easyeffects --gapplication-service &"

myDynamicManageHook :: ManageHook
myDynamicManageHook =
  composeAll
    []

main :: IO ()
main =
  do
    xmonad
    $ withSB myPolybarConf
    $ docks
      . ewmhFullscreen
      . ewmh
    $ def
      { terminal = myTerminal,
        focusFollowsMouse = myFocusFollowsMouse,
        borderWidth = myBorderWidth,
        modMask = myModMask,
        workspaces = myWorkspaces,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys = myAdditionalKeys,
        mouseBindings = myMouseBindings,
        layoutHook = myLayoutHook,
        manageHook = myManageHook,
        handleEventHook = swallowEventHook (className =? "kitty") (return True) <> onXPropertyChange "WM_NAME" myDynamicManageHook,
        logHook = dynamicLog,
        startupHook = myStartupHook
      }
      `additionalKeysP` myKeys

myPolybarConf =
  def
    { sbLogHook =
        xmonadPropLog
          =<< dynamicLogString polybarPPdef,
      sbStartupHook = spawn "~/.config/polybar/startup.sh",
      sbCleanupHook = spawn "killall polybar"
    }

polybarPPdef =
  def
