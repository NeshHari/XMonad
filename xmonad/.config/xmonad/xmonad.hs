import Custom.MyCatppuccin
import Custom.MyKeys
import Custom.MyLayouts
import Custom.MyMouse
import Custom.MyScratchpads
import Custom.MyStartupApps
import Custom.MyWorkspaces
import Data.Map qualified as M
import Data.Monoid
import System.Exit
import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.OnPropertyChange
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing (swallowEventHook)
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig
import XMonad.Util.Hacks as Hacks
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce

myTerminal = "kitty --single-instance"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth = 3

myModMask = mod4Mask

myNormalBorderColor = catMantle

myFocusedBorderColor = catMauve

myDynamicManageHook :: ManageHook
myDynamicManageHook =
  composeAll
    []

myLogHook = return ()

main :: IO ()
main =
  do
    xmonad
    $ Hacks.javaHack
    $ withSB myPolybarConf
    $ docks
      {-       . ewmhFullscreen -}
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
        manageHook = namedScratchpadManageHook myScratchpads,
        handleEventHook = swallowEventHook (className =? "kitty") (return True) <> onXPropertyChange "WM_NAME" myDynamicManageHook <> Hacks.windowedFullscreenFixEventHook,
        logHook = myLogHook,
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
