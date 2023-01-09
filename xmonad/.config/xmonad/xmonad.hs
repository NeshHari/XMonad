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
import XMonad.Util.NamedScratchpad (namedScratchpadAction)

myTerminal = "kitty --single-instance"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myModMask = mod4Mask

myBorderWidth = 3

myNormalBorderColor = catMantle

myFocusedBorderColor = catMauve

myDynamicManageHook :: ManageHook
myDynamicManageHook =
  composeAll
    [ className =? "epicgameslauncher.exe" --> doFloat,
      className =? "redlauncher.exe" --> doFloat,
      className =? "witcher3.exe" --> doFloat
    ]

myManageHook = namedScratchpadManageHook myScratchpads <> myDynamicManageHook

myLogHook = return ()

myEventHook = swallowEventHook (className =? "kitty") (return True) <> onXPropertyChange "WM_NAME" myDynamicManageHook <> Hacks.windowedFullscreenFixEventHook

main :: IO ()
main =
  do
    xmonad
    $ disableEwmhManageDesktopViewport
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
        manageHook = myManageHook,
        handleEventHook = myEventHook,
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
