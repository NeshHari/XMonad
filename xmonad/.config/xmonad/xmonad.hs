-- import modules in ./lib/Custom/
import Custom.MyCatppuccin
import Custom.MyKeys
import Custom.MyLayouts
import Custom.MyManagement
import Custom.MyMouse
import Custom.MyScratchpads
import Custom.MyStartupApps
import Custom.MyWorkspaces
-- given modules from xmonad and xmonad-contrib
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

-- reduce startup time by using single sprite cache
myTerminal = "kitty --single-instance"

myFocusFollowsMouse = True

myModMask = mod4Mask

myBorderWidth = 3

myNormalBorderColor = catMantle

myFocusedBorderColor = catMauve

myLogHook = return ()

myEventHook = swallowEventHook (className =? "kitty") (return True) <> onXPropertyChange "WM_NAME" myManageHook <> Hacks.windowedFullscreenFixEventHook

main =
  do
    xmonad
    {- force XMonad to *not* set _NET_DESKTOP_VIEWPORT
    available in latest commit cf13f8f (https://github.com/xmonad/xmonad-contrib/commit/cf13f8f9)
    correct polybar order on dual monitors -}
    $ disableEwmhManageDesktopViewport
    $ Hacks.javaHack
    $ withSB myPolybar
    $ docks
      -- . ewmhFullscreen
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

myPolybar =
  def
    { -- for defining what to log
      sbLogHook =
        xmonadPropLog
          =<< dynamicLogString def,
      -- lower polybar for monacle layout
      sbStartupHook = spawn "~/.config/polybar/startup.sh; sleep 1; xdo lower -N 'Polybar'",
      sbCleanupHook = spawn "killall -q polybar"
    }
