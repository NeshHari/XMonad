-- import modules in ./lib/Custom/
import Custom.MyCatppuccin
import Custom.MyDecorations (myBorderWidth, myFocusedBorderColor, myNormalBorderColor)
import Custom.MyKeys
import Custom.MyLayouts
import Custom.MyManagement
import Custom.MyMouse
import Custom.MyPolybar
import Custom.MyScratchpads
import Custom.MyScreen
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
import XMonad.Hooks.Rescreen
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing (swallowEventHook)
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig
import XMonad.Util.Hacks as Hacks
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare

myEventHook = swallowEventHook (className =? "kitty") (return True) <> onXPropertyChange "WM_NAME" myManageHook <> Hacks.windowedFullscreenFixEventHook

main =
  do
    xmonad
    $ Hacks.javaHack
      . rescreenHook rescreenCfg
    $ withSB myPolybar
    $ docks
    -- . ewmhFullscreen
    $ addEwmhWorkspaceSort (pure (filterOutWs [scratchpadWorkspaceTag]))
      {- force XMonad to *not* set _NET_DESKTOP_VIEWPORT available since commit cf13f8f (https://github.com/xmonad/xmonad-contrib/commit/cf13f8f9)
       - correct polybar order on dual monitors -}
      . disableEwmhManageDesktopViewport
      . ewmh
    $ def
      { terminal = "kitty --single instance",
        focusFollowsMouse = True,
        borderWidth = myBorderWidth,
        modMask = mod4Mask,
        workspaces = myWorkspaces,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys = myAdditionalKeys,
        mouseBindings = myMouseBindings,
        layoutHook = myLayoutHook,
        manageHook = myManageHook,
        handleEventHook = myEventHook,
        logHook = return (),
        startupHook = myStartupHook
      }
      `additionalKeysP` myKeys
