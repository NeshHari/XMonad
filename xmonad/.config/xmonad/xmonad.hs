-- import modules in ./lib/Custom/
import Custom.MyCatppuccin
import Custom.MyDecorations (myBorderWidth, myFocusedBorderColor, myNormalBorderColor)
import Custom.MyKeys
import Custom.MyLayouts (myLayoutHook)
import Custom.MyManagement
import Custom.MyMouse (myMouseBindings)
import Custom.MyPolybar (myPolybar)
import Custom.MyScreen (rescreenCfg)
import Custom.MyStartupApps (myStartupHook)
import Custom.MyWorkspaces (myWorkspaces)
-- given modules from xmonad and xmonad-contrib
import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (docks)
import XMonad.Hooks.OnPropertyChange (onXPropertyChange)
import XMonad.Hooks.Rescreen (rescreenHook)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.WindowSwallowing (swallowEventHook)
import XMonad.Util.EZConfig
import XMonad.Util.Hacks as Hacks
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)
import XMonad.Util.WorkspaceCompare

myEventHook = swallowEventHook (className =? "kitty") (return True) <> onXPropertyChange "WM_NAME" myManageHook <> Hacks.windowedFullscreenFixEventHook

main :: IO ()
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
