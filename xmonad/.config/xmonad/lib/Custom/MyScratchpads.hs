module Custom.MyScratchpads where

import Custom.MyManagementPositioning
import XMonad (appName)
import XMonad.ManageHook ((=?))
import XMonad.Util.NamedScratchpad

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "quick commands" spawnQc findQc myCenter,
    NS "glava" spawnGl findGl myCenterSmall
  ]
  where
    {-
    To get WM_CLASS of a visible window, run "xprop | grep 'CLASS'" and select the window.
    appName :: Query StringSource
    Return the application name; i.e., the first string returned by WM_CLASS.

    resource :: Query StringSource
    Backwards compatible alias for appName.

    className :: Query StringSource
    Return the resource class; i.e., the second string returned by WM_CLASS. -}
    spawnQc = "alacritty -e fish"
    findQc = appName =? "Alacritty"

    spawnGl = "glava"
    findGl = appName =? "GLava"
