module Custom.MyManagement where

import Custom.MyScratchpads
import XMonad
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Util.NamedScratchpad

myManagement =
  composeAll
    [ (className =? "witcher3.exe" <&&> className =? "steam_app_0") --> doCenterFloat,
      (className =? "xdg-desktop-portal-gnome") --> doCenterFloat
    ]

myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook myScratchpads <> myManagement
