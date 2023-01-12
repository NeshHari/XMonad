module Custom.MyManagement where

import Custom.MyScratchpads
import XMonad
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Util.NamedScratchpad

myManagement =
  composeAll
    [ className =? "witcher3.exe" --> doCenterFloat,
      className =? "witcher3.exe" --> doShift "games",
      className =? "steam_app_0" --> doShift "games"
    ]

myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook myScratchpads <> myManagement
