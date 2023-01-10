module Custom.MyManagement where

import Custom.MyScratchpads
import XMonad
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Util.NamedScratchpad

myManagement =
  composeAll
    [ (className =? "epicgameslauncher.exe" <&&> className =? "redlauncher.exe" <&&> className =? "witcher3.exe") --> doCenterFloat,
      (className =? "epicgameslauncher.exe" <&&> className =? "redlauncher.exe" <&&> className =? "witcher3.exe") --> doShift "games"
    ]

myManageHook = namedScratchpadManageHook myScratchpads <> myManagement
