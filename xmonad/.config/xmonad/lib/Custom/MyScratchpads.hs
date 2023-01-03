module Custom.MyScratchpads where

import XMonad
import XMonad.StackSet qualified as W
import XMonad.Util.NamedScratchpad

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "quick commands" spawnQc findQc manageQc,
    NS "pavucontrol" spawnPavu findPavu managePavu
  ]
  where
    -- emulate "drop-down behavior"
    spawnQc = "alacritty"
    findQc = resource =? "Alacritty"
    manageQc = customFloating $ W.RationalRect 0 0 1 1

    spawnPavu = "pavucontrol"
    findPavu = resource =? "pavucontrol"
    managePavu = customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)
