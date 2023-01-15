{-# LANGUAGE ImportQualifiedPost #-}

-- separated module from Custom.MyManagement to prevent mutually recursive modules
module Custom.MyManagementPositioning where

import XMonad
import XMonad.StackSet qualified as W
import XMonad.Util.NamedScratchpad

myCenter :: ManageHook
myCenter = customFloating $ W.RationalRect fromLeft fromTop width height
  where
    width = 1 / 2
    height = 1 / 2
    fromLeft = (1 - width) / 2
    fromTop = (1 - height) / 2

myCenterSmall :: ManageHook
myCenterSmall = customFloating $ W.RationalRect fromLeft fromTop width height
  where
    width = 1 / 3
    height = 1 / 3
    fromLeft = (1 - width) / 2
    fromTop = (1 - height) / 2
