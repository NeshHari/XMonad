module Custom.MyLayouts where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Column
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders
import XMonad.Layout.PerScreen
import XMonad.Layout.Renamed as XLR
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.MultiToggle.Instances

mySpacing i = spacingRaw False (Border 10 10 30 30) True (Border i i i i) True

tall =
  renamed [XLR.Replace "Tall"] $
    avoidStruts $
      mySpacing 3 $
        ResizableTall nmaster delta ratio []
  where
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

column =
  renamed [XLR.Replace "Column"] $
    avoidStruts $
      mySpacing 3 $
        Column 1.0

full = renamed [XLR.Replace "Monocle"] $ noBorders Full

myLayout = ifWider 1080 tall column ||| full

myLayoutHook =
  smartBorders
    $ mkToggle
      (NOBORDERS ?? FULL ?? EOT)
      myLayout
