module Custom.MyLayouts where

import Custom.MyCatppuccin
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Column
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerScreen
import XMonad.Layout.Renamed as XLR
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation

mySpacing i = spacingRaw False (Border 10 10 30 30) True (Border i i i i) True

myShowWNameConfig =
  def
    { swn_font = "xft:Sugar Snow:size=60",
      swn_color = catFlamingo,
      swn_bgcolor = catBase,
      swn_fade = 0.8
    }

myTabConfig =
  def
    { activeColor = catGreen,
      inactiveColor = catBase,
      urgentColor = catRed,
      activeBorderColor = catBase,
      inactiveBorderColor = catBase,
      urgentBorderColor = catRed,
      activeTextColor = catBase,
      inactiveTextColor = catFlamingo,
      urgentTextColor = catBase,
      fontName = "xft:Sugar Snow:size=12"
    }

tabs =
  renamed [XLR.Replace "Tabs"] $
    avoidStruts $
      tabbed
        shrinkText
        myTabConfig

tall =
  renamed [XLR.Replace "Tall"] $
    avoidStruts $
      windowNavigation $
        addTabs shrinkText myTabConfig $
          subLayout [] tabs $
            mySpacing 7 $
              ResizableTall nmaster delta ratio []
  where
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

column =
  renamed [XLR.Replace "Column"] $
    avoidStruts $
      windowNavigation $
        addTabs shrinkText myTabConfig $
          subLayout [] tabs $
            mySpacing 7 $
              Column 1.0

full = renamed [XLR.Replace "Monocle"] $ noBorders Full

myLayout = ifWider 1080 tall column ||| full

myLayoutHook =
  showWName' myShowWNameConfig $
    smartBorders $
      mkToggle
        (NOBORDERS ?? FULL ?? EOT)
        myLayout
