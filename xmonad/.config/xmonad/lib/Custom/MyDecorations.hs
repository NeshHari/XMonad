module Custom.MyDecorations where

import Custom.MyCatppuccin
import XMonad (xK_Escape)
import XMonad qualified
import XMonad.Actions.EasyMotion
import XMonad.Layout.ShowWName
import XMonad.Layout.Tabbed
import XMonad.Prompt

myBorderWidth :: XMonad.Dimension
myBorderWidth = 2

myNormalBorderColor :: String
myNormalBorderColor = catCrust

myFocusedBorderColor :: String
myFocusedBorderColor = catText

myPromptConfig :: XPConfig
myPromptConfig =
  def
    { bgColor = catCrust,
      fgColor = catText,
      bgHLight = catText,
      fgHLight = catCrust,
      historySize = 0,
      position = Top,
      borderColor = catCrust,
      promptBorderWidth = 0,
      defaultText = "",
      alwaysHighlight = True,
      height = 60,
      font = "xft:FiraCode Nerd Font:style=Regular:size=12",
      autoComplete = Nothing,
      showCompletionOnTab = False
    }

myShowWNameConfig :: SWNConfig
myShowWNameConfig =
  def
    { swn_font = "xft:Vanilla Caramel:size=60",
      swn_color = catText,
      swn_bgcolor = catCrust,
      swn_fade = 0.8
    }

myTabConfig :: Theme
myTabConfig =
  def
    { activeColor = catText,
      inactiveColor = catCrust,
      urgentColor = catRed,
      activeBorderColor = catCrust,
      inactiveBorderColor = catCrust,
      urgentBorderColor = catRed,
      activeTextColor = catCrust,
      inactiveTextColor = catFlamingo,
      urgentTextColor = catCrust,
      fontName = "xft:FiraCode Nerd Font:size=12"
    }

emConf :: EasyMotionConfig
emConf =
  def
    { txtCol = catCrust,
      bgCol = catText,
      borderCol = catText,
      cancelKey = xK_Escape,
      emFont = "xft:Vanilla Caramel:size=60",
      overlayF = textSize,
      borderPx = 30
    }
