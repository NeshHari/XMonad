module Custom.MyDecorations where

import Custom.MyKanagawa
import XMonad (xK_Escape)
import XMonad qualified
import XMonad.Actions.EasyMotion
import XMonad.Layout.ShowWName
import XMonad.Layout.Tabbed
import XMonad.Prompt

myBorderWidth :: XMonad.Dimension
myBorderWidth = 2

myNormalBorderColor :: String
myNormalBorderColor = sumiInk0

myFocusedBorderColor :: String
myFocusedBorderColor = crystalBlue

myPromptConfig :: XPConfig
myPromptConfig =
  def
    { bgColor = sumiInk0,
      fgColor = oldWhite,
      bgHLight = oldWhite,
      fgHLight = sumiInk0,
      historySize = 0,
      position = Top,
      borderColor = sumiInk0,
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
      swn_color = oldWhite,
      swn_bgcolor = sumiInk0,
      swn_fade = 0.8
    }

myTabConfig :: Theme
myTabConfig =
  def
    { activeColor = oldWhite,
      inactiveColor = sumiInk0,
      urgentColor = peachRed,
      activeBorderColor = sumiInk0,
      inactiveBorderColor = sumiInk0,
      urgentBorderColor = peachRed,
      activeTextColor = sumiInk0,
      inactiveTextColor = roninYellow,
      urgentTextColor = sumiInk0,
      fontName = "xft:FiraCode Nerd Font:size=12"
    }

emConf :: EasyMotionConfig
emConf =
  def
    { txtCol = oldWhite,
      bgCol = sumiInk0,
      borderCol = sumiInk0,
      cancelKey = xK_Escape,
      emFont = "xft: FiraCode Nerd Font-60",
      overlayF = textSize,
      borderPx = 30
    }
