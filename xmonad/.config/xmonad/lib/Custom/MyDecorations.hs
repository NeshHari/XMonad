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
      fgColor = springViolet2,
      bgHLight = springViolet2,
      fgHLight = sumiInk0,
      historySize = 0,
      position = Top,
      borderColor = sumiInk0,
      promptBorderWidth = 0,
      defaultText = "",
      alwaysHighlight = True,
      height = 55,
      font = "xft:Vanilla Caramel:style=Regular:size=12",
      autoComplete = Nothing,
      showCompletionOnTab = False
    }

myShowWNameConfig :: SWNConfig
myShowWNameConfig =
  def
    { swn_font = "xft:Vanilla Caramel:size=60",
      swn_color = springViolet2,
      swn_bgcolor = sumiInk0,
      swn_fade = 0.8
    }

myTabConfig :: Theme
myTabConfig =
  def
    { activeColor = springViolet2,
      inactiveColor = sumiInk0,
      urgentColor = peachRed,
      activeBorderColor = sumiInk0,
      inactiveBorderColor = sumiInk0,
      urgentBorderColor = peachRed,
      activeTextColor = sumiInk0,
      inactiveTextColor = roninYellow,
      urgentTextColor = sumiInk0,
      fontName = "xft:Vanilla Caramel:size=12"
    }

emConf :: EasyMotionConfig
emConf =
  def
    { txtCol = springViolet2,
      bgCol = sumiInk0,
      borderCol = sumiInk0,
      cancelKey = xK_Escape,
      emFont = "xft: Vanilla Caramel-60",
      overlayF = textSize,
      borderPx = 30
    }
