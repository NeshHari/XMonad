module Custom.MyDecorations where

import Custom.MyCatppuccin
import XMonad.Layout.ShowWName
import XMonad.Layout.Tabbed
import XMonad.Prompt

myPromptConfig :: XPConfig
myPromptConfig =
  def
    { bgColor = catBase,
      fgColor = catLavender,
      bgHLight = catLavender,
      fgHLight = catBase,
      historySize = 0,
      position = Top,
      borderColor = catBase,
      promptBorderWidth = 0,
      defaultText = "",
      alwaysHighlight = True,
      height = 55,
      font = "xft:Sugar Snow:style=Regular:size=12",
      autoComplete = Nothing,
      showCompletionOnTab = False
    }

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
