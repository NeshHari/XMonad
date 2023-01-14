module Custom.MyPrompts where

import Custom.MyCatppuccin
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
