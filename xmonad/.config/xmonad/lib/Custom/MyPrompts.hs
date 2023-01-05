module Custom.MyPrompts where

import Custom.MyCatppuccin
import XMonad.Prompt

myPromptConfig =
  def
    { historySize = 0,
      position = Top,
      promptBorderWidth = 0,
      defaultText = "",
      alwaysHighlight = True,
      height = 50,
      font = "xft:Sugar Snow:style=Regular:size=12",
      bgColor = catBase,
      fgColor = catLavender,
      bgHLight = catLavender,
      fgHLight = catBase,
      autoComplete = Nothing,
      showCompletionOnTab = False
    }
