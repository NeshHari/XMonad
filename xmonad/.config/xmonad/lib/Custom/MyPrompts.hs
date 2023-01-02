module Custom.MyPrompts where

import XMonad.Prompt

myPromptConfig =
  def
    { position = Top,
      promptBorderWidth = 0,
      defaultText = "",
      alwaysHighlight = True,
      height = 50,
      font = "xft:CaskaydiaCove Nerd Font:style=Regular:size=12",
      bgColor = "#1e1e2e",
      fgColor = "#f5e0dc",
      bgHLight = "#eba0ac",
      fgHLight = "#1e1e2e",
      autoComplete = Nothing,
      showCompletionOnTab = False,
      historySize = 0
    }

