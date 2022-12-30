import XMonad

main :: IO ()
main = do
  xmonad $
    def
      { terminal = "kitty"
      }
