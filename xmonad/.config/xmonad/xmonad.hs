import XMonad

main :: IO ()
main = do
  xmonad $
    def
      { terminal = "kitty --single-instance",
        modMask = mod4Mask
      }
