module Custom.MyKeys where

import Custom.MyPrompts
import Custom.MyScratchpads
import Custom.MyWorkspaces
import Data.Map qualified as M
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Search qualified as S
import XMonad.Actions.Submap qualified as SM
import XMonad.Actions.WithAll
import XMonad.Hooks.ManageDocks
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

myKeys :: [(String, X ())]
myKeys =
  [ -- Terminal
    ("M-<Return>", spawn "kitty"),
    -- Browser
    ("M-b", spawn "microsoft-edge-stable"),
    -- Rofi
    ("M-p", spawn "rofi -show drun"),
    -- XPrompts
    ("M-S-p m", manPrompt myPromptConfig),
    ("M-S-p x", xmonadPrompt myPromptConfig),
    ("M-S-q", confirmPrompt myPromptConfig "exit" $ io exitSuccess),
    -- Flameshot
    ("<Print>", spawn "flameshot gui"),
    ("S-<Print>", spawn "flameshot full"),
    -- Search commands
    ("M-s", SM.submap $ searchEngineMap $ S.promptSearchBrowser myPromptConfig "microsoft-edge-stable"),
    ("M-S-s", SM.submap $ searchEngineMap $ S.selectSearchBrowser "microsoft-edge-stable"),
    -- NamedScratchpads
    ("M-t", namedScratchpadAction myScratchpads "quick commands"),
    ("M-C-a", namedScratchpadAction myScratchpads "pavucontrol"),
    -- Bluetooth
    ("M-C-S-b", spawn "bluetoothctl -- connect 08:EF:3B:2B:B8:5F"),
    ("M-C-S-d", spawn "bluetoothctl -- disconnect 08:EF:3B:2B:B8:5F"),
    -- Close window(s)
    ("M-c", kill),
    ("M-S-c", killAll),
    -- Layouts
    ("M-<Space>", sendMessage NextLayout),
    ("M-C-<Space>", spawn "polybar-msg cmd toggle" >> sendMessage ToggleStruts),
    ("M-C-b", sendMessage $ Toggle NOBORDERS),
    -- Cycle workspaces
    ("M-<Down>", nextWS),
    ("M-<Up>", prevWS),
    ("M-S-<Down>", shiftToNext),
    ("M-S-<Up>", shiftToPrev),
    ("M-<Right>", nextScreen),
    ("M-<Left>", prevScreen),
    ("M-S-<Right>", shiftNextScreen),
    ("M-S-<Left>", shiftPrevScreen),
    ("M-S-z", toggleWS),
    ("M-S-<Down>", shiftToNext >> nextWS),
    ("M-S-<Up>", shiftToPrev >> prevWS),
    -- Focus
    ("M-<Tab>", windows W.focusDown),
    ("M-S-<Tab>", windows W.focusUp),
    ("M-k", windows W.focusUp),
    ("M-j", windows W.focusDown),
    ("M-m", windows W.focusMaster),
    ("M-S-<Return>", windows W.swapMaster),
    ("M-S-j", windows W.swapDown),
    ("M-S-k", windows W.swapUp),
    -- Master keybinds
    ("M-h", sendMessage Shrink),
    ("M-l", sendMessage Expand),
    ("M-a", sendMessage MirrorShrink),
    ("M-z", sendMessage MirrorExpand),
    ("M-S-t", withFocused $ windows . W.sink),
    ("M-,", sendMessage (IncMasterN 1)),
    ("M-.", sendMessage (IncMasterN (-1))),
    -- XMonad
    ("M-q", spawn "xmonad --recompile; xmonad --restart"),
    -- Volume
    ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -1%"),
    ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +1%"),
    ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
    -- Spacing
    ("M-C-S-k", incScreenSpacing 5),
    ("M-C-S-j", decScreenSpacing 5),
    ("M-C-S-l", incWindowSpacing 5),
    ("M-C-S-h", decWindowSpacing 5)
  ]

myAdditionalKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
    -- Reset the layouts on the current workspace to default
    [((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)]
      ++ [ ((m .|. mod4Mask, k), windows $ f i)
           | (i, k) <- zip myWorkspaces [xK_1 .. xK_9],
             (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
         ]
      ++ [ ((mod4Mask .|. mask, key), f sc)
           | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..],
             (f, mask) <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]
         ]

searchEngineMap method =
  M.fromList
    [ ((0, xK_a), method $ S.searchEngine "archwiki" "http://wiki.archlinux.org/index.php/Special:Search?search="),
      ((0, xK_g), method S.google),
      ((0, xK_h), method S.hoogle),
      ((0, xK_i), method S.imdb),
      ((0, xK_p), method S.aur),
      ((0, xK_s), method $ S.searchEngine "stackoverflow" "https://stackoverflow.com/search?q="),
      ((0, xK_w), method S.wikipedia),
      ((0, xK_y), method S.youtube)
    ]
