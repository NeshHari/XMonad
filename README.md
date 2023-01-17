<h1 align="center">XMonad Starter Kit</h1>
<h2 align="center">~ The Stains of Purple ~</h2>

![IMAGE](./images/reddit.png)

## An Informal Intro...
Pardon the informality in this introduction. If you are reading this, you probably already know what [XMonad](https://xmonad.org/) is. Well, if you don't, it's a dynamic tiling window manager (WM) for [X Windows System (X11)](https://wiki.archlinux.org/title/xorg) that is written, configured and fully extensible in Haskell (sincere apologies for the somewhat plagiarised description). Anyways, it's Haskell: the preeminent reason for staying clear of this WM. Regardless, it's one that you should be using. At the expense of my blood, sweat, and tears (quite literally), I present a **living document** to reduce your resistance to adopting XMonad as your daily driver. Everything required to get an aesthetic and advanced user-specific workflow is broken into smaller, consumable chunks below. 

## What's Covered (docs-wise)
*Note: Completed sections are ticked ✓. Rest can be assumed to be WIP.*
- Haskell Language Server Integration With Neovim ✓
- The Fundamentals of Modularisation ✓
- Multi-Monitor and Hot Plugging Support ✓
- Polybar As Your Statusbar
- Hyper Key Support ✓
- Other Notable Implementations: 
    - ResizableTile (Tall and Resizable, and Possible Grid Replacement)
    - PerScreen (Different Layouts for Varied Screen Dimensions)
    - SubLayouts (Custom Tabs) & Window Navigation
    - CycleWS (Cycling Through Workspaces and Screens)
    - EasyMotion (Focus and Kill Any Visible Window)
    - Rescreen (Monitor Hot Plugging) ✓
    - WindowSwallowing (Hide Terminal Instance Which Launches GUI)
    - Windowed Fullscreen (Chromium Support)
    - EwmhDesktops (Communicate with Polybar)
    - NamedScratchpads (Quick Commands, Glava, etc.) ✓
    - ShowWMName (Display Workspace Name When Switching Workspaces)
    - Custom Prompts (Man Pages, Search Engines, etc.)
    - Spacing/Gaps on the Fly
    - Managehelpers (Center Float, Shift to Workspace, etc.) ✓
    - Sane Keybindings With mkKeymap (Emacs-Style)
    - Catppuccin Colour Scheme ✓
    - Better Borders (Single Open Window, Fullscreen, etc.)
    - Topic Spaces (upcoming)
    - Theme Switching (upcoming)

## Prerequisites
The following guide requires the latest/git version of XMonad to be installed to avert recompilation errors from missing dependencies. For compatibility with the stable version (>= 0.17), consider removing [disableEwmhManageDesktopViewport](https://github.com/xmonad/xmonad-contrib/commit/cf13f8f9a7acddc1134be3f71097633def1476a8) in xmonad.hs, which is unavailable in said version at the time of writing.


## Recompilation Tips
- Ensure xorg-xmessage is installed to view compilation errors
- Ambiguity occurrences can be combatted by renaming the namespace of the imported modules using the "as" clause. For example, import ModuleA as MA and call the required functions/variables by prepending "MA."
- Avoid mutual recursion (i.e., don't import from each other). If required, create a new module. More details are provided in the Modularisation subsection. 
- Missing signature warnings can be addressed by explicitly defining the variable type or ignoring the warnings during compilation.
    ```haskell
    -- to ignore, prepend this at the top of file
    {-# OPTIONS_GHC -Wno-missing-signatures #-}
    ```
    ```haskell
    -- to address, explicit definition of myVar
    myVar :: String
    ```

## Haskell Cheat Sheet
Here's a pretty good [cheat sheet](https://hackage.haskell.org/package/CheatSheet-1.10/src/CheatSheet.pdf) to familiarise with Haskell if you come from any other programming language.

## Haskell Language Server (HLS) With Neovim
To easily manage LSP servers in Neovim, I suggest using the [Mason](https://github.com/williamboman/mason.nvim) plugin. A straightforward approach is to install [LSP Zero](https://github.com/VonHeikemen/lsp-zero.nvim).
```lua
-- example using lazy plugin manager
 { 'VonHeikemen/lsp-zero.nvim',
        dependencies = {
            -- LSP Support
            { 'neovim/nvim-lspconfig' },
            { 'williamboman/mason.nvim' },
            { 'williamboman/mason-lspconfig.nvim' },
            -- Autocompletion
            { 'hrsh7th/nvim-cmp' },
            { 'hrsh7th/cmp-buffer' },
            { 'hrsh7th/cmp-path' },
            { 'saadparwaiz1/cmp_luasnip' },
            { 'hrsh7th/cmp-nvim-lsp' },
            { 'hrsh7th/cmp-nvim-lua' },
            -- Snippets
            { 'L3MON4D3/LuaSnip' },
            { 'rafamadriz/friendly-snippets' },
        }
    },
```
*Note: Look at my [lsp.lua](./nvim/.config/nvim/after/plugin/lsp.lua) for configuration post installation.*

To ensure complete compatibility [Haskell Language Server (HLS)](https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#user-content-hls) with Neovim, XMonad should be [setup](https://xmonad.org/INSTALL.html) using stack or cabal. Installing via Pacman/AUR will result in "could not find module" or "unknown package" errors on import of any module, despite HLS successfully attaching and running on Neovim buffer. HLS provides various features such as diagnostics, completions, code actions, and formatting. [Ormolu](https://haskell-language-server.readthedocs.io/en/latest/features.html) is utilised as my formatter of choice. The complete list of features is provided [here](https://haskell-language-server.readthedocs.io/en/latest/features.html).

A minimal hie.yaml must be defined for HLS to function.
```yaml
cradle:
    stack:
```

Expected High Level Structure
```
.
├── hie.yaml
├── lib
├── stack.yaml
├── stack.yaml.lock
├── xmonad
├── xmonad-contrib
└── xmonad.hs
```

*Note: If XMonad was installed via stack, symlink or add the xmonad executable to PATH to make "xmonad --restart" usable.*
```bash
sudo ln -s ~/.local/bin/xmonad /usr/bin
```

## Modularisation
To improve accessibility and testing compared to monolithic code, code is separated into independent modules integrated as required in xmonad.hs. Modules in XMonad are read from the "lib" folder by default, which can be created in the same directory as xmonad.hs. Individual modules (i.e., files with .hs extension) can then be made directly in that folder (./lib) or subfolders (e.g. ./lib/Custom) within and imported in xmonad.hs.

### The Basics

General Path: ./lib/Custom/MyModule.hs, where "." is relative to where xmonad.hs resides, and MyModule is replaceable by the name of the module. As a rule of thumb, ensure both file name (MyModuleName.hs) and module name (Custom.MyModuleName) are the same.

```haskell
-- path: lib/Custom/MyModule.hs
-- define module at the top of file
module Custom.MyModule where
-- other imports
-- code...
```

```haskell
-- path: xmonad.hs
-- import the custom module
import Custom.MyModule

-- code...
```
```fish
xmonad/lib/Custom
>  exa --tree
.
├── MyCatppuccin.hs
├── MyDecorations.hs
├── MyEasyMotion.hs
├── MyKeys.hs
├── MyLayouts.hs
├── MyMacAddresses.hs
├── MyManagement.hs
├── MyManagementPositioning.hs
├── MyMouse.hs
├── MyScratchpads.hs
├── MyScreen.hs
├── MyStartupApps.hs
└── MyWorkspaces.hs
```

*Note: Ensure no mutually recursive modules exist, or XMonad will not compile. These are modules that import each other. For example, if you import Custom.MyScratchpads in MyManagement.hs, do not import Custom.MyManagement.hs in Custom.MyScratchpads. If the need arises, you can bypass this by extracting part of the module into an even simpler module, as seen in MyManagementPositioning.hs.*

## MyCatppuccin.hs  (Catppuccin Mocha)
Create colour variables for the "Catppuccin Mocha" [palette](https://github.com/catppuccin/catppuccin#user-content--palettes) due to their simplicity in recognition compared to hex representations. Prepend colour variables with something unique to that colour scheme, such as "cat", to prevent ambiguity when used in conjunction with other colour schemes with the same variable name. For example, catBlue and nordBlue are different, but using just "blue" creates **ambiguity** errors. Refer to the Recompilation Tips subsection on other methods to prevent ambiguous occurrences.

```haskell
module Custom.MyCatppuccin where

catRosewater :: String
catRosewater = "#f5e0dc"

catFlamingo :: String
catFlamingo = "#f2cdcd"

catPink :: String
catPink = "#f5c2e7"

catMauve :: String
catMauve = "#cba6f7"

catRed :: String
catRed = "#f38ba8"

catMaroon :: String
catMaroon = "#eba0ac"

catPeach :: String
catPeach = "#fab387"

catYellow :: String
catYellow = "#f9e2af"

catGreen :: String
catGreen = "#a6e3a1"

catTeal :: String
catTeal = "#94e2d5"

catSky :: String
catSky = "#89dceb"

catSapphire :: String
catSapphire = "#74c7ec"

catBlue :: String
catBlue = "#89b4fa"

catLavender :: String
catLavender = "#b4befe"

catText :: String
catText = "#cdd6f4"

catSubtext1 :: String
catSubtext1 = "#bac2de"

catSubtext0 :: String
catSubtext0 = "#a6adc8"

catOverlay2 :: String
catOverlay2 = "#9399b2"

catOverlay1 :: String
catOverlay1 = "#7f849c"

catOverlay0 :: String
catOverlay0 = "#6c7086"

catSurface2 :: String
catSurface2 = "#585b70"

catSurface1 :: String
catSurface1 = "#45475a"

catSurface0 :: String
catSurface0 = "#313244"

catBase :: String
catBase = "#1e1e2e"

catMantle :: String
catMantle = "#181825"

catCrust :: String
catCrust = "#11111b"

```
## MyStartupApps.hs
Launch startup applications/scripts like setting wallpaper etc. Some applications are denoted by "spawn" instead of "[spawnOnce](https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Util-SpawnOnce.html)" (i.e., only once) is to facilitate display hot plugging. Further details are provided in the MyRescreen.hs subsection.
```haskell
module Custom.MyStartupApps where

import XMonad
import XMonad.Util.SpawnOnce

myStartupHook :: X ()
myStartupHook = do
  spawn "feh --bg-scale ~/wallpapers/stains_of_purple.jpg"
  spawn "~/scripts/feh-blur.sh -s; ~/scripts/feh-blur.sh -d"
  spawnOnce "xmodmap ~/.Xmodmap"
  spawnOnce "dunst &"
  spawn "killall picom; picom -b"
  spawnOnce "easyeffects --gapplication-service &"
```

## MyRescreen.hs
Hot plugging is a must-have feature for any multi-monitor workflow. XMonad provides a [custom hook](https://xmonad.github.io/xmonad-docs/xmonad-contrib-0.16.999/XMonad-Hooks-Rescreen.html) that monitors xrandr changes. This is best used alongside [autorandr](https://github.com/phillipberndt/autorandr), which automatically selects a predefined configuration dependent on the number of connected displays.

### Autorandr 
Required package: [autorandr](https://archlinux.org/packages/community/any/autorandr/)

Firstly, ensure autorandr detects all possible monitor combinations. In this example, I shall provide single and dual monitor setups. This assumes the layouts (i.e., portrait/landscape & positioning) are already set up.

With only one display on, run the following command: 
```fish
autorandr --save single
```

With both displays on, run the following command:
```fish
autorandr --save dual
```
Use the following command to ensure the correct layout is detected:
```fish
autorandr --detected
```
Adapt this concept to whatever configuration you have.

### Rescreen Hook
Once autorandr is good to go, add the self-explanatory Rescreen hooks below. If you get kicked to TTY (i.e., Xorg crashed), increase the sleep duration before restarting xmonad. My purpose for "restarting" is to recall the StartupHook in Custom.MyStartupApps, and spawn polybar and feh accordingly on the detected monitor. This is critical when switching from smaller to more extensive displays (e.g., single -> dual monitor).

```haskell
module Custom.MyScreen where

import XMonad
import XMonad.Hooks.Rescreen

myAfterRescreenHook :: X ()
myAfterRescreenHook = spawn "sleep 1; xmonad --restart"

myRandrChangeHook :: X ()
myRandrChangeHook = spawn "autorandr --change"

rescreenCfg :: RescreenConfig
rescreenCfg =
  def
    { afterRescreenHook = myAfterRescreenHook,
      randrChangeHook = myRandrChangeHook
    }
```

## MyWorkspaces.hs
Workspaces can be named as you wish. When switching workspaces, the names will be displayed on the screen if used alongside [XMonad.Layout.ShowWName](https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Layout-ShowWName.html). Always remember what's named here must be carried over to your Polybar configuration, mainly if icons are used.

```haskell
module Custom.MyWorkspaces where

myWorkspaces :: [String]
myWorkspaces = ["one", "two", "three", "four", "five"]
```
Snippet of EWMH module in Polybar's config.ini:
```
icon-0 = one;<icon-for-ws-1>
icon-1 = two;<icon-for-ws-2>
icon-2 = three;<icon-for-ws-3>
icon-3 = four;<icon-for-ws-4>
icon-4 = five;<icon-for-ws-5>
```
Click [here](./polybar/.config/polybar/config.ini) for my full Polybar configuration.

## MyManagement.hs
There will be instances where you want windows to automatically start in full screen, float in the middle, spawn in a different workspace, and much more etc. This is where [ManageHelpers](https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Hooks-ManageHelpers.html) come in. For such windows, you must first identify appName/className/resource of that window. The differences are indicated below, referenced from [Hackage](https://hackage.haskell.org/package/xmonad-0.17.1/docs/XMonad-ManageHook.html).
```haskell
appName :: Query String
Return the application name; i.e., the first string returned by WM_CLASS.

resource :: Query String
Backwards compatible alias for appName.

className :: Query String
Return the resource class; i.e., the second string returned by WM_CLASS.
```
To get the WM_CLASS, run the following in the terminal:
```fish
xprop | grep 'CLASS'
```
*Note: The terminal will not display any output till a visible window is clicked with a mouse/cursor.*

Now that we know the window's name, we use composeAll, which executes all matching rules, unlike composeOne, which only executes the first match. We can then utilise the same manage helper for multiple windows (via <&&>). To shift to a different workspace, use doShift followed by the workspace name (e.g., --> doShift "games"). Note that the workspace names must exist for this to work. [MyWorkspaces.hs](./xmonad/.config/xmonad/lib/Custom/MyWorkspaces.hs) covers how workspaces are defined. Finally, since manage helpers are functions to be used with manageHook, we must add them back to the hook since myManagement was extracted instead of defined directly in the manageHook.
```haskell
module Custom.MyManagement where

import Custom.MyScratchpads
import XMonad
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Util.NamedScratchpad

myManagement =
  composeAll
    [(className =? "witcher3.exe" <&&> className =? "steam_app_0") --> doCenterFloat]

myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook myScratchpads <> myManagement
```

## MyManagementPositioning.hs
This is an extension to Custom.MyManagement. It handles the positioning of floating windows such as scratchpads. To keep the windows centred whilst varying the size, only modify the width and height "size" variables whilst ignoring the fromLeft and fromTop "distance" variables I have defined. You can skip the {-# Language... #-}. It is just something I use to ignore warnings stemming from importing qualified during stack install.
```haskell
{-# LANGUAGE ImportQualifiedPost #-}

-- separated module from Custom.MyManagement to prevent mutually recursive modules
module Custom.MyManagementPositioning where

import XMonad
import XMonad.StackSet qualified as W
import XMonad.Util.NamedScratchpad

myCenter :: ManageHook
myCenter = customFloating $ W.RationalRect fromLeft fromTop width height
  where
    width = 1 / 2
    height = 1 / 2
    fromLeft = (1 - width) / 2
    fromTop = (1 - height) / 2

myCenterSmall :: ManageHook
myCenterSmall = customFloating $ W.RationalRect fromLeft fromTop width height
  where
    width = 1 / 3
    height = 1 / 3
    fromLeft = (1 - width) / 2
    fromTop = (1 - height) / 2
```
## MyScratchpads.hs
Scratchpads can be considered floating windows that are hidden and shown as necessary. I use it for running quick terminal commands via Alacritty instead of my main terminal Kitty. The reason is that scratchpads are dependent on WM_CLASS. If a terminal (e.g., kitty) is already open, and one toggles a scratchpad with the same WM_CLASS "kitty", XMonad may hide the tiled window instead.
```haskell
module Custom.MyScratchpads where

import Custom.MyManagementPositioning
import XMonad (appName)
import XMonad.ManageHook ((=?))
import XMonad.Util.NamedScratchpad

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "quick commands" spawnQc findQc myCenter,
    NS "glava" spawnGl findGl myCenterSmall
  ]
  where
    spawnQc = "alacritty -e fish"
    findQc = appName =? "Alacritty"

    spawnGl = "glava"
    findGl = appName =? "GLava"

{-
To get WM_CLASS of a visible window, run "xprop | grep 'CLASS'" and select the window.
appName :: Query String
Return the application name; i.e., the first string returned by WM_CLASS.

resource :: Query String
Backwards compatible alias for appName.

className :: Query String
Return the resource class; i.e., the second string returned by WM_CLASS. -}
```
Another thing to consider is hiding the "NSP" workspace, which appears in the polybar from the first spawn of a named scratchpad. A simple and effective solution is to set the icon for the NSP workspace to empty. Assuming icons are used as labels for workspaces, leave the icon for NSP  blank.

Snippet of EWMH module in Polybar's config.ini:
```
icon-0 = one;<icon-for-ws-1>
icon-1 = two;<icon-for-ws-2>
icon-2 = three;<icon-for-ws-3>
icon-3 = four;<icon-for-ws-4>
icon-4 = five;<icon-for-ws-5>
icon-5 = NSP;
```

## Hyper Keys
Inspired by Ethan Schoonover's [video](https://www.youtube.com/watch?v=70IxjLEmomg)...

Required package: [xcape](https://archlinux.org/packages/community/x86_64/xcape/)

The following is achieved:
- Caps to Escape (for vim use). 
- Holding down Caps (i.e., Esc) acts as Ctrl (easy to Ctrl-f for shell completion etc.)
- Holding down either Tab or Backslash acts as Windows key (i.e., Mod4Mask).

Files used (ensure xcape is installed):
1. .xinitrc
```
    xmodmap ~/.Xmodmap

    setxkbmap -option "caps:ctrl_modifier" &

    xcape -e 'Caps_Lock=Escape' &
```
2. .Xmodmap (also called in MyStartupApps)
```
    ! Tab as modifier

    keycode 23 = Tab Hyper_L
    
    ! Backslash(\) as modifier + preserve bar(|)

    keycode 51 = backslash bar Hyper_L
    
    add mod4 = Hyper_L
    
    clear lock
```
