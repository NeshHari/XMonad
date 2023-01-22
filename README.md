<h1 align="center">XMonad Starter Kit</h1>
<h2 align="center">~ The Stains of Purple ~</h2>

![IMAGE](./images/xmonad.png)

## An Informal Intro...
Are you tired of using the same old window manager? Want to elevate your computing experience to the next level? Look no further than XMonad - the dynamic, tiling window manager written in the powerful Haskell programming language.

But don't let the fact that it's written in Haskell scare you away. Despite its intimidating reputation, XMonad is incredibly user-friendly and customizable to fit your specific needs. And the effort you put into learning it will be well worth it.

Through blood, sweat, and tears (yes, literally), I present to you a comprehensive guide to ease your transition to using XMonad as your daily driver. From aesthetics to advanced workflow, everything required to get an aesthetic and advanced user-specific workflow is broken into smaller, consumable chunks below. So what are you waiting for? Let's revolutionize the way you work and play with XMonad.

## What's Covered (docs-wise)
| Feature                                                           | Status    |
|-------------------------------------------------------------|----------|
| Haskell Language Server Integration with Neovim             | Completed |
| The Fundamentals of Modularization                           | Completed |
| Multi-Monitor and Hot-Plugging Support                       | Completed |
| Polybar as Your Statusbar                                   | Completed |
| Hyper Key Support                                           | Completed |
| ResizableTile (Tall and Resizable, and Possible Grid Replacement) | Completed |
| Per-Screen (Different Layouts for Varied Screen Dimensions)   | Completed |
| Sub-Layouts (Custom Tabs) & Window Navigation                 | Completed |
| CycleWS (Cycling Through Workspaces and Screens)             | WIP       |
| EasyMotion (Focus and Kill Any Visible Window)               | WIP       |
| Rescreen (Monitor Hot-Plugging)                              | Completed |
| WindowSwallowing (Hide Terminal Instance Which Launches GUI) | WIP       |
| Windowed Fullscreen (Chromium Support)                       | WIP       |
| EwmhDesktops (Communicate with Polybar)                      | WIP       |
| NamedScratchpads (Quick Commands, Glava, etc.)              | Completed |
| ShowWMName (Display Workspace Name When Switching Workspaces)| Completed |
| Custom Prompts (Man Pages, Search Engines, etc.)            | WIP       |
| Spacing/Gaps on the Fly                                      | WIP       |
| Managehelpers (Center Float, Shift to Workspace, etc.)       | Completed |
| Sane Keybindings with mkKeymap (Emacs-Style)                | WIP       |
| Catppuccin Color Scheme                                     | Completed |
| Better Borders (Single Open Window, Fullscreen, etc.)        | Completed |
| Topic Spaces                                                 | Upcoming  |
| Theme Switching                                              | Upcoming  |

## Prerequisites
The following guide requires the latest/git version of XMonad to be installed to avert recompilation errors from missing dependencies. For compatibility with the stable version (>= 0.17), consider removing [disableEwmhManageDesktopViewport](https://github.com/xmonad/xmonad-contrib/commit/cf13f8f9a7acddc1134be3f71097633def1476a8) in xmonad.hs, which is unavailable in said version at the time of writing.

## Recompilation Tips
*Note: xorg-xmessage is installed to view compilation errors*
In xmonad, there are several common recompilation errors that can occur while building a configuration. Some of these errors include:
- Mutual recursion with imported modules - When two or more functions are defined in terms of each other, creating an infinite loop, and the functions are defined in different imported modules. To fix this, you will need to ensure that the mutual recursion is not happening by importing the modules in a different order or by changing the function calls to not cause recursion.
- Ambiguity Occurrences - An ambiguity error in xmonad can occur when multiple functions with the same name are imported from different modules. To fix this, you can use a qualified import or use an import abbreviation (import module *as* abbreviation) to specify which function to use.

    For example, if you want to use the function foo from both module A and B in your xmonad configuration, you can import them like this:
    ```haskell
    import A (foo)
    import B (foo) as fooB
    ```
    Then, when you use the function foo in your code, it will refer to the one imported from module A, and you can use fooB to refer to the one from module B.
    
    Alternatively, you can use a qualified import to specify which function to use like this:
    ```haskell
    import qualified A
    import qualified B
    ```
    Then, when you use the function foo in your code, you will have to specify from which module you want to use it, like A.foo or B.foo. By doing this, you can avoid the ambiguity error, the compiler will understand which function you want to use.
- Unresolved symbols - When the compiler is unable to find the definition of a symbol that is referenced in the code. To fix this, you will need to import the necessary modules or define the symbol.
- Type error - When the type of a variable or expression does not match what is expected by the function or operation. To fix this, you will need to ensure that the types of the variables and expressions are consistent with what is expected.
- Syntax error - When the code does not conform to the Haskell syntax. To fix this, you will need to check for missing or extra characters, or any other syntax issues.
- Missing type signatures/incomplete type annotations - When the type signature of a function or variable is missing or not specified, the compiler may not have enough information to understand the expected behavior of the function or variable. This can lead to potential type errors during compilation. To avoid this, it's important to ensure that all functions and variables have their type signature specified. This way, the compiler can understand the inputs and outputs of the function or variable, and how it should behave. Although it's possible to suppress this warning by using the -Wno-missing-signatures flag when compiling the code, it's not recommended as it can lead to potential type errors in the future. For best practices, it's always recommended to include the type signature for functions and variables.
    ```haskell
    -- to ignore, prepend this at the top of the file
    {-# OPTIONS_GHC -Wno-missing-signatures #-}
    ```
    ```haskell
    -- to address, explicitly define the type for myVar (e.g. String)
    myVar :: String
    ```

## Haskell Cheat Sheet
Here's a pretty good [cheat sheet](https://hackage.haskell.org/package/CheatSheet-1.10/src/CheatSheet.pdf) to familiarise yourself with Haskell if you come from any other programming language.

### $ vs . Operators
In Haskell, both the $ operator and the dot (.) operator are used to control the order of function application. However, they have different functionality.

The $ operator is used to apply a function to an argument without the need for parentheses. This operator has the lowest precedence of any operator in Haskell and is often used to make code more readable by allowing the argument to be placed at the end of a chain of function calls. For example, f(g(x)) can be rewritten as f $ g x.
On the other hand, the dot operator (.) is used to compose two functions together. This creates a new function that applies the right-hand function to the result of the left-hand function. An example of this would be f(g(x)) becoming (f . g) x. The dot operator has a higher precedence than the $ operator, so it will be evaluated first.

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

Expected High-Level Structure
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
-- define the module at the top of the file
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
├── MyPolybar.hs
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
Hot plugging is a must-have feature for any multi-monitor workflow. XMonad provides a [custom hook](https://xmonad.github.io/xmonad-docs/xmonad-contrib-0.16.999/XMonad-Hooks-Rescreen.html) that monitors xrandr changes, best used alongside [autorandr](https://github.com/phillipberndt/autorandr), which automatically selects a predefined configuration dependent on the number of connected displays.

### Autorandr 
Required package: [autorandr](https://archlinux.org/packages/community/any/autorandr/)

Firstly, ensure autorandr detects all possible monitor combinations. In this example, I shall provide single and dual monitor setups. This assumes the layouts (i.e., portrait/landscape & positioning) are already set up.

With only one display on, run the following command: 
```fish
autorandr --save single
```
*Note: There may be cases, particularly on boot, where the second monitor is turned off (i.e., black screen) but connected to a power supply and display port. In cases where xrandr does not correctly detect the screen resolution, you may want to force the second monitor to be off before saving the "single" profile. In my setup, DP-1 is the second monitor I want off, whilst the specified resolution is for the monitor I wish to utilise.*
```fish
xrandr --fb 2560x1440 --output DP-1 --off
autorandr --save single --force
```
With both displays on, run the following command:
```fish
autorandr --save dual
```
Use the following command to ensure the correct layout is detected:
```fish
autorandr --detected
```
Adapt this concept to whatever configuration you have. To debug, run the following: 
```fish
autorandr --debug --dry-run -cf
```

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
There will be instances where you want windows to start in full screen automatically, float in the middle, spawn in a different workspace, etc. [ManageHelpers](https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Hooks-ManageHelpers.html) come in handy in such instances. For such windows, you must first identify the appName/className/resource of that window. The differences are referenced below from [Hackage](https://hackage.haskell.org/package/xmonad-0.17.1/docs/XMonad-ManageHook.html).
```haskell
appName :: Query String
Return the application name, i.e., the first String returned by WM_CLASS.

resource :: Query String
Backwards compatible alias for appName.

className :: Query String
Return the resource class, i.e., the second String returned by WM_CLASS.
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
Return the application name; i.e., the first String returned by WM_CLASS.

resource :: Query String
Backwards compatible alias for appName.

className :: Query String
Return the resource class; i.e., the second String returned by WM_CLASS. -}
```
Another thing to consider is hiding the "NSP" workspace, which appears in the polybar from the first spawn of a named scratchpad. A simple and effective solution is to set the icon for the NSP workspace to empty. Assuming icons are used as labels for workspaces, leave the icon for NSP  blank. Another alternative is to filter out the workspace. 

Snippet of EWMH module in Polybar's config.ini:
```
icon-0 = one;<icon-for-ws-1>
icon-1 = two;<icon-for-ws-2>
icon-2 = three;<icon-for-ws-3>
icon-3 = four;<icon-for-ws-4>
icon-4 = five;<icon-for-ws-5>
icon-5 = NSP;
```
Snippet of xmonad.hs:
```haskell
$ addEwmhWorkspaceSort (pure (filterOutWs [scratchpadWorkspaceTag]))
````

## MyLayouts.hs
Each layout and screen can be customised to your workflow. I have included the layouts I use daily. I suggest using the "ResizableTall" layout as it permits the modification of window height and width. Remember to avoid struts on layouts that are not full screen to prevent docks (i.e., polybar) from overlapping the layouts or vice versa. I have added a key map to toggle polybar and struts manually if you wish. Optionally, you may rename your layouts which will affect how the current layout name is printed in polybar.

Snippet of key map:
```haskell
("M-C-<Space>", spawn "polybar-msg cmd toggle" >> sendMessage ToggleStruts),
```
### PerScreen
In my dual monitor setup, I have a portrait and landscape monitor. It is not sensible to use the same layouts for both monitors. Using the "Tall" layout on a portrait monitor (1080 x 1920) is impractical. Therefore, I specify that only if the screen resolution is wider than 1080, I use "Tall". Otherwise I use "Column". Additionally, for both monitors I use "Full" since I would like full screen support for both. 

### SubLayouts & BoringWindows
SubLayouts are layouts within a layout. I use tabs as a sub layout for "Tall" and "Column". Although not included, you may consider using "Accordion" as a sub layout to "Column". Tweak this however you wish as long as its sensible to you. BoringWindows allow you to skip over windows in a sublayout when moving focus. I use a separate separate key map to navigate through windows confined in a sub layout (i.e., grouped together). 

Snippet of key map:
```haskell
("M-C-.", onGroup W.focusUp'),
("M-C-,", onGroup W.focusDown')
```
### Better Borders
If borders are not needed when a single window is present, use "smartBorders". Borders will be visible when more than one window is present. For full screen layouts, use "noBorders" to altogether remove the border, achievable by dynamically applying and removing the transformer. 

Snippet of concept above: 
```haskell
smartBorders $
  mkToggle
    (NOBORDERS ?? FULL ?? EOT)
```
*Note: EOT simply marks the end of transformer*

Everything in MyLayout.hs:

*Note: I have disabled the missing signature warning for this module due to the complexity in defining type signatures for the present variables.*
```haskell
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Custom.MyLayouts where

import Custom.MyDecorations
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BoringWindows
import XMonad.Layout.Column
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerScreen
import XMonad.Layout.Renamed as XLR
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation

mySpacing i = spacingRaw False (Border 10 10 30 30) True (Border i i i i) True

tabs =
  renamed [XLR.Replace "Tabs"] $
    avoidStruts $
      tabbed
        shrinkText
        myTabConfig

tall =
  renamed [XLR.Replace "Tall"] $
    avoidStruts $
      windowNavigation $
        addTabs shrinkText myTabConfig $
          subLayout [] tabs $
            mySpacing 7 $
              ResizableTall nmaster delta ratio []
  where
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

column =
  renamed [XLR.Replace "Column"] $
    avoidStruts $
      windowNavigation $
        addTabs shrinkText myTabConfig $
          subLayout [] tabs $
            mySpacing 7 $
              Column 1.0

full = renamed [XLR.Replace "Monocle"] $ noBorders Full

myLayout = boringWindows (ifWider 1080 tall column ||| full)

myLayoutHook =
  showWName' myShowWNameConfig $
    smartBorders $
      mkToggle
        (NOBORDERS ?? FULL ?? EOT)
        myLayout
```
## Polybar Support
With the release of [XMonad.Hooks.StatusBar](https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Hooks-StatusBar.html) and [XMonad.Hooks.StatusBar.PP](https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Hooks-StatusBar-PP.html), utilising Polybar with XMonad has become straightforward. To get started, the polybar configuration requires two modules: "ewmh" and "xmonad". Let us take a closer look at each module separately.

### EWMH Module
This module queries the EWMH desktops configured by XMonad, which explains why we import EwmhDesktops (ewmh) in xmonad.hs. As highlighted in the section on prerequisites, I suggest utilising disableEwmhManageDesktopViewport, which prevents the wrong ordering of workspaces you may encounter, especially with a multi-polybar instance workflow. When assigning each workspace an icon, ensure the name of the workspace (e.g., "one") is an exact match of that declared in MyWorkspaces.hs. As far as I know, it is not possible to retrieve other information such as the current monitor layout via this module alone. 

*Note: Window titles are not supported by this module. To display window titles, use module/title of type internal/xwindow. Or you may also use the module/xmonad.*

```ini
[module/ewmh]
type = internal/xworkspaces
enable-click = false
enable-scroll = false

icon-0 = one;
icon-1 = two;
icon-2 = three;
icon-3 = four;
icon-4 = five;
icon-5 = NSP;

format = <label-state>
label-active = %icon% 
label-occupied = %icon%
label-empty = %icon%

label-empty-padding = 1
label-active-padding = 1
label-urgent-padding = 1
label-occupied-padding = 1

label-empty-foreground = ${colors.surface2}
label-active-foreground = ${colors.green}
label-urgent-foreground = ${colors.red}
label-occupied-foreground = ${colors.flamingo}

```
### XMonad Module
This module executes xmonadpropread found in xmonad-contrib. It permits the use of property logging via xmonadPropLog, which writes a formatted string (i.e., dynamicLogString) to _XMONAD_LOG to be further processsed by polybar's module/xmonad.
```ini
[module/xmonad]
type = custom/script
exec = /home/nesh/.config/xmonad/xmonad-contrib/scripts/xmonadpropread.hs
tail = true
format-font = 5
format-foreground = ${colors.peach}
format-offset = -20
```
### MyPolybar.hs
With the rudiments of xmonadPropLog and dynamicLogString covered in the previous subsection, we can move on to customising the formatted string discussed earlier. At this stage in our example, we only have workspace icons. Optionally, we discussed how to include window titles. Although all of which can be made available by modifying the ppOrder to include "ws" (i.e., workspace) and "t" (i.e., title), it is much more tedious to do so since we need to find the exact icon code rather than simply copy and paste the icon of choice. Therefore, I personally prefer the current approach of using two distinct modules (i.e., ewmh and xmonad) in polybar. Continuing our example, I only wish to add the current layout ("l") as defined in ppOrder. However, I would like polybar to assign the colors as it would for any module via "format-foreground". I also do not wish to wrap the current layout with anything (e.g. [Tall], >Tall<, etc.). Therefore, I shall only define the textColor variable and ignore wraps.
```haskell
module Custom.MyPolybar where

import XMonad (spawn)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

myPolybar :: StatusBarConfig
myPolybar =
  def
    { sbLogHook =
        xmonadPropLog
          =<< dynamicLogString polybarPP,
      sbStartupHook = spawn "~/.config/polybar/startup.sh",
      sbCleanupHook = spawn "killall polybar"
    }

polybarPP :: PP
polybarPP =
  def
    { ppCurrent = textColor "" . wrap "" "",
      ppOrder = \(_ : l : _ : _) -> [l]
    }

textColor :: String -> String -> String
textColor color = wrap ("%{F" <> color <> "}") " %{F-}"
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

