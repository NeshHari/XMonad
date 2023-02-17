<div align="center">

<h1 align="center">XMonad Starter Kit</h1>

<a href="https://github.com/NeshHari/XMonad/stargazers">![GitHub Repo stars](https://img.shields.io/github/stars/NeshHari/XMonad?color=%23f2cdcd&label=STARS%20&logo=github&logoColor=%23f2cdcd&style=for-the-badge)</a>
<a href="https://github.com/NeshHari/XMonad/issues">![GitHub issues](https://img.shields.io/github/issues/NeshHari/XMonad?color=%23b4befe&label=ISSUES&logo=GitBook&logoColor=%23b4befe&style=for-the-badge)<a>

</div>

<h2 align="center">Ewwification</h2>

<img src="./images/ewwification.png" alt="img" align="center">

## Tool Overview

- **WM**: [xmonad](https://github.com/xmonad/xmonad)
  - **Extensions**: [xmonad-contrib](https://github.com/xmonad/xmonad-contrib)
- **OS**: [Arch Linux](https://archlinux.org/)
- **Compositior**: [picom-pjulius-git](https://github.com/pijulius/picom)
- **Shell**: [fish](https://wiki.archlinux.org/title/fish)
  - **Plugin Manager**: [fisher](https://github.com/jorgebucaran/fisher)
- **Terminal**: [kitty](https://github.com/kovidgoyal/kitty/)
- **Editor**: [Neovim](https://github.com/neovim/neovim/)
  - **Plugin Manager**: [lazy](https://github.com/folke/lazy.nvim)
- **File Manager**: [Thunar](https://git.xfce.org/xfce/thunar/)
- **Launcher**: [Rofi](https://github.com/davatorium/rofi/) + [jgmenu](https://github.com/jgmenu/jgmenu)
- **Multipurpose**: [eww](https://github.com/elkowar/eww)
- **Browser**: [Microsoft Edge Beta/Dev](https://aur.archlinux.org/packages/microsoft-edge-beta-bin)
  - **Extension**: [nightTab](https://github.com/zombieFox/nightTab)

## Contents

<!--toc:start-->

- [Introduction](#introduction)
- [Scope of Coverage](#scope-of-coverage)
- [Preparatory Requirements](#preparatory-requirements)
- [Installation Guidelines](#installation-guidelines)
  - [Automatic Installation (Arch Linux)](#automatic-installation-arch-linux)
  - [Dependencies for Manual Installation](#dependencies-for-manual-installation)
  - [Additional Fonts](#additional-fonts)
  - [TUI/CLI Steps](#tuicli-steps)
  - [Wallpaper](#wallpaper)
  - [Cursor](#cursor)
  - [Spotify](#spotify)
  - [GTK](#gtk)
  - [Discord](#discord)
  - [Neovim](#neovim)
  - [Pacman Command Aliases](#pacman-command-aliases)
- [Compilation Advice and Troubleshooting](#compilation-advice-and-troubleshooting)
- [Haskell Cheat Sheet](#haskell-cheat-sheet)
  - [Difference Between $ and . Operators](#difference-between-and-operators)
- [Neovim Integration with Haskell Language Server (HLS)](#neovim-integration-with-haskell-language-server-hls)
- [Modular Design](#modular-design)
  - [Fundamental Principles](#fundamental-principles)
- [MyCatppuccin.hs (Catppuccin Mocha)](#mycatppuccinhs-catppuccin-mocha)
- [MyStartupApps.hs](#mystartupappshs)
- [MyScreen.hs](#myscreenhs)
  - [Autorandr](#autorandr)
  - [Rescreen Hook](#rescreen-hook)
- [MyWorkspaces.hs](#myworkspaceshs)
- [MyManagement.hs](#mymanagemenths)
- [MyManagementPositioning.hs](#mymanagementpositioninghs)
- [MyScratchpads.hs](#myscratchpadshs)
- [MyLayouts.hs](#mylayoutshs)
  - [PerScreen](#perscreen)
  - [SubLayouts and BoringWindows](#sublayouts-and-boringwindows)
  - [Enhanced Borders](#enhanced-borders)
  - [Gaps Gaps Gaps](#gaps-gaps-gaps)
- [Polybar Integration](#polybar-integration)
  - [EWMH Module](#ewmh-module)
  - [XMonad Module](#xmonad-module)
  - [MyPolybar.hs](#mypolybarhs)
- [Event Management](#event-management)
  - [Event Swallowing Hook](#event-swallowing-hook)
  - [X Property Change Event](#x-property-change-event)
  - [Fullscreen Hack for Windowed Applications (Chromium)](#fullscreen-hack-for-windowed-applications-chromium)
- [Java Application Hack](#java-application-hack)
- [Hyper Keys](#hyper-keys)
- [Emacs-Like Key Bindings](#emacs-like-key-bindings)
- [Vim-Like Easy Motion](#vim-like-easy-motion)
- [Cycle Through Workspaces](#cycle-through-workspaces)
- [Flexible Mouse Resize](#flexible-mouse-resize)
- [Search Engine & Other Prompts](#search-engine-other-prompts)
<!--toc:end-->

<h2 align="center"> IMPORTANT </h2>

**_This document serves as a supplementary resource to provide insights and
understanding into the code of the dotfiles. It is not intended to be used
as a copy-paste solution and should not replace careful review and customization
of the actual dotfiles._**

## Introduction

Unleash the power of XMonad, the dynamic tiling window manager that will leave your old window manager in the dust. Written in Haskell, XMonad packs a punch in terms of customization options and will transform the way you work. But don't worry, this guide, produced at the expense of blood, sweat, and tears (yes, literally), will make the transition a breeze. So, get ready to laugh in the face of intimidation and take control of your computing experience with XMonad. The time for revolution is now!

## Scope of Coverage

| Feature                                                           | Status    |
| ----------------------------------------------------------------- | --------- |
| Haskell Language Server Integration with Neovim                   | Completed |
| The Fundamentals of Modularization                                | Completed |
| Multi-Monitor and Hot-Plugging Support                            | Completed |
| Polybar as Your Statusbar                                         | Completed |
| Hyper Key Support                                                 | Completed |
| ResizableTile (Tall and Resizable, and Possible Grid Replacement) | Completed |
| Per-Screen (Different Layouts for Varied Screen Dimensions)       | Completed |
| Sub-Layouts (Custom Tabs) & Window Navigation                     | Completed |
| CycleWS (Cycling Through Workspaces and Screens)                  | Completed |
| EasyMotion (Focus and Kill Any Visible Window)                    | Completed |
| Rescreen (Monitor Hot-Plugging)                                   | Completed |
| WindowSwallowing (Hide Terminal Instance Which Launches GUI)      | Completed |
| Java Hack (Better Support for Java Apps)                          | Completed |
| Windowed Fullscreen (Chromium Support)                            | Completed |
| EwmhDesktops (Communicate with Polybar)                           | Completed |
| NamedScratchpads (Quick Commands, Glava, etc.)                    | Completed |
| ShowWMName (Display Workspace Name When Switching Workspaces)     | Completed |
| Custom Prompts (Man Pages, Search Engines, etc.)                  | Completed |
| Spacing/Gaps on the Fly                                           | Completed |
| Managehelpers (Center Float, Shift to Workspace, etc.)            | Completed |
| Sane Keybindings with mkKeymap (Emacs-Style)                      | Completed |
| Catppuccin Color Scheme                                           | Completed |
| Better Borders (Single Open Window, Fullscreen, etc.)             | Completed |
| Flexible Mouse Resize                                             | Completed |
| Topic Spaces                                                      | Upcoming  |
| Theme Switching                                                   | Upcoming  |
| Support for NixOS                                                 | Future    |

## Preparatory Requirements

The following guide requires the latest/git version of XMonad to be installed to avert recompilation errors from missing dependencies. For compatibility with the stable version (>= 0.17), consider removing [disableEwmhManageDesktopViewport](https://github.com/xmonad/xmonad-contrib/commit/cf13f8f9a7acddc1134be3f71097633def1476a8) in xmonad.hs, which is unavailable in said version at the time of writing.

## Installation Guidelines

### Automatic Installation (Arch Linux)

_This [setup script](./setup.sh) is offered as a convenience, but its use is at your own discretion (**unstable**). Manual installation is recommended for a safer and more controlled process, as it eliminates the risk of accidental overwrites or incongruous symlinks. As the author, I suggest manual installations to better respect the user's file structure. Please review before use._

```fish
wget https://raw.githubusercontent.com/NeshHari/XMonad/main/setup.sh
bash setup.sh
```

### Dependencies for Manual Installation

_Note: Paru is interchangeable with any AUR helper_

```fish
paru -Syu alacritty autorandr bat beautyline catppuccin-cursors-mocha catppuccin-gtk-theme-mocha cava dunst eww-git fd feh fish fzf ghc ghcup-hs-bin git glava haskell-utf8-string haskell-x11 jgmenu kitty lazygit libnotify lxappearance ttf-iosevka-nerd neovim nodejs polybar picom-pijulius-git playerctl python-pip rofi stack starship stow terminus-font ttf-font-awesome unclutter wmctrl xcape xdo xorg-xinit xorg-xmessage xorg-xmodmap xorg-xsetroot zoxide
```

_Note: Polybar will not automatically launch after installation. This is expected behavior. The [polybar launch script](./polybar/.config/polybar/startup.sh) is deliberately tied to autorandr, which is used for hot-plugging. Refer to the section on MyRescreen.hs to see how they all come together._

### Additional Fonts

- [Vanilla Caramel](https://www.dafont.com/vanilla-caramel.font)
- [Cream Cake](https://www.dafont.com/cream-cake.font)
- [Sugar Snow](https://www.dafont.com/sugar-snow.font)
- [Purple Smile](https://www.dafont.com/purple-smile.font)

_Note: Downloaded fonts should be placed in ~/.local/share/fonts, before running fc-cache -f -v to update the font cache._

### TUI/CLI Steps

_Note: Avoid using "stow \*\" unless on a fresh install. By default, measures are
in place so that you do not accidentally overwrite existing configurations.
I suggest manually copying configurations to the pertinent directory,
or selectively stowing certain directories (e.g. stow fish)
whilst in the root of the cloned folder._

```fish
git clone https://github.com/NeshHari/XMonad.git
mv XMonad starter_kit_dots
cd starter_kit_dots
rm README.md setup.sh
stow *
cd ~/.config/xmonad
rm -r xmonad xmonad-contrib
git clone https://github.com/xmonad/xmonad.git
git clone https://github.com/xmonad/xmonad-contrib.git
stack init
stack install
sudo ln -s ~/.local/bin/xmonad /usr/bin
xmonad --recompile && xmonad --restart
```

### Wallpaper

[Pots](./wallpapers/pots.jpg)

[Pots ~ Custom (multi-filtered)](./wallpapers/pots_custom.png)

[Dark Arts ~ Custom (multi-filtered)](./wallpapers/dark_arts_custom.png)

You may change the path to your wallpaper in ~/.config/xmonad/lib/Custom/MyStartupApps.hs

### Cursor

You may optionally install a custom cursor theme.
I alternate between [Oreo](https://aur.archlinux.org/packages/oreo-cursors-git),
which provides a handful of catppuccin-like colors with a little more contrast,
and the [Catppuccin Mocha Dark Cursors](https://aur.archlinux.org/packages/catppuccin-mocha-dark-cursors).
Once installed, the new cursor can be set with lxappearance.

### Spotify

You may use the [official package](https://aur.archlinux.org/packages/spotify)
or [Flatpak](https://flathub.org/apps/details/com.spotify.Client) to get spotify.

```fish
flatpak install flathub com.spotify.Client

flatpak run com.spotify.Client
```

To integrate with polybar and avoid the traceback error, you may need to run the following command:

```fish
pip install dbus-python
```

Additionally, to ensure polybar (main-middle) launches only when spotify is used and closes when it is not, please modify the desktop entry:

```shell
# PATH: ~/.local/share/applications/spotify.desktop (copy and modify /usr/share/applications/spotify.desktop so it persist after updates)

[Desktop Entry]
Type=Application
Name=Spotify
...
Exec=/home/myusername/.config/polybar/scripts/poly_spoti.sh
...
```

_Note: Configurations using [Spicetify](https://aur.archlinux.org/packages/spicetify-cli) are available [here](./spicetify/.config/spicetify/Themes/darkarts/) and [here](./spicetify/.config/spicetify/config-xput.ini) but require manual configuration on your end. Refer to the [official docs](https://spicetify.app/docs/getting-started) for more information._

### GTK

For GTK applications, open lxappearance and select the Catppuccin theme of choice. In the Widget section, set the default font to Sugar Snow/Purple Smile. In the Icon Theme section, select BeautyLine.

### Discord

Install [betterdiscord](https://aur.archlinux.org/packages/betterdiscord-installer) and inject the [CSS](./betterdiscord/darkarts_discord.css).

### Neovim

Launch Neovim and let it do its thing :) Watch out for those LSP/Treesitter prompts, and install what you need automatically.

### Pacman Command Aliases

Fish aliases such as "pacsi" and "parusi" help you fuzzy find official and AUR packages respectively with a preview panel consisting of all necessary information, right in your terminal. You can multi select or toggle select packages my marking or unmarking them using Tab. Likewise, you can remove existing packages in your system using "pacrem". Lastly, "pacupd" runs 'sudo pacman -Syu'.

## Compilation Advice and Troubleshooting

_Note: xorg-xmessage must be installed to view compilation errors_

In xmonad, there are several common recompilation errors that can occur while building a configuration. Some of these errors include:

- **Mutual recursion with imported modules** - When two or more functions are defined in terms of each other, creating an infinite loop, and the functions are defined in different imported modules. To fix this, you will need to ensure that the mutual recursion is not happening by importing the modules in a different order or by changing the function calls to not cause recursion.
- **Ambiguity Occurrences** - An ambiguity error in xmonad can occur when multiple functions with the same name are imported from different modules. To fix this, you can use a qualified import or use an import abbreviation (import module _as_ abbreviation) to specify which function to use.

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

- **Unresolved symbols** - When the compiler is unable to find the definition of a symbol that is referenced in the code. To fix this, you will need to import the necessary modules or define the symbol.

- **Type error** - When the type of a variable or expression does not match what is expected by the function or operation. To fix this, you will need to ensure that the types of the variables and expressions are consistent with what is expected.

- **Syntax error** - When the code does not conform to the Haskell syntax. To fix this, you will need to check for missing or extra characters, or any other syntax issues.

- **Missing type signatures/incomplete type annotations**- When the type signature of a function or variable is missing or not specified, the compiler may not have enough information to understand the expected behavior of the function or variable. This can lead to potential type errors during compilation. To avoid this, it's important to ensure that all functions and variables have their type signature specified. This way, the compiler can understand the inputs and outputs of the function or variable, and how it should behave. Although it's possible to suppress this warning by using the -Wno-missing-signatures flag when compiling the code, it's not recommended as it can lead to potential type errors in the future. For best practices, it's always recommended to include the type signature for functions and variables.
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

### Difference Between $ and . Operators

In Haskell, both the $ operator and the dot (.) operator are used to control the order of function application. However, they have different functionality.

The $ operator is used to apply a function to an argument without the need for parentheses. This operator has the lowest precedence of any operator in Haskell and is often used to make code more readable by allowing the argument to be placed at the end of a chain of function calls. For example, f(g(x)) can be rewritten as f $ g x.
On the other hand, the dot operator (.) is used to compose two functions together. This creates a new function that applies the right-hand function to the result of the left-hand function. An example of this would be f(g(x)) becoming (f . g) x. The dot operator has a higher precedence than the $ operator, so it will be evaluated first.

## Neovim Integration with Haskell Language Server (HLS)

Managing LSP servers in Neovim can be made easier by using the [Mason](https://github.com/williamboman/mason.nvim) plugin. A simpler option would be to install [LSP Zero](https://github.com/VonHeikemen/lsp-zero.nvim).

```haskell
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
}
```

_Note: My [lsp.lua](./nvim/.config/nvim/after/plugin/lsp.lua) file contains configuration information post-installation._

In order to ensure optimal compatibility between the Haskell Language Server (HLS) and Neovim, it is recommended to set up XMonad using the package manager stack or cabal. Installing HLS via Pacman or AUR may result in errors, such as "could not find module" or "unknown package" on import of modules, even if HLS is successfully attached and running on the Neovim buffer.

HLS offers a range of features including diagnostics, completions, code actions, and formatting. To avoid potential errors, I suggest installing HLS via "ghcup install hls," so that Mason can detect the executable when running ":MasonInstall haskell-language-server" in Neovim. A complete list of HLS features can be found at the website [here](https://haskell-language-server.readthedocs.io/en/latest/features.html).

Personally, my formatter of choice is [Ormolu](https://haskell-language-server.readthedocs.io/en/latest/features.html) due to its readability and consistency. It is also the default formatter for HLS. You may want to include a key mapping and/or autocommand for formatting in Neovim.

```lua
-- autocommand
vim.cmd [[autocmd BufWritePre * lua vim.lsp.buf.format({async = false})]]
-- manual format in normal mode (assuming vim.g.mapleader is set)
vim.keymap.set("n", "<leader>fo", vim.lsp.buf.format)
```

You may also include other formatters (e.g., fourmolu, brittany) by first installing them with Mason, and integrating them with null-ls as shown [here](./nvim/.config/nvim/after/plugin/nullls.lua).

Moving on, a minimal hie.yaml must be defined for HLS to function.

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

If XMonad was installed via stack, symlink or add the xmonad executable to PATH to make "xmonad --recompile/--restart" usable.

```bash
sudo ln -s ~/.local/bin/xmonad /usr/bin
```

## Modular Design

I utilize a modular structure to enhance accessibility, manageability, and testability in comparison to monolithic code. This method consists of dividing/breaking the code into smaller, independent modules that can be integrated as needed. In XMonad, these modules are typically imported from a folder named "lib", which is located in the same directory as the xmonad.hs file. If the folder does not already exist, it can be created. Users can create individual modules by creating files with the .hs extension within the lib folder or any of its subfolders (e.g. ./lib/Custom). These modules can then be imported directly into xmonad.hs or indirectly through other modules for use.

### Fundamental Principles

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
├── MyKeys.hs
├── MyLayouts.hs
├── MyMacAddresses.hs (hidden)
├── MyManagement.hs
├── MyManagementPositioning.hs
├── MyMouse.hs
├── MyPolybar.hs
├── MyScratchpads.hs
├── MyScreen.hs
├── MyStartupApps.hs
└── MyWorkspaces.hs
```

_Note: Ensure no mutually recursive modules exist, or XMonad will not compile. These are modules that import each other. For example, if you import Custom.MyScratchpads in MyManagement.hs, do not import Custom.MyManagement.hs in Custom.MyScratchpads. If the need arises, you can bypass this by extracting part of the module into an even simpler module, as seen in MyManagementPositioning.hs._

## MyCatppuccin.hs (Catppuccin Mocha)

In order to improve recognition and simplify the use of the "Catppuccin Mocha" [palette](https://github.com/catppuccin/catppuccin#user-content--palettes), the MyCatppuccin.hs file should define color variables for each color in the palette. Doing this prevents the need to memorise hex representations for each color. To prevent ambiguity when used in conjunction with other color schemes with the same variable name, it is recommended to prepend the color variables with something unique to that color scheme, such as "cat". For example, "catBlue" and "nordBlue" are different, but using just "blue" creates ambiguity errors. Refer to the "Recompilation Tips" subsection for other methods to prevent ambiguous occurrences.

```haskell
module Custom.MyCatppuccin where

catRosewater, catFlamingo, catPink, catMauve, catRed, catMaroon, catPeach, catYellow, catGreen, catTeal, catSky, catSapphire, catBlue, catLavender, catText, catSubtext1, catSubtext0, catOverlay2, catOverlay1, catOverlay0, catSurface2, catSurface1, catSurface0, catBase, catMantle, catCrust :: String
catRosewater = "#f5e0dc"
catFlamingo = "#f2cdcd"
catPink = "#f5c2e7"
catMauve = "#cba6f7"
catRed = "#f38ba8"
catMaroon = "#eba0ac"
catPeach = "#fab387"
catYellow = "#f9e2af"
catGreen = "#a6e3a1"
catTeal = "#94e2d5"
catSky = "#89dceb"
catSapphire = "#74c7ec"
catBlue = "#89b4fa"
catLavender = "#b4befe"
catText = "#cdd6f4"
catSubtext1 = "#bac2de"
catSubtext0 = "#a6adc8"
catOverlay2 = "#9399b2"
catOverlay1 = "#7f849c"
catOverlay0 = "#6c7086"
catSurface2 = "#585b70"
catSurface1 = "#45475a"
catSurface0 = "#313244"
catBase = "#1e1e2e"
catMantle = "#181825"
catCrust = "#11111b"
```

## MyStartupApps.hs

The main difference between the spawn and spawnOnce functions in XMonad is that spawn will start the specified command every time it is called, while spawnOnce will start the specified command only the first time it is called, and subsequent calls will have no effect. Despite traditional use cases where the wallpaper is set only once, some users might choose to set it every time xmonad is restarted so as to better support monitor hot-plugging. This ensures that the wallpaper will be set correctly even if a new monitor is plugged in or if the resolution of the monitor changes. This assumes the user utilises something like "feh bg-scale" in order to scale the wallpaper accordingly.

```haskell
module Custom.MyStartupApps where

import XMonad
import XMonad.Util.SpawnOnce

myStartupHook :: X ()
myStartupHook = do
  spawn "feh --bg-scale ~/wallpapers/dark_arts_custom.png"
  spawn "~/scripts/feh-blur.sh -s; ~/scripts/feh-blur.sh -d"
  spawn "killall picom; picom -b"
  spawnOnce "easyeffects --gapplication-service &"
```

## MyScreen.hs

Hot plugging is a crucial feature for any multi-monitor workflow as it allows users to seamlessly add or remove monitors without disrupting their workflow. XMonad provides a [custom hook](https://xmonad.github.io/xmonad-docs/xmonad-contrib-0.16.999/XMonad-Hooks-Rescreen.html) that is best used in conjunction with other tools such as [autorandr](https://github.com/phillipberndt/autorandr), which automatically selects a predefined configuration dependent on the number of connected displays. It works by listening for RandR events and performing a defined action such as restarting xmonad when the number of screens changes. Together they provide a robust solution for monitor hot-plugging.

### Autorandr

Required package: [autorandr](https://archlinux.org/packages/community/any/autorandr/)

Firstly, ensure autorandr detects all possible monitor combinations. In this example, I shall provide single and dual monitor setups. This assumes the layouts (i.e., portrait/landscape & positioning) are already set up.

With only one display on, run the following command:

```fish
autorandr --save single
```

_Note: There may be cases, particularly on boot, where the second monitor is turned off (i.e., black screen) but connected to a power supply and display port. In cases where xrandr does not correctly detect the screen resolution, you may want to force the second monitor to be off before saving the "single" profile. In my setup, DP-1 is the second monitor I want off, whilst the specified resolution is for the monitor I wish to utilise._

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

Workspaces are a powerful feature that allow users to organize and manage their open applications in a highly customizable way. The names of these workspaces can be chosen according to personal preference. Utilizing [XMonad.Layout.ShowWName](https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Layout-ShowWName.html) via the layout hook will display the assigned names on the screen when switching workspaces. It is important to note that the names assigned to workspaces must also be reflected in the Polybar configuration, particularly if icons are utilized. The XMonad.Layout.ShowWName package provides additional functionality and customization options which are covered in the [Custom.MyDecorations](./xmonad/.config/xmonad/lib/Custom/MyDecorations.hs) module.

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

The XMonad window manager provides a number of powerful tools for managing and positioning windows, including the [ManageHelpers](https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Hooks-ManageHelpers.html) module. This module can be particularly useful for scenarios where certain windows need to be automatically set to full-screen, float in the center of the screen, or spawn on a specific workspace.

To utilize the ManageHelpers module, the first step is to identify the appName, className, or resource of the target window. These terms refer to different aspects of the window's properties, and are described in more detail on the Hackage documentation page for the [XMonad.ManageHook](https://hackage.haskell.org/package/xmonad-0.17.1/docs/XMonad-ManageHook.html) module. One way to obtain the WM_CLASS value for a window is to run the command "xprop | grep 'CLASS'" in the terminal, and then click on the desired window with the cursor.

Quick Peek at Referenced Differences:

```haskell
appName :: Query String
Return the application name, i.e., the first String returned by WM_CLASS.

resource :: Query String
Backwards compatible alias for appName.

className :: Query String
Return the resource class, i.e., the second String returned by WM_CLASS.
```

Once the window's properties have been identified, the ManageHelpers module can be used to apply various management rules. The composeAll function can be used to execute all matching rules, while composeOne will only execute the first match. Additionally, the <&&> operator can be used to apply the same manage helper to multiple windows.

For example, to shift a window to a specific workspace, the doShift function can be used, followed by the name of the desired workspace. It is important to note that the workspace names must already exist, and can be defined using the MyWorkspaces module.

Finally, it's worth mentioning that manage helpers are functions that should be used with the manageHook. Therefore, they must be added back to the manageHook after they are defined in the MyManagement module.

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

In addition to the MyManagement module, the MyManagementPositioning module can be used to handle the positioning of floating windows such as scratchpads. To keep windows centered while varying their size, it's recommended to only modify the width and height "size" variables, while ignoring the "distance" variables defined in the fromLeft and fromTop. The purpose of extending the existing Custom.MyManagement module is to avert mutual recursion between Custom.MyManagement and Custom.MyScratchpads. See the Recompilation Tips subsection for more information on mutual recursion.

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

Scratchpads are a useful feature in XMonad that allow users to temporarily hide and show floating windows as needed. One common use case for scratchpads is running quick terminal commands through a terminal emulator such as Alacritty, rather than the primary terminal (e.g. Kitty) that is already open. This is because scratchpads are dependent on the WM_CLASS property of a window, and if a terminal with the same WM_CLASS as a scratchpad (e.g. "kitty") is already open, XMonad may hide the tiled window instead of the scratchpad.

The code snippet below illustrates an example of how to define and manage scratchpads using the NamedScratchpad module from the xmonad-contrib package. The example defines two scratchpads, "quick commands" and "glava", and assigns them specific spawn and find commands. This allows the user to easily toggle these scratchpads using the defined hotkeys.

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
```

Another consideration when using scratchpads is hiding the "NSP" workspace that appears in the polybar after the first spawn of a named scratchpad. One solution is to set the icon for the NSP workspace to empty in the polybar's config.ini file. Or alternatively, one can filter out the workspace using the addEwmhWorkspaceSort function and filterOutWs as shown in the snippet of xmonad.hs

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
```

Overall, scratchpads are a powerful feature that can improve workflow and organization in XMonad, but it's important to keep in mind the WM_CLASS dependencies and the potential issues with the NSP workspace when configuring them.

## MyLayouts.hs

The Custom.MyLayouts module provides a starting point for customizing the layout and appearance of windows on the screen. This module allows users to select from a variety of layouts, such as "ResizableTall", "Tall", "Column", "BSP", "Accordion", Full", and customize them to suit their workflow.

_Note: I have disabled the missing signature warning for this module due to the complexity in defining type signatures for the present variables._

```haskell
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Custom.MyLayouts where

import Custom.MyDecorations
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
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

accordion =
  renamed [XLR.Replace "Accordion"] $
    avoidStruts $
      windowNavigation $
        addTabs shrinkText myTabConfig $
          subLayout [] tabs $
            mySpacing 7 Accordion

bsp =
  renamed [XLR.Replace "BSP"] $
    avoidStruts $
      windowNavigation $
        addTabs shrinkText myTabConfig $
          subLayout [] tabs $
            mySpacing 7 emptyBSP

full = renamed [XLR.Replace "Monocle"] $ noBorders Full

myLayout = boringWindows (ifWider 1080 (tall ||| bsp) (column ||| accordion) ||| full)

myLayoutHook =
  showWName' myShowWNameConfig $
    smartBorders $
      mkToggle
        (NOBORDERS ?? FULL ?? EOT)
        myLayout
```

One layout that is particularly useful is "ResizableTall", as it permits the modification of window height and width. When using layouts that are not full-screen, it is important to avoid struts, as they can cause docks (such as polybar) to overlap the layouts or vice versa. To address this issue, I have added a key map that allows the user to toggle the polybar and struts manually. Additionally, it's possible to rename the layouts, which will affect how the current layout name is displayed in the polybar.

Snippet of key map:

```haskell
("M-C-<Space>", spawn "polybar-msg cmd toggle" >> sendMessage ToggleStruts),
```

### PerScreen

In the case of a dual monitor setup, it may be useful to use different layouts for each monitor. For example, using the "Tall" or "BSP" layouts on a portrait monitor (1080 x 1920) might not be practical. Therefore, I specify that only if the screen resolution is wider than 1080, I use "Tall" or "BSP", otherwise I use "Column" or "Accordion" layout. Additionally, for both monitors I use "Full" since I would like full screen support for both.

```haskell
myLayout = boringWindows (ifWider 1080 (tall ||| bsp) (column ||| accordion) ||| full)
```

### SubLayouts and BoringWindows

Another useful feature in XMonad is the use of sublayouts and boring windows. Sublayouts are layouts within a layout, and can be used to group windows together. In this example, I use tabs as a sublayout for "Tall" and "Column", but other options such as "Accordion" could also be used. Additionally, BoringWindows allow you to skip over windows in a sublayout when moving focus. I use a separate key map to navigate through windows confined in a sublayout.

Snippet of key map:

```haskell
("M-C-.", onGroup W.focusUp'),
("M-C-,", onGroup W.focusDown')
```

### Enhanced Borders

The XMonad window manager provides several options for customizing the appearance of window borders, including the "smartBorders" and "noBorders" layout transformers. These transformers can be used to improve the appearance and functionality of the borders, depending on the number of windows and the layout being used.

The "smartBorders" layout transformer is particularly useful when borders are not needed when only a single window is present. This transformer automatically hides the borders when there is only one window, and makes them visible again when more than one window is present. This can greatly improve the appearance and usability of the window manager, as it eliminates the need for unnecessary borders when working with a single window.

Another useful layout transformer is "noBorders", which can be used to altogether remove the border from full-screen layouts. This transformer can be dynamically applied and removed by using the "mkToggle" function, which allows the user to toggle the border on and off as needed. This can be achieved using the following code snippet:

```haskell
smartBorders $
  mkToggle
    (NOBORDERS ?? FULL ?? EOT)
```

_Note: EOT simply marks the end of transformer_

The [XMonad.Layout.NoBorders](https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Layout-NoBorders.html) package provides more information on how to configure and use these transformers

### Gaps Gaps Gaps

Snippet of reference:

```haskell
mySpacing i = spacingRaw False (Border 10 10 30 30) True (Border i i i i) True
```

"XMonad.Layout.Spacing" is a Haskell module for the XMonad Window Manager. It provides the functionality to add a configurable amount of space around windows. You can use this module by importing it into your xmonad.hs configuration file and then using the spacing or spacingWithEdge commands to specify the amount of spacing you want to add around your windows.

The spacingWithEdge command adds spacing around both windows and the edges of the screen, while spacing only adds spacing around windows. There is also the spacingRaw command, which provides more fine-grained control over the spacing. The spacingRaw command takes 5 arguments:

- A Boolean indicating whether to enable the smartBorder, which ensures that borders are not applied if there is only one window on the screen.
- The screenBorder, which specifies the amount of space to add around the edges of the screen.
- A Boolean indicating whether the screenBorder is enabled.
- The windowBorder, which specifies the amount of space to add around each window.
- A Boolean indicating whether the windowBorder is enabled.
- It is important to note that if you use the avoidStruts layout modifier, it must come before any of the spacing modifiers.

## Polybar Integration

The integration of Polybar with XMonad has been greatly simplified with the release of the [XMonad.Hooks.StatusBar](https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Hooks-StatusBar.html) and [XMonad.Hooks.StatusBar.PP](https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Hooks-StatusBar-PP.html), modules. These modules provide a convenient way to display important information such as the current workspace and layout in Polybar. Let us analyse these module separately.

### EWMH Module

To get started, the Polybar configuration requires two modules: "ewmh" and "xmonad". The "ewmh" module queries the EWMH desktops configured by XMonad, and is responsible for displaying the current workspace and its associated icon in Polybar. It is important to note that the workspace names must match exactly with the names defined in the MyWorkspaces module. Additionally, it is recommended to use the disableEwmhManageDesktopViewport function to prevent any issues with the ordering of workspaces, especially when using a multi-Polybar instance workflow.

To my knowledge, while the EWMH module can display information such as the current workspace and its associated icon, it does not support the display of window titles or other information such as the current monitor layout. To display window titles, use module/title of type internal/xwindow or module/xmonad.

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

The "xmonad" module, on the other hand, uses the xmonadpropread script found in the xmonad-contrib package to execute property logging via xmonadPropLog. This allows for the creation of a formatted string, such as the current layout, which is then written to \_XMONAD_LOG and further processed by the Polybar module/xmonad. This module also allows for the display of window titles in Polybar.

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

With the rudiments of xmonadPropLog and dynamicLogString covered in the previous subsection, we can move on to customising the formatted string discussed earlier. At this stage in our example, we only have workspace icons. Optionally, we discussed how to include window titles. Although all of which can be made available by modifying the ppOrder to include "ws" (i.e., workspace) and "t" (i.e., title), it is much more tedious to do so since we need to find the exact icon code rather than simply copy and paste the icon of choice. Therefore, I personally prefer the current approach of using two distinct modules (i.e., ewmh and xmonad) in polybar. Continuing our example, I only wish to add the current layout ("l") as defined in ppOrder. However, I would like polybar to assign the colors as it would for any module via "format-foreground". I also do not wish to wrap the current layout with anything (e.g. [Tall], >Tall<, etc.). Therefore, I set the text color of the layout information by defining the textColor variable, and ignoring wraps.

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
      sbStartupHook = spawn "~/.config/polybar/launch.sh",
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

## Event Management

_Note: This section is a summary of the key points from the official documentation._

We will use this example/snippet to comprehend how to apply the windowed fullscreen hack to Chromium-based applications, onXPropertyChange, and the swallowEventHook.

```haskell
myEventHook = swallowEventHook (className =? "kitty") (return True) <> onXPropertyChange "WM_NAME" myManageHook <> Hacks.windowedFullscreenFixEventHook
```

### Event Swallowing Hook

XMonad.Hooks.WindowSwallowing is a module that provides a handleEventHook that enables window swallowing functionality. This feature allows for the reduction of unnecessary screen space usage by detecting and "swallowing" parent windows when a new window is opened from within them. The module utilizes the pstree command to analyze the process hierarchy, but it is important to note that this does not always work perfectly. Some applications that implement instance sharing, such as some terminal emulators and tmux, cannot be supported by window swallowing. Additionally, the module requires the \_NET_WM_PID X-property to be set in order to check the process hierarchy, so some child programs may not be supported. To use this module, it must be imported into your xmonad.hs file and the swallowEventHook function can be added to your handleEventHook. The variant swallowEventHookSub can also be used if a layout from XMonad.Layouts.SubLayouts is employed, allowing for the merging of child windows with the parent instead of swallowing.

### X Property Change Event

The XMonad.Hooks.OnPropertyChange module is designed to allow for the management of already-mapped windows based on changes in their properties. This is particularly useful for identifying and managing browser windows by title, as these properties may not be fully set until after the window has been mapped and all associated documents and scripts have loaded. Additionally, this module can be used in conjunction with Electron applications, such as Spotify, that set their WM_CLASS properties at a later stage, making it difficult for traditional window managers to properly map them. This module utilizes a handleEventHook that triggers on PropertyChange events, and currently does not take into account properties that may have been removed.

### Fullscreen Hack for Windowed Applications (Chromium)

The XMonad.Util.Hacks module provides a set of utility functions for customizing and optimizing the behavior of XMonad for certain applications. The windowed fullscreen hack is a feature provided by the XMonad.Util.Hacks module that addresses an issue with certain Chromium-based applications, such as Chrome, Discord, and others, when they request to be put into fullscreen mode. These applications may not correctly detect the size of the window when displaying fullscreen content, resulting in cut-off content.

The windowed fullscreen hack works by forcing the window to recalculate its dimensions after initiating fullscreen mode. This allows Chromium-based applications to correctly display fullscreen content within their normal window dimensions.

To utilize this feature, it can be added to the handleEventHook function in the xmonad config file (i.e., xmonad.hs), as shown in the example provided. It will apply and undo a resize quickly causing chromium to recalculate the fullscreen window dimensions to match the actual windowed fullscreen dimensions.

## Java Application Hack

The Java Hack is a feature provided by the XMonad.Util.Hacks module that addresses compatibility issues with certain Java-based applications that may not function correctly when used in conjunction with XMonad.

A common workaround for this issue is to set the environment variable \_JAVA_AWT_WM_NONREPARENTING to 1. The javaHack function in the XMonad.Util.Hacks module automatically sets this variable, providing a simple and convenient solution for users experiencing compatibility issues with Java applications.

To utilize this feature, it can be added to main in the xmonad config file (i.e., xmonad.hs), as shown in the snippet provided. There is no longer a need to define it in your xinitrc.

```haskell
main =
  do
    xmonad
    $ Hacks.javaHack
```

## Hyper Keys

Inspired by Ethan Schoonover's [video](https://www.youtube.com/watch?v=70IxjLEmomg)...

Required package: [xcape](https://archlinux.org/packages/community/x86_64/xcape/)

The following is achieved:

- Caps to Escape (for vim use)

- Holding down Caps (i.e., Esc) acts as Ctrl (easy to Ctrl-f for completion etc.)

- Holding down either Tab or Backslash acts as Windows key (i.e., Mod4Mask).

1. .xinitrc

```sh
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
xcape -e 'Control_L=Escape' -t 1000
```

2. .Xmodmap

```sh
remove Lock = Caps_Lock
keycode 66 = Control_L
add Control = Control_L

! Tab as modifier
keycode 23 = Tab Hyper_L

! Backslash(\) as modifier + preserve bar(|)
keycode 51 = backslash bar Hyper_L

add mod4 = Hyper_L
```

## Emacs-Like Key Bindings

The XMonad.Util.EZConfig module is a potent enabler of customization for the XMonad window manager. Its functions, such as additionalKeysP and additionalMouseBindings, empower users to effortlessly augment their XMonad configuration with new, personally tailored key and mouse bindings. These functions offer an intuitive means of sculpting XMonad's behavior to perfectly align with the user's preferences. Moreover, the mkKeymap function in this module provides an effortless way to amalgamate multiple existing keymaps, leading to an XMonad experience that is uniquely attuned to the user's needs. The XMonad.Util.EZConfig module is therefore a vital component in the pursuit of an optimally personalized XMonad window management experience. Refer to [MyKeys.hs](./xmonad/.config/xmonad/lib/Custom/MyKeys.hs) for a more technical perspective.

## Vim-Like Easy Motion

The XMonad.Actions.EasyMotion module is a component of the XMonad window manager that simplifies navigation between windows. The module provides the selectWindow function which allows the user to quickly select a window from a list of all windows currently present on the screen. The function takes an input pattern and highlights all windows that match the pattern, allowing the user to select the desired window and move the focus to it. By offering a fast and efficient way to navigate between windows, the selectWindow function of the XMonad.Actions.EasyMotion module provides a convenient alternative to traditional methods of window navigation in XMonad.

Snippet of bindings:

```haskell
("M-g", selectWindow emConf >>= (`whenJust` windows . W.focusWindow)),
("M-x", selectWindow emConf >>= (`whenJust` killWindow)),
```

## Cycle Through Workspaces

The XMonad.Actions.CycleWS module provides functionality for cycling between workspaces in the XMonad window manager. The module provides the cycleRecentWS and cycleRecentWSUp functions, which allow the user to switch between workspaces in a cyclical manner, moving forward and backward through the list of workspaces, respectively. The module also provides the nextScreen and prevScreen functions, which allow the user to cycle between physical screens connected to the system. The module offers a convenient way to manage workspaces and navigate between physical screens in XMonad, streamlining the process of organizing windows and tasks.

Snippet of bindings:

```haskell
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
    ("M-k", focusUp),
    ("M-j", focusDown),
    ("M-m", focusMaster),
    ("M-S-<Return>", windows W.swapMaster),
    ("M-S-j", swapDown),
    ("M-S-k", swapUp),

```

## Flexible Mouse Resize

XMonad.Actions.FlexibleResize provides a convenient method for resizing floating windows in XMonad, allowing the user to resize from any corner of the floating window. With this module, instead of being limited to resizing from a specific edge or corner (i.e., bottom-right), the user can click and drag any part of the window to resize it, making the resizing process more flexible and intuitive.

```haskell
import Data.Map qualified as M
import XMonad
import XMonad.Actions.FlexibleResize qualified as Flex
import XMonad.StackSet qualified as W

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    [ ( (modm, button1),
        \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
      ),
      ((modm, button2), \w -> focus w >> windows W.shiftMaster),
      ( (modm, button3),
        \w -> focus w >> Flex.mouseResizeWindow w >> windows W.shiftMaster
      )
    ]
```

## Search Engine & Other Prompts

The XMonad.Actions.Search module provides a way to efficiently search the web by launching queries via XMonad.

Snippets of search engine bindings:

``

```haskell
-- Search commands (wait for next keypress)
("M-s", SM.submap $ searchEngineMap $ S.promptSearchBrowser myPromptConfig myBrowser),
```

```haskell
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
```

This code sets up a keyboard shortcut "Mod + s" in Xmonad. When this shortcut is pressed, a search prompt will appear, allowing you to search various websites. The behavior of the search prompt and the search process is defined by the "myPromptConfig" and "myBrowser" parameters passed to the "S.promptSearchBrowser" function.

The "searchEngineMap" method maps specific key presses to specific search engines. For example, when you press "Mod + s" **then** "g", you'll be searching Google. And if you press "Mod + s" **then** "w", you'll be searching Wikipedia. The "S.searchEngine" function takes two arguments: the name of the search engine and the URL format used to search that engine. Those undefind are provided by XMonad. You may view them [here](https://xmonad.github.io/xmonad-docs/xmonad-contrib/XMonad-Actions-Search.html).

So, to put it simply, this code sets up a keyboard shortcut "Mod + s" in Xmonad that opens a search prompt. By pressing different keys after the initial "Mod + s" shortcut, you can search various websites.

Snippet of prompt bindings:

```haskell
("M-s-m", manPrompt myPromptConfig),
("M-s-x", xmonadPrompt myPromptConfig),
("M-S-q", confirmPrompt myPromptConfig "exit" $ io exitSuccess),
```

Similarly, the XMonad-Prompt library provides functions for creating interactive prompts in Xmonad. It offers support for input fields, selection lists, and password inputs. Customization options for the look and behavior of prompts are available in [MyDecorations.hs](./xmonad/.config/xmonad/lib/Custom/MyDecorations.hs).
