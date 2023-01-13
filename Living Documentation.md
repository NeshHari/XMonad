---
id: "Living Documentation.md"
aliases:
  - "Linux Distribution"
tags: []
---

# Linux Distribution
Vanilla Arch
Package Manager: Pacman
AUR Helper: Paru

# File Management
## Dotfiles:
### [GNU Stow ](https://github.com/aspiers/stow)
The "symlink farm manager" to facilitates file backup.

## Git:
### [lazygit](https://github.com/jesseduffield/lazygit)
Terminal UI for git commands.

# Terminal
## Emulator:
### [kitty](https://github.com/kovidgoyal/kitty)
GPU-based terminal emulator extensible by kittens (i.e., plugins), and supports layouts and tabs.

## Shell:
### Default Shell: [dash](https://wiki.archlinux.org/title/Dash)
Modern POSIX-compliant implementation of /bin/sh that is 4 times faster than Bash. Re-symlink /bin/sh to /bin/dash for improved performance.

### User Shell: [fish](https://github.com/fish-shell/fish-shell)
Interactive shell that supports suggestions, syntax highlighting, and tab completions out of the box. Recommended to launch shell with terminal instance (e.g., kitty -e "fish" or add "shell fish" to kitty.conf).

#### [fisher](https://github.com/jorgebucaran/fisher)
Plugin manager for fish.

## Modern Command Line Tools:
### [fd](https://github.com/sharkdp/fd)
Rust implementation of "find" that is faster due to parallelised directory travel, supporting majority of use cases. Hidden files and directories are hidden by default. Smart-casing is also supported.

### [exa](https://github.com/ogham/exa)
Rust replacement for "ls" featuring colour with parallelisation featuring colour and git support. 

### [bat](https://github.com/sharkdp/bat)
Clone of cat with syntax highlighting and integration with other tools (i.e., find, fd, fzf, git, etc.)

### [tldr-pages](https://github.com/tldr-pages/tldr)
Simpler and more approachable man pages.

### [zoxide](https://github.com/ajeetdsouza/zoxide)
Navigate (i.e., "jump")  to most frequently used directories, replacing "cd".

### [fzf](https://github.com/junegunn/fzf)
Interactive multipurpose Unix filter and fuzzy finding utility.

# Editor
### [neovim](https://github.com/neovim/neovim)

[telescope](https://github.com/nvim-telescope/telescope.nvim)

[treesitter](https://github.com/nvim-treesitter/nvim-treesitter)

[harpoon](https://github.com/ThePrimeagen/harpoon)

[undotree](https://github.com/mbbill/undotree)

[lsp-zero](https://github.com/VonHeikemen/lsp-zero.nvim)



# About
Pardon the informality in this "about" section. If you are reading this you probably already know what [XMonad](https://xmonad.org/) is. Well if you don't, its a dynamic tiling window manager for [X Windows System (X11)](https://wiki.archlinux.org/title/xorg) that is written, configured, and fully extensible in Haskell (sincere apologies for the somewhat plagiarised description). Anyways, the point is, its Haskell: the preeminent reason for staying clear of this WM. After a week of blood, sweat, and tears,

##
The following guide requires the latest/git version of XMonad to be installed to avert recompilation errors from missing dependencies. For compatibility with the stable version (>= 0.17), consider removing [disableEwmhManageDesktopViewport](https://github.com/xmonad/xmonad-contrib/commit/cf13f8f9a7acddc1134be3f71097633def1476a8) in xmonad.hs, which is unavailable at the time of writing.

## Setup
For compatibility with the [Haskell Language Server (HLS)](https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#user-content-hls) provided by Neovim's native LSP, XMonad must be [setup](https://xmonad.org/INSTALL.html) using stack or cabal. Installing via pacman/AUR will result in "could not find module" or "unknown package" errors on import of any module, despite HLS successfully attaching and running on Neovim buffer.  HLS provides various features such as diagnostics, completions, code actions, and formatting. As a personal choice [Ormolu](https://haskell-language-server.readthedocs.io/en/latest/features.html)  is utilised. The complete list of features is provided [here](https://haskell-language-server.readthedocs.io/en/latest/features.html).

Additionally, a minimal hie.yaml must be defined as follows for HLS to function.
```yaml
cradle:
	stack:
```

Expected folder structure:
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

Note: If XMonad was installed via stack, symlink or add the xmonad executable to PATH to make "xmonad --restart" usable.
```bash
sudo ln -s ~/.local/bin/xmonad /usr/bin
```

### Modularisation
To improve accessibility and testing in comparison to monolithic code, code is separated into independent modules that are integrated as required in xmonad.hs.  Modules in XMonad are read from the "lib" folder by default, which can be created in the same directory as xmonad.hs. Individual modules (i.e., files with .hs extension) can then be created directly in that folder or subfolders within, and imported in xmonad.hs. In this example, 

How it works?

*Note: MyModule is replaceable by the name of the module*
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

### Custom Modules:
General Path: ./lib/Custom/MyModuleName.hs, where "." is relative to where xmonad.hs resides.

#### MyCatppuccin.hs  (Catppuccin Mocha)
Create color variables for the "Catppuccin Mocha" [palette](https://github.com/catppuccin/catppuccin#user-content--palettes) due to their simplicity in recognition compared to hex representations. Prepend color variables with something unique to that colorscheme such as "cat", to prevent ambiguity when used in conjuction with other colorschemes with the same variable name. For example, catBlue and nordBlue are different, but using just "blue" creates amguity errors.
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
#### MyStartupApps.hs
Launch startup applications/scripts like setting wallpaper etc. 
```haskell
module Custom.MyStartupApps where

import XMonad
import XMonad.Hooks.SetWMName  
import XMonad.Util.SpawnOnce

myStartupHook :: X ()
myStartupHook = do
  spawn "~/.fehbg"
  -- supports hyper keys
  spawnOnce "xmodmap ~/.Xmodmap"
  spawn "killall picom; picom -b"
  spawn "~/feh-blur.sh -s; ~/feh-blur.sh -d"
  spawn "easyeffects --gapplication-service &"
  -- export _JAVA_AWT_WM_NONREPARENTING=1 instead of LG3D
  -- setWMName "LG3D"

```

### User-Specific Workflow Configuration 

#### Hyper Keys
Inspired by Ethan Schoonover's [video](https://www.youtube.com/watch?v=70IxjLEmomg)...

The following is achieved:
	Caps to Escape (for vim use). 
	Holding down Caps acts as Ctrl (easy to Ctrl-f for shell completion etc.)
	Holding down either Tab or Backslash acts as Windows key (i.e., Mod4Mask).

Files used (ensure xcape is installed):
1. .xinitrc
	setxkbmap -option "caps:ctrl_modifier" &
	xcape -e 'Caps_Lock=Escape' &

2. .Xmodmap (called in XMonad startupApps)
	! Tab as modifier
	keycode 23 = Tab Hyper_L
	
	! Backslash(\) as modifier + preserve bar(|)
	keycode 51 = backslash bar Hyper_L
	
	add mod4 = Hyper_L
	
	clear lock


*Note: Ensure xorg-xmessage is installed to view compilation errors.*

Windowed 
[XMonad.Hooks.EwmhDesktops](https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Hooks-EwmhDesktops.html) + 

