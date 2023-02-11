#!/usr/bin/env bash

# Welcome message
echo "Welcome to the dotfiles installation script!"
echo "Please choose your AUR helper:"
echo "[1] yay"
echo "[2] paru"

# Get user's choice of AUR helper
read -p "Enter your choice: " aur_choice
if [ "$aur_choice" == "1" ]; then
	aur_helper="yay"
elif [ "$aur_choice" == "2" ]; then
	aur_helper="paru"
else
	echo "Invalid choice. Exiting..."
	exit 1
fi

# Check if AUR helper exists
if ! command -v "$aur_helper" >/dev/null; then
	echo "$aur_helper not found. Please install it first."
	exit 1
fi

# Packages to install
packages=(
	alacritty
	autorandr
	bat
	beautyline
	catppuccin-cursors-mocha
	catppuccin-gtk-theme-mocha
	cava
	dunst
	eww
	fd
	feh
	fish
	fzf
	ghc
	ghcup-hs-bin
	git
	glava
	haskell-utf8-string
	haskell-x11
	jgmenu
	kitty
	lazygit
	libnotify
	lxappearance
	neovim-git
	nodejs
	polybar
	picom
	playerctl
	python-pip
	rofi
	stack
	starship
	stow
	terminus-font
	ttf-font-awesome
	ttf-iosevka-nerd
	ttf-material-icons-git
	unclutter
	wmctrl
	xcape
	xdo
	xorg-xinit
	xorg-xmessage
	xorg-xmodmap
	xorg-xsetroot
	zoxide
)

# Install packages
$aur_helper -Syu ${packages[@]}

# Clone dotfiles and stow
echo "Cloning and stowing dotfiles..."
cd $HOME
git clone https://github.com/NeshHari/XMonad.git
mv XMonad starter_kit_dots
cd starter_kit_dots
rm README.md setup.sh
stow_dirs=()
for dir in *; do
	if ! stow $dir; then
		stow_dirs+=($dir)
	fi
done

# Check if there are any failed directories
if [ ${#stow_dirs[@]} -gt 0 ]; then
	echo "The following directories could not be stowed: ${stow_dirs[@]}"
	echo "Please manually copy the respective configs."
fi
cd ~/.config/xmonad
rm -r xmonad xmonad-contrib
git clone https://github.com/xmonad/xmonad.git
git clone https://github.com/xmonad/xmonad-contrib.git
if pacman -Qi xmonad &>/dev/null; then
	read -p "xmonad binary already exists. Do you want to remove it to continue? (y/n) " remove_xmonad
	if [ "$remove_xmonad" == "y" ]; then
		sudo pacman -Rns xmonad
	fi
fi

if pacman -Qi xmonad-contrib &>/dev/null; then
	read -p "xmonad-contrib already exists. Do you want to remove it to continue? (y/n) " remove_xmonad_contrib
	if [ "$remove_xmonad_contrib" == "y" ]; then
		sudo pacman -Rns xmonad-contrib
	fi
fi

if [ "$remove_xmonad" == "n" ] || [ "$remove_xmonad_contrib" == "n" ]; then
	echo "xmonad and xmonad-contrib must be removed before installing via stack. Exiting..."
	exit 1
fi

if ! stack init; then
	read -p "stack init failed. Do you want to try 'stack init --force'? (y/n) " force_init
	if [ "$force_init" == "y" ]; then
		stack init --force
	fi
fi

stack install

if ! [ -x "$(command -v xmonad)" ]; then
	echo "xmonad binary not found"
	read -p "Do you want to symlink xmonad to /usr/bin or add it to PATH in .bashrc? (s/p) " link_choice
	if [ "$link_choice" == "s" ]; then
		sudo ln -s ~/.local/bin/xmonad /usr/bin
	elif [ "$link_choice" == "p" ]; then
		echo 'export PATH="$HOME/.local/bin:$PATH"' >>~/.bashrc
		source ~/.bashrc
	else
		echo "Invalid choice. xmonad binary will not be added to PATH. Please do it manually."
	fi
fi

xmonad --recompile
xmonad --restart
echo "Installation complete! Enjoy your new dotfiles :)"
