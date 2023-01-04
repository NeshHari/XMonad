function pacupd --wraps='sudo pacman -Syu' --description 'alias pacupd=sudo pacman -Syu'
  sudo pacman -Syu $argv; 
end
