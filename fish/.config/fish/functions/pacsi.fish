function pacsi --wraps=pacman\ -Slq\ \|\ fzf\ --multi\ --preview\ \'pacman\ -Si\ \{1\}\'\ \|\ xargs\ -ro\ sudo\ pacman\ -S --description alias\ pacsi=pacman\ -Slq\ \|\ fzf\ --multi\ --preview\ \'pacman\ -Si\ \{1\}\'\ \|\ xargs\ -ro\ sudo\ pacman\ -S
  pacman -Slq | fzf --multi --preview 'pacman -Si {1}' | xargs -ro sudo pacman -S $argv; 
end
