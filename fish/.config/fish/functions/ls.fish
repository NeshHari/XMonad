function ls --wraps=exa --wraps='exa --color=auto' --wraps='exa --color-scale --icons' --description 'alias ls=exa --color-scale --icons'
  exa --color-scale --icons $argv
        
end
