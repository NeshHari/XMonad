function ll --wraps='exa -lah' --wraps='exa -lah --color-scale --icons' --description 'alias ll=exa -lah --color-scale --icons'
  exa -lah --color-scale --icons $argv
        
end
