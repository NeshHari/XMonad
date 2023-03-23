function lag --wraps='exa -lah --color-scale --icons --grid' --wraps='exa -laG --color-scale --icons' --description 'alias lag=exa -laG --color-scale --icons'
  exa -laG --color-scale --icons $argv
        
end
