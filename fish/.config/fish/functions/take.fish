function take --wraps='mkdir -p $argv;cd $argv' --description 'Create a directory and cd into it'
  mkdir -p $argv
  cd $argv
end
