#General exports for all shells

export GEM_HOME="$HOME/gems"
export PATH="$PATH:$HOME/bin:$(ruby -e 'print Gem.user_dir')/bin"
export QUOTING_STYLE=literal
export EDITOR=vim
