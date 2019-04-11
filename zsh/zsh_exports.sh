#General exports for all shells

export GEM_HOME="$HOME/gems"
export PATH="$PATH:$HOME/bin:$HOME/devel/bin:$(ruby -e 'print Gem.user_dir')/bin:$HOME/.local/bin/"
export QUOTING_STYLE=literal
export EDITOR=vim
