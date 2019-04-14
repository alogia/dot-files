#General exports for all shells

export GEM_HOME="$HOME/gems"
export PATH="$PATH:$HOME/bin:$HOME/devel/bin:$(ruby -e 'print Gem.user_dir')/bin:$HOME/.local/bin/:$HOME/.cabal/bin"
export QUOTING_STYLE=literal
export EDITOR=vim

export QT_QPA_PLATFORM=wayland-egl
export CLUTTER_BACKEND=wayland
export SDL_VIDEODRIVER=wayland
