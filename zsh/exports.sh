#####################################
## ZSH default exports 
#####################################

## Pathes
export GEM_HOME="$HOME/gems"
export PATH="$PATH:$HOME/bin:$HOME/devel/bin:$(ruby -e 'print Gem.user_dir')/bin:$HOME/.local/bin/:$HOME/.cabal/bin"


## Editors
export EDITOR=vim
export VISUAL=vim

## Wayland support flags
export XCURSOR_SIZE=16
export QT_QPA_PLATFORM=wayland-egl
export CLUTTER_BACKEND=wayland
export SDL_VIDEODRIVER=wayland
export MOZ_ENABLE_WAYLAND=1
export QT_SCALE_FACTOR=2

## Misc
export QUOTING_STYLE=literal
