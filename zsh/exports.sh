#####################################
## ZSH default exports 
#####################################

## Paths
export GEM_HOME="$HOME/.gem"
export PATH="$PATH:$HOME/bin:$HOME/devel/bin:$(ruby -e 'print Gem.user_dir')/bin:$HOME/.local/bin/:$HOME/.cabal/bin"
##export BUNDLE_PATH="$HOME/gem/bin"

## Python
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"


# #Editors
export EDITOR=vim
export VISUAL=vim

#Tex variables
export PDFVIEWER=evince
export TEXDOCVIEW_pdf=evince
export TEXDOC_VIEWER_PDF=evince

## Wayland support flags
#export GDK_BACKEND=wayland
export GTK_THEME=Adwaita:dark 
export XCURSOR_SIZE=16
export QT_QPA_PLATFORM=wayland-egl
export CLUTTER_BACKEND=wayland
export SDL_VIDEODRIVER=wayland
export MOZ_ENABLE_WAYLAND=1
#export QT_SCALE_FACTOR=2
export QT_QPA_PLATFORMTHEME=qt5ct
export USE_SYMENGINE=1

## Misc
export QUOTING_STYLE=literal
export _JAVA_AWT_WM_NONREPARENTING=1

## SSH
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
