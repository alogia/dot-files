#General exports for all shells

export GEM_HOME="$HOME/gems"
export PATH="$PATH:$HOME/bin:$HOME/devel/bin:$(ruby -e 'print Gem.user_dir')/bin:$HOME/.local/bin/:$HOME/.cabal/bin"
export QUOTING_STYLE=literal
export EDITOR=vim
export VISUAL=vim

export XCURSOR_SIZE=16
export QT_QPA_PLATFORM=wayland-egl
export CLUTTER_BACKEND=wayland
export SDL_VIDEODRIVER=wayland

#FZF setup options
export FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"
export FZF_DEFAULT_OPTS='--bind=alt-n:up,alt-t:down'
export FZFZ_EXCLUDE_PATTERN='\.(git|cache|stack|mozilla)|node_modules'

export MOZ_ENABLE_WAYLAND=1
