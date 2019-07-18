##########################
#    FZF setup options
##########################

## Source all system setup files
source /usr/share/fzf/completion.zsh
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/fzf-extras.zsh

## Exclude pattern for ag. Fd commands use the ~/.fdignore file. 
export FZFZ_EXCLUDE_PATTERN='\.(git|cache|stack|mozilla)|node_modules'

## Default 
export FZF_DEFAULT_COMMAND="fd --follow --hidden --type f . $HOME "
export FZF_DEFAULT_OPTS='--bind=alt-n:up,alt-t:down'

## CTRL-T  --- Select Files
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"

## ALT-C --- Change Directory
export FZF_ALT_C_COMMAND="fd --follow --hidden --type d . $HOME "
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"

## CTRL-R  --- History
export FZF_CTRL_R=''
