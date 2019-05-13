###########################################
# 			ANTIGEN SETUP
###########################################
source /usr/share/zsh/share/antigen.zsh
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle screen
antigen bundle bundler
antigen bundle tmux
antigen bundle z

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-history-substring-search
antigen bundle joel-porquet/zsh-dircolors-solarized.git
antigen bundle andrewferrier/fzf-z


antigen theme af-magic
antigen apply

############################################
#				EXPORTS
############################################
source $HOME/.config/zsh/exports 

############################################
#				ALIASES	
############################################
source $HOME/.config/zsh/alias
############################################
#			  KEY BINDINGS   
############################################
source $HOME/.config/zsh/keybindings

############################################
#			 	TWEEKS 
############################################
#Turn off flow control.
stty -ixon 
unsetopt autopushd

############################################
#Set up fzf
############################################
source /usr/share/fzf/completion.zsh
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/fzf-extras.zsh
