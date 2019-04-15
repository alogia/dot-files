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


antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-history-substring-search
antigen bundle joel-porquet/zsh-dircolors-solarized.git


antigen theme af-magic
antigen apply

############################################
#				EXPORTS
############################################
source $HOME/.zsh_exports 

############################################
#			  KEY BINDINGS   
############################################
source $HOME/.keybindings

############################################
#			 	TWEEKS 
############################################

#Turn off flow control.
stty -ixon 
unsetopt autopushd
