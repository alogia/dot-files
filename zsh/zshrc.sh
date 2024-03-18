	###########################################
# 			ANTIGEN SETUP
###########################################
ANTIGEN=0
if [ -f /usr/share/zsh/share/antigen.zsh ]; then 
	source /usr/share/zsh/share/antigen.zsh 
	ANTIGEN=1
elif [ -f /usr/local/share/zsh-antigen/antigen.zsh ]; then
	source /usr/local/share/zsh-antigen/antigen.zsh
	ANTIGEN=1
else
	echo "ERROR: Cannot find antigen file."
fi

if (( $ANTIGEN == 1 )); then
	antigen use oh-my-zsh

	# Bundles from the default repo (robbyrussell's oh-my-zsh).
	antigen bundle git
	antigen bundle screen
	antigen bundle bundler
	antigen bundle tmux
	antigen bundle z

	antigen bundle zsh-users/zsh-syntax-highlighting
	antigen bundle zsh-users/zsh-history-substring-search
	if command -v dircolors &> /dev/null; then
		antigen bundle joel-porquet/zsh-dircolors-solarized.git
	fi
	antigen bundle andrewferrier/fzf-z


	antigen theme af-magic
	antigen apply
fi
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
source $HOME/.config/zsh/fzf

############################################
#Set up python
############################################
eval "$(pyenv init -)" 

