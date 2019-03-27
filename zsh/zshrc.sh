###########################################
# 			ANTIGEN SETUP
###########################################
source /usr/share/zsh/share/antigen.zsh
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle screen
antigen bundle sudo
antigen bundle bundler


antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-history-substring-search
antigen bundle joel-porquet/zsh-dircolors-solarized.git


antigen theme af-magic
antigen apply

############################################
#				EXPORTS
############################################
export GEM_HOME="$HOME/gems"
export PATH="$PATH:$HOME/bin:$(ruby -e 'print Gem.user_dir')/bin"

############################################
#			  KEY BINDINGS   
############################################
source $HOME/.keybindings
