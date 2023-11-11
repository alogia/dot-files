source ~/.bash_aliases

export GEM_HOME="$HOME/gems"
export PATH="$PATH:$HOME/bin:$(ruby -e 'print Gem.user_dir')/bin"
export HISTCONTROL=ignoreboth:erasedups
export PS1='\[\033k\033\\\]'$PS1
export LC_CTYPE=en_US.UTF-8
export LS_COLORS='di=1;36:'


#Tex variables
export GDK_BACKEND=wayland
export PDFVIEWER=evince
export TEXDOCVIEW_pdf=evince
export TEXDOC_VIEWER_PDF=evince
