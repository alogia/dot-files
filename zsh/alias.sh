alias wifi='sudo netctl switch-to "$(netctl list | fzf | sed -r '\''s/^[\* ]? //'\'')"'
alias als='alias | grep'
alias dh='sudo dhcpcd wlp58s0'
alias c=clear
alias esp='source /opt/esp-idf/export.sh'
alias off='sudo shutdown now'
alias mplab mplab_ide
