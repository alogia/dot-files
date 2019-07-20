alias wifi='sudo netctl switch-to "$(netctl list | fzf | sed -r '\''s/^[\* ]? //'\'')"'
alias als='alias | grep'
alias dh='sudo dhcpcd wlp58s0'
