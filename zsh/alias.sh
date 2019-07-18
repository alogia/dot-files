alias wifi='sudo netctl start "$(netctl list | fzf | sed -r '\''s/^[\* ]? //'\'')"'
alias als='alias | grep'
