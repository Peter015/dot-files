# Set the manpage
set -x MANPAGER "sh -c 'col -bx | bat -l man -p'"

#set aliases
alias ls='exa --color=always --group-directories-first'
alias la='exa -a --color=always --group-directories-first'  # all files and dirs
alias ll='exa -l --color=always --group-directories-first'  # long format
alias lt='exa -aT --color=always --group-directories-first' # tree listing
alias grep='rg'
alias cat='bat'
alias config='/usr/bin/git --git-dir=/home/peterz/.cfg/ --work-tree=/home/peterz'

# opam configuration
source /home/peterz/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
