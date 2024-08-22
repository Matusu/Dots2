
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

eval "$(starship init bash)"

alias grep='grep --color=auto'
alias vim='nvim'
alias ls='exa --long --color=auto'
alias battery='bat /sys/class/power_supply/BAT0/capacity'
alias paru-r="paru -Rcns"
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
PS1='[\u@\h \W]\$ '

neofetch
