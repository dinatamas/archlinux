# History settings.
HISTCONTROL=ignoreboth
shopt -s histappend
HISTSIZE=1000
HISTFILESIZE=2000

# Check window size after each command.
shopt -s checkwinsize

# '**' will match strongly.
shopt -s globstar

# Set custom prompt.
PS1='\[\033[01;32m\]\u\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\$ '

# Set aliases.
alias ls="ls -AlCF --color=auto"
alias grep="grep --color=auto"
alias fgrep="fgrep --color=auto"
alias egrep="egrep --color=auto"
export LESS="$LESS -R -Q"
