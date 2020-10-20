# Use Powerline! This is a bit bloaty, but I think it's worth it.
powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. /usr/share/powerline/bindings/bash/powerline.sh

# Use the solarized dark TTY theme.
source /archlinux/themes/tty-solarized-dark-doubled.sh

# History settings.
HISTCONTROL=ignoreboth
shopt -s histappend
HISTSIZE=1000
HISTFILESIZE=2000

# Check window size after each command.
shopt -s checkwinsize

# '**' will match a lot of things.
# shopt -s globstar

# Set custom prompt.
# Powerline will overwrite this.
PS1='\[\033[01;32m\]\u\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\$ '

# Set aliases.
alias ls="ls -AlCF --color=auto"
alias diff="diff --color=auto"
alias grep="grep --color=auto"
alias fgrep="fgrep --color=auto"
alias egrep="egrep --color=auto"
alias mkdir="mkdir -vp"
alias history="history | grep -i"
alias Tami="echo -e \"\\nTomi <3 Tami\\n\""
alias vi="vim"
alias ping="ping -c 2"
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export LESS="$LESS -R -Q"

# Add color to man.
man() {
    LESS_TERMCAP_md=$'\e[01;31m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[01;44;33m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[01;32m' \
    command man "$@"
}

# Use cd and ls together.
function cd {
    builtin cd "$@"
    ls
}

# Print battery info.
function battery {
    cat /sys/class/power_supply/BAT0/capacity
}

# Decompress compressed files.
function extract {
    for n in $@; do
        case "${n%,}" in
            *.tar.gz | *.tar.xz | *.tgz | *.txz | *.tar)
                tar xvf "$n" ;;
            *.rar)
                unrar x -ad "$n" ;;
            *.gz)
                gunzip "$n" ;;
            *.zip)
                unzip "$n" ;;
            *.z)
                uncompress "$n" ;;
            *.7z)
                7z x "$n" ;;
            *.xz)
                unxz "$n" ;;
            *)
                echo "extract: '$n' - unknown archive method" ;;
        esac
    done
}

# Set aliases for wifi-related scripts.
alias startwifi="sudo /archlinux/network/systemd/startwifi.sh"
alias stopwifi="sudo /archlinux/network/systemd/stopwifi.sh"

# Auto-start tmux (if not already running).
if [ -z "$TMUX" ]; then
    tmux
fi
