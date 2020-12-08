# Use Powerline! This is a bit bloaty, but I think it's worth it.
source /usr/share/powerline/bindings/fish/powerline-setup.fish
powerline-setup

# Disable greeting.
set fish_greeting ""

# Set miscellaneous aliases.
alias ls    "ls -AFC --color=auto"
alias diff  "diff --color=auto"
alias emacs "emacs -nw"
alias fgrep "fgrep --color=auto"
alias mkdir "mkdir -p"
alias rm    "rm -i"
alias Tami  "echo -e \"\\nTomi <3 Tami\\n\""
alias ping  "ping -c 2"

# Add color to less, plus mute it.
set -x LESSOPEN "| /usr/bh n/source-highlight-esc.sh %s"
set -x LESS     "$LESS -R -Q"

# Add color to man.
set -x LESS_TERMCAP_mb (printf "\033[01;31m")  
set -x LESS_TERMCAP_md (printf "\033[01;31m")  
set -x LESS_TERMCAP_me (printf "\033[0m")  
set -x LESS_TERMCAP_se (printf "\033[0m")  
set -x LESS_TERMCAP_so (printf "\033[01;44;33m")  
set -x LESS_TERMCAP_ue (printf "\033[0m")  
set -x LESS_TERMCAP_us (printf "\033[01;32m")

# Use cd and ls together.
function cd
    builtin cd $argv; and ls
end

# Auto-start tmux (if not already running).
if not set -q TMUX
    tmux
end

# Do NOT cache Python bytecode.
set -x PYTHONDONTWRITEBYTECODE 1

# Print battery info.
function battery
    cat /sys/class/power_supply/BAT0/capacity
end

# Wifi configuration.
alias startwifi "sudo /archlinux/scripts/startwifi.fish"
alias stopwifi  "sudo /archlinux/scripts/stopwifi.fish"

# Load my secrets. Keep this file safe!
source /archlinux/scripts/secrets.fish
