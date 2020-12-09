# Disable greeting.
set fish_greeting ""

# Set miscellaneous aliases.
alias ls     "ls -AFC --color=auto"
alias diff   "diff --color=auto"
alias emacs  "emacs -nw"
alias fgrep  "fgrep --color=auto"
alias mkdir  "mkdir -p"
alias rm     "rm -i"
alias Tami   "echo -e \"\\nTomi <3 Tami\\n\""
alias tmux   "tmux -f /archlinux/config/tmux/tmux.conf"
alias ping   "ping -c 2"
alias startx "startx /archlinux/config/X/xinitrc"

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

# Change some global config paths.
set -x GIT_CONFIG "/archlinux/config/git/gitconfig"

# Change some user-specific config paths.
set -x EMACS_USER_DIRECTORY "$HOME/.config/emacs.d/"
set -x GNUPGHOME "$HOME/.local/gnupg/.gnupg"
set -x XAUTHORITY "$HOME/.config/X/xauthority"

# Do NOT cache Python bytecode.
set -x PYTHONDONTWRITEBYTECODE 1

# Script aliases.
alias battery "/archlinux/scripts/battery.fish"
alias monitor "/archlinux/scripts/monitor.fish"

# Load my secrets. Keep this file safe!
source /archlinux/scripts/secrets.fish

# Avoid advanced config for stupid shells.
if [ "$TERM" = "linux" ]
   exit
end

# Use Powerline! This is a bit bloaty, but I think it's worth it.
source /usr/share/powerline/bindings/fish/powerline-setup.fish
powerline-setup

# Auto-start tmux (if not already running).
if not set -q TMUX
    tmux
end
