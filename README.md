# My archlinux setup

> Dina Tamás Pál | 2020.10.15.

------------------------------

These are my notes and setup scripts while installing and preparing my Arch
Linux environment. Thanks to John Hammond for inspiring this repo!

## Usage

__Preparatory steps__
1. Please read through the official Arch Linux Installation Guide!
1. Download the Arch Linux image and prepare the installation medium.
1. Disable Secure Boot from the BIOS.
1. Boot into the live install image's virtual console.
1. Connect to the internet, using Wi-fi, Ethernet, USB tethering, etc.

__Run the install scripts__
1. `loadkeys hu`
1. `pacman -Sy git`
1. `git clone https://github.com/dinatamas/archlinux.git`
1. `cd archlinux`
1. `chmod +x ./install*.sh`
1. `./install1.sh`
1. After executing the script, please reboot.

__First run__
1. `sudo pacman -Syu`

## Explanation of dotfiles

**Cross-user:**
* `/etc/profile`: Cross-shell configuration. Not modified.
* `/etc/profile.d`: Cross-shell configurations for specific programs. Not modified.
* `/etc/inputrc`: Modified to ensure that the command-line bell is muted.
* `/etc/bashrc`: Sourced by `/etc/profile` when bash shell is recognized. Not used.
* `/etc/bash.bashrc`: This is used instead of `/etc/bashrc`. Not modified.

**User-specific:**
* `~/.profile`: Cross-shell configuration. Bash uses it in absence of `~/.bash_profile` and `~/.bash_login`. Not used.
* `~/.bash_profile`: Preferred configuration file for bash login shells. Not modified.
* `~/.bash_login`: Configuration file in absence of `~/.bash_profile`. Not used.
* `~/.bash_logout`: Used to perform actions upon logging out. Not used.
* `~/.bash_aliases`: Separate file to store bash aliases. Not used.
* `~/.bashrc`: Used for non-login shells, but is also sourced by login-shell configs. This file is modified.

**Program-specific:**
* `/etc/vim/vimrc`: Cross-user vim configuration. Not modified.
* `~/.vimrc`: User-specific vim configuration. This file is modified.
* `~/.tmux.conf`: User-specific tmux configuration. This file is modified.

## TODO

### Miscellaneous
* Add a LICENSE to this repository.
* Consider alternative networking approaches.
* Consider having a fallback kernel. (GRUB should handle this nicely...)
* Consider using another shell (such as zsh or fish...)
* Desktop: basic i3 <-> gnome <-> pantheon
* Desktop: xorg <-> wayland
* Desktop: xinit <-> startx
* Users are responsible for updating non-official driver packages in major kernel updates.
* Examine the archiso image for packages, configs, etc.
* Look for config file examples.
* `echo "needs_root_rights=yes" >> /etc/X11/Xwrapper.config`?
* home-dir or etc `bash_profile` sourcing `.bashrc`?

### Installing more software
* Install recommended packages of installed packages with pacman?
* Review the list of packages available in the installation medium.
* Review the list of packages in base, base-devel, linux, linux-firmware.
* Review the Arch Linux list of applications.
* Configure AUR (and Arch build system) and a simple AUR-manager (like yay).
* Install pkgstats?
* Security, GNU-binutils, pwn tools.
* Multimedia, codecs, professional audio, sound servers.
* Nvidia, display drivers, proprietary add-ons, touchpad drivers (synaptics/libinput).
* Tex, pandoc, nvim/neovim.

### Script usability improvements
* Reversibility: capture KILL and EXIT and Ctrl+C and SIGHUP... and before exiting, try cleaning up.
* Make proceed prompt a little bit better, e.g. allow skipping steps?
* More safety switches: etc. check wifi issue, check intel or amd microcode, etc.
* `wpa_passphrase` wrapper: no plain pwd, priorities, hidden networks.
* Write more scripts: turn on/off wifi, bluetooth, gui, etc.
* Experiment with executing all commands using indent.
* Use a logfile for the script!
* Experiment with more visible delimiters, e.g. multiple lines of characters?
* Remember where last execution left off and retry from there.

### More system configuration
* RSA, PGP key generation + SSH setup.
* Setup repeated maintenance, such as security/risk checks, mirror file updates, backups.
* Read Arch Linux news RSS.
* Security hardening: firewall, systemctl --failed, dmesg grep for errors, use secure DNS services.
* Performance improvements: laptops, CPU scaling, ThrottleStop.
* Performance benchmarking: install tools and setup regular checks.
* xdg-user-dirs and xdg-users-dirs-update.service.
* df -Tha --total, du -ach | sort -h, free -mt, lsblk -o "NAME,PATH,SIZE,TYPE,FSTYPE,MOUNTPOINT"

### Personal configuration
* Bash resources, tips&tricks, aliases.
* Improve copy-paste, tab completion.
* Disable beep system-wide.
* Uniform look for GTK and QT (themes?)
* Eye candy: wallpapers, etc. See more i3 config,
* Start with fn+esc by default.
* Important: powerline for tmux, bash, i3, vim.
* Vim configuration: vim-plug, vim-sensible, vim-subline-monokai.
* Vim configuration: JohnHammond's vimrc file?
* Powerilne!!??

### Do some reading
* See more guides, with packages like archdi (arch desktop install).
* The General Recommendations section mentions tmux and X together... how?
* Arch Linux wiki reading:
    * Benchmarking, Command-line shells, General troubleshooting, Improve performance, Pacman Tips&Tricks, Power management, System maintenance, System services.
* Traffic shaping.
* How to get help? Alternatives to apt-get and dman...
    * `pacman -Qlg <package_name> | grep /usr/bin`
* xterm and terminfo/tset capabilities.
