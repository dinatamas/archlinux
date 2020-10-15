# My archlinux setup

> Dina Tamás Pál | 2020.10.11.

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
1. `chmod +x ./install1.sh`
1. `./install1.sh`
1. After executing the script, please reboot.

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
* Some configurations don't work automatically (for informative console output):
    * wifi ping from the startwifi script
    * timedatectl not syncing automatically
* Full system upgrade upon first start?
* Copy network scripts and dotfiles to easily available places. Both root and new users?

### Installing more software
* Install recommended packages of installed packages with pacman?
* Review the list of packages available in the installation medium.
* Review the list of packages in base, base-devel, linux, linux-firmware.
* Review the Arch Linux list of applications.
* Configure AUR (and Arch build system) and install some packages from there.
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
* Read Arch Linuy new RSS.
* Security hardening: firewall, systemctl --failed, dmesg grep for errors, use secure DNS services.
* Performance improvements: laptops, CPU scaling, ThrottleStop.
* Performance benchmarking: install tools and setup regular checks
* xdg-user-dirs and xdg-users-dirs-update.service.

### Personal configuration
* Better fonts (TrueType/GNU free).
* Bash resources, tips&tricks, aliases.
* Prompt: user@topmost-directory.
* Improve copy-paste, tab completion.
* Disable beep system-wide.
* Uniform look for GTK and QT (themes?)
* Eye candy: wallpapers, etc.
* Desktop settings: keyboard layout.

### Do some reading
* See more guides, with packages like archdi (arch desktop install).
* The General Recommendations section mentions tmux and X together... how?
* Arch Linux wiki reading:
    * Benchmarking, Command-line shells, General troubleshooting, Improve performance, Pacman Tips&Tricks, Power management, System maintenance, System services.
* Traffic shaping.
* How to get help? Alternatives to apt-get and dman...
    * `pacman -Qlg <package_name> | grep /usr/bin`
