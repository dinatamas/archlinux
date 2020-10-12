# My archlinux setup

> Dina Tamás Pál | 2020.10.11.

------------------------------

These are my notes and setup scripts while installing and preparing my Arch
Linux environment. Thanks to John Hammond for inspiring this repo!

## Usage

__Preparatory steps:__
1. Please read through the official Arch Linux Installation Guide!
1. Download the Arch Linux image and prepare the installation medium.
1. Disable Secure Boot from the BIOS.
1. Boot into the live install image's virtual console.
1. Connect to the internet, using Wi-fi, Ethernet, USB tethering, etc.

__Run the install scripts.__
1. Download the contents of this repository using wget.
1. Execute the `install1.sh` script.
1. After executing the script, please reboot.

## TODO

* Walk through John Hammond's Arch Linux setup > bootstrap + his config stuff.
* Install recommended packages of installed packages with pacman?
* Review the list of packages available in the installation medium.
* Review the Arch Linux list of applications.
* Reversibility: capture KILL and EXIT and Ctrl+C and SIGHUP... and before exiting, try cleaning up.
* Make proceed prompt a little bit better, e.g. allow skipping steps?
* More safety switches: etc. check wifi issue, check intel or amd microcode, etc.
* See more guides, with packages like archdi (arch desktop install).
* Set root password via password prompt.
* Create new user.
* Preconfigure netword, resolved and iwd similarly to the isntall image... (wpa supplicant off?)
* Add a LICENSE to this repository.
* Add a .gitignore to this repository.
* Provide the following: .bashrc (or similar), .vimrc, tmux.conf
    * The General Recommendations section mentions tmux and X together... how?
* I've discarded the idea once, but double-check: do I not need os-prober?
* Create a sudo group, edit the sudoers file? No-pwd sudo?
* Setup X window system, xinitrc, i3, terminator config?
* Setup networking: either systemd-networkd or network-manager, wpa supplicant, etc.
* Configure git.
* Add even more to-install packages.
* Go deeper into the Arch setup wiki links:
    * System maintenance: backing up, checking errors, upgrades, clean filesystem, tips & tricks.
    * System services: file index and search. Disable unneeded: printing, etc.
    * Consider having a fallback kernel if a new kernel update may cause problems?
      This includes configuring GRUB to handle these boot options.
    * Also: create fallbacks before upgrading, so that it can be reverted to a previous version.
      And compat packages to support packages that rely on older shared libraries.
    * Automatic Arch system maintenance: scripts and services (and arch news RSS).
    * Security hardening of the system, firewall.
        * systemctl --failed
        * dmesg grep for errors?
        * Visit General troubleshooting > f.e. retaining boot messages.
        * Use a secure and verified DNS service?
    * updatedb for mlocate service regularly needed?
    * Routinely update the mirrors file.
    * Configure users and groups (better).
    * Add some more unofficial repositories, and install pkgstats service?
        * How about AUR and build scripts? Should those be used?
    * Consider installing the Arch Build System, Arch User Repository.
    * Improve power management (laptops, CPU scaling, throttling, etc.).
    * Optimize the performance (and power management): CPU, SSD and benchmarking tools.
    * Multimedia, codecs, sound servers, IRC.
        * ALSA sound driver (unmute!)
        * PulseAudio + some more nice audio.
    * Custom and proprietary drivers, and device setup (e.g. touchpad).
        * Synaptics or libinput.
        * Also: check that my mouse works well.
      Users are responsible for updating non-official driver packages in major kernel updates.
    * Consider implementing traffic shaping (Arch does not provide it out of the box!)
* Install GUI (desktop environment & window manager)!
    * Xorg or Wayland?
    * Install NVIDIA display drivers.
    * Desktop environment <-> window manager + display manager (VS starting X manually...)
        * startx, xinit (xinitrc?)
    * Install xdg-user-dirs and run xdg-user-dirs-update.service or manually.
* GUI and CLI improvements.
    * Choose a nicer font that can display more characters required by me.
        * Install an actual TrueType font (family) that looks/scales nicer.
    * Terminal emulator?
    * Read through the Console improvements section (Command-line shells)
    * Aliases.
    * grml-zsh-config add-on for zsh (like install media)
    * bash tips & tricks, colored output
    * Significantly improve copy-paste (e.g. general mouse setup?)
    * Tab-completion.
    * Display only the topmost directory in prompt.
    * Make sure HiDPI/terminus?
    * Make sure to DISABLE beep (system/console-wide, or in bashrc, less, vim...)
    * See the setterm command
    * Setting keyboard layout in Xorg is different from that of the console.
    * Uniform look for Qt and GTK applications > themes!
* Write scripts for CLI-only usage: such as ones for turning on/off wifi, bluetooth, etc.
* Utilities for getting help and searching packages via Arch (from Ubuntu):
    * `pacman -Qlg <package_name> | grep /usr/bin`
    * `dman` alternative...
* Examine what is supplied in base, base-devel, linux and linux-firmware.
* Read through the other main sections in the Arch Linux wiki.
  This means categories referenced from the Installation Guide and General Recommendations.
    * Power management.
* Read the pacman/Tips and tricks section as well.
* Power management with systemd: ACPI events (closing lid, etc.)
* CPU frequency scaling (compare: throttlestop)
* See: Laptop guides (with power management specifics).
* Read the Benchmarking (and then Improving Performance + SSD) sections.
* "Eye candy" + personal preference setup, also with wallpapers, etc.
* More git config: line endings, anything else?
* SSH keys and such: generate and help with using them during setup...
* Write a `wpa_passphrase` wrapper that doesn't include plain text passwd, can set priority,
  hidden networks, and can add a line below and above (thus can be >>'d to the conf file)
