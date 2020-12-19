# My archlinux setup

> Dina Tamás Pál | 2020.12.08.

------------------------------

These are my notes and scripts for setting up my Arch Linux workstation.

## Usage

__Preparatory steps__
1. Please read through the official Arch Linux Installation Guide!
1. Download the Arch Linux image and prepare the installation medium.
1. Disable Secure Boot from the BIOS.
1. Boot into the live install image's virtual console.
1. Connect to the internet, using Wi-fi, Ethernet, USB tethering, etc.

__Run the install scripts__
1. `pacman -Sy git`.
1. `git clone https://github.com/dinatamas/archlinux.git`.
1. Read through and customize the script.
1. `cd archlinux`
1. `./install1.sh`
1. After executing the script, please reboot.

Note: If you exit the script during execution, it will clean up after iself.
Thus it can be safely run and restarted multiple times.

__First run__
1. `sudo pacman -Syu`.
1. Emacs packages may need to be installed manually.
1. Tmux packages may need to be installed manually.

__Using the system__
* Start GUI: `startx`.
* Start internet: `wifi`.
* Configure networks: `nmtui`.
  * Configured networks can be copied from `/etc/NetworkManager/system-connections/`.
* Check battery status from CLI: `battery`.
* Live plug monitors: `monitor`.
* Live plug tv (HiDPI): `tv`.
* Take screenshot: `flameshot`.
* Change wallpaper: `wallpaper`.

Further help can be found in the `cheatsheets` directory.
