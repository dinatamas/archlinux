# My archlinux setup

> Dina Tamás Pál | 2021.02.16.

------------------------------

These are my notes and scripts for setting up my Arch Linux workstation.

![Console environment](/img/console_screenshot.png)
![Development environment](/img/work_screenshot.png)

## Usage

__Preparatory steps__
1. Please read through the official Arch Linux Installation Guide!
1. Download the Arch Linux image and prepare the installation medium.
1. Disable Secure Boot from the BIOS.
1. Boot into the live install image's virtual console.
1. Connect to the internet, using Wi-fi, Ethernet, USB tethering, etc.

__Run the install scripts__
1. `pacman -Sy git`
1. `git clone https://github.com/dinatamas/archlinux.git`
1. Read through and customize the scripts.
1. Read through and customize the configuration files.
1. `cd archlinux`
1. `./install1.sh`
1. Handle (copy) the "secret" files (ssh config, network config, etc.)
1. `./install3.sh`
1. After executing the scripts, please reboot.
1. At first run: `sudo pacman -Syu`

Note: If you exit the script during execution, just re-execute `install1.sh`.

TODO: Highlight where things may need to be changed in the scripts / configs.

__Using the system__
* Start GUI: `startgui`
* Start internet: `wifi`
* Configure networks: `nmtui`
* Check battery status from CLI: `battery`
* Live plug monitors: `monitor`
* Live plug tv (HiDPI): `tv`
* Take screenshot: `flameshot`
* Change wallpaper: `wallpaper`
* Hold `shift` to open GRUB menu during bootup.

__Copy-Paste instructions__
* URxvt:
    * Select using the mouse.
    * Copy by clicking the right mouse button.
    * Paste by clicking the middle mouse button.
* tmux:
    * Select using the mouse.
    * Copy happens automatically.
    * Paste by pressing Ctrl+V.
* Emacs:
    * Ctrl+Space can be used to select text.
    * Alt+w copies, Ctrl+w cuts, Ctrl+y pastes.

Further help can be found in the `cheatsheets` directory.
TODO: Add a cheatsheets directory.

## Further inspiration

* r/unixporn
* r/emacs
* doom emacs configuration
* LukeSmith's wallpapers (uploaded by DiscoBiscuit99)
* BSPWM - Camus edition (by u/NoBrightFutureForMe)
* GitHub: Unixado/dotfiles
* wallpapers: PositronDream.com
* GitHub: André Almeida's Polybar config
* GitHub: kingjuliando/config Polybar
* GitHub: koraxnyx/bunsenlabs (rounded window corners?)
* u/Altinus - zathura opacity?
* gist.github.com/dbb/785347
* daviwil emacs dotfiles
* rougier: NANO emacs
* rougier: elegant emacs
* anzu package for highlighting
* ioah86/repetition_error_finder
* Bret Victor: Inventing on Principle
* github snakedye wlr_config wallpapers
* hashtagsaurav dotfiles

------------------------------

TODO: Make this part of the installation procedure!
Windows-copyrighted fonts:
sudo mkdir /opt/aur_builds
sudo cd /opt/aur_builds
sudo git clone -n https://aur.archlinux.org/ttf-ms-win10.git
cd ttf-ms-win10
sudo git checkout 8e4be920594505a20dca5532cf6a2eb2cac1a17d
The files were extracted legally. (See ArchLinux wiki for instructions.)
They are placed under password protected encryption.
The password is sourced from the secret file.

Place the extracted archive to the /opt/aur_builds/ttf-ms-win10/ directory.
