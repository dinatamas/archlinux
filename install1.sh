#!/bin/bash

COLOR_RED=$(tput setaf 1)
COLOR_YELLOW=$(tput setaf 3)
BOLD=$(tput bold)
COLOR_RESET=$(tput sgr0)

function print_help() {
    cat << EOF
Start this script in the initial archlinux virtual console.
Please make sure to read through the official Installation Guide.
EOF
    exit 0
}

function parse_args() {
    local arg
    while [[ $# -gt 0 ]]; do
        arg="$1"
        shift
        case $arg in
            -h | --help)
                print_help
                ;;
            *)
                echo "${COLOR_RED}Invalid argument: $arg!${COLOR_RESET}"
                exit 1
                ;;
        esac
    done
}

function ask_proceed() {
    while true; do
        read -p "Proceed? [y/n] " reply
        case $reply in
            [Yy]*)
                break
                ;;
            [Nn]*)
                echo "Exiting..."
                exit 0
                ;;
            *)
                echo "Please answer yes (y) or no (n)"
                ;;
        esac
    done
}

function ask_proceed_quiet() {
    read -s -p "Press Enter to proceed..." _
    echo -en "\033[2K"; printf "\r"
}

function indent() {
    eval "$@" |& sed "s/^/\t/" ; return "$PIPESTATUS"
}

parse_args "$@"

echo "Verifying that the computer has network connectivity..."
ask_proceed_quiet
ping archlinux.org -c 3 &>/dev/null
if [ $? -ne 0 ]; then
    echo "${COLOR_RED}Error: No network connectivity!${COLOR_RESET}"
    exit 1
fi

echo "-----"

echo "Loading Hungarian keyboard layout..."
ask_proceed_quiet
loadkeys hu

echo "-----"

echo "Verifying that the system was booted in UEFI mode..."
ask_proceed_quiet
ls /sys/firmware/efi/efivars &>/dev/null
if [ $? -ne 0 ]; then
    echo "${COLOR_YELLOW}Warning: The system wasn't booted in UEFI mode!${COLOR_RESET}";
    ask_proceed
fi

echo "-----"

echo "Old disk layout on /dev/sda:"
ask_proceed_quiet
indent 'fdisk -l /dev/sda'
echo
indent 'lsblk -o "NAME,PATH,SIZE,TYPE,FSTYPE,MOUNTPOINT"'
ask_proceed_quiet

echo "-----"

echo "Backing up current partition layout..."
ask_proceed_quiet
sfdisk --dump /dev/sda > sda.dump
echo "The contents of ./sda.dump:"
indent 'cat sda.dump'
ask_proceed_quiet

echo "-----"

echo "Creating new partition layout..."
echo "The current partition layout will be wiped"
ask_proceed_quiet
# TODO: Force the wiping of signatures?
read -r -d '' sfdisk_script << EOM
label: gpt
/dev/sda1 : size=500MiB type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B
/dev/sda2 : size=4GiB type=0657FD6D-A4AB-43C4-84E5-0933C84B4F4F
/dev/sda3 : type=4F68BCE3-E8CD-4DB1-96E7-FBCAF984B709
EOM
echo "sfdisk script:"
indent "echo \"$sfdisk_script\""
echo "The above sfdisk script will be applied"
ask_proceed
echo "$sfdisk_script" | sfdisk /dev/sda

echo "-----"

echo "Creating new filesystems..."
ask_proceed_quiet
mkfs.fat -F32 /dev/sda1
mkswap /dev/sda2
mkfs.ext4 /dev/sda3

echo "Mounting new filesystems..."
ask_proceed_quiet
mkdir /mnt/efi
mount /dev/sda1 /mnt/efi
swapon /dev/sda2
mount /dev/sda3 /mnt

echo "-----"

echo "New disk layout on /dev/sda:"
ask_proceed_quiet
indent 'fdisk -l /dev/sda'
echo
indent 'lsblk -o "NAME,PATH,SIZE,TYPE,FSTYPE,MOUNTPOINT"'
ask_proceed_quiet

echo "-----"

echo "Generating new mirror file..."
ask_proceed_quiet
cp /etc/pacman.d/mirrorlist /etc/pacman.d/mirrorlist.bak
echo "Original mirrorfile backed up to /etc/pacman.d/mirrorlist.bak"
curl "https://www.archlinux.org/mirrorlist/?country=HU&protocol=https&ip_version=4&use_mirror_status=on" > ./mirrorlist &>/dev/null
sed -i 's/#Server/Server/g' ./mirrorlist
echo "This mirror file will be applied:"
indent "cat ./mirrorlist"
ask_proceed
cp ./mirrorlist /etc/pacman.d/mirrorlist

echo "-----"

echo "Installing essential packages..."
echo "The following packages will be installed:"
# TODO: Allow the selection of packages?
base="base base-devel efibootmgr git grub intel-ucode linux linux-firmware"
indent "echo \"Base packages that are required for a minimal installation: ${base}\""
helpgetting="man-db man-pages texinfo"
indent "echo \"Packages to help getting help:                              ${helpgetting}\""
networking="dhcpcd wpa_supplicant"
indent "echo \"Packages required for networking:                           ${networking}\""
coreutils="cronie curl diffutils less nano openssh openssl rsync sudo util-linux vi vim wget"
indent "echo \"The most basic utilities:                                   ${coreutils}\""
archiving="bzip2 gzip p7zip unzip zip"
indent "echo \"Archiving, compressing, extracting:                         ${archiving}\""
system="htop hwinfo lshw mc neofetch"
indent "echo \"Tools for system management:                                ${system}\""
extended="tmux transmission-cli"
indent "echo \"Extended utilities:                                         ${extended}\""
ask_proceed
pacstrap /mnt $packagelist # &>/dev/null

# Things to be added to the above list:
# Developer stuff: docker, jo, jq, python3.8
# Devices and multimedia: alsa-utils pavucontrol pciutils pulseaudio usbutils
# GUI basics: gnu-free-fonts i3 xorg-server xorg-xinit xorg-xrandr
# GUI applications: bitwarden, calendar, firefox, spotify, terminator, todoist, vlc, vscode

# Things that are here just to remember them:
# Alternative networking packages: dhclient dhcping iw iwd network-manager
# GNU stuff: gdm gnome-control-center gnome-session
# Misc/unknown: dmenu mlocate polkit polybar udev yay

echo "-----"

echo "Generating fstab file..."
ask_proceed_quiet
genfstab -U /mnt >> /mnt/etc/fstab
echo "Here is the new fstab file:"
indent "cat /mnt/etc/fstab"
ask_proceed_quiet

echo "-----"

echo "Copying over resource and environment files..."
ask_proceed_quiet
mkdir /mnt/archlinux
cp -r ./* /mnt/archlinux/

echo "-----"

echo "Executing install2.sh in chroot..."
ask_proceed
arch-chroot /mnt /bin/bash -c "cd /archlinux && chmod +x install2.sh && ./install2.sh"
# TODO: Very important - see if this is equivalent to running the commands themselves!

echo "-----"

echo "Unmounting /mnt to check for errors..."
umount /mnt/efi
umount /mnt
swapoff /dev/sda2
# TODO: Use fuser to check mount point usage

echo "Done!"
echo "Please reboot."
ask_proceed_quiet
