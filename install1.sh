#!/bin/bash

COLOR_RED=$(tput setaf 1)
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
        read -p "Proceed? [y/N] " reply
        if [[ -z "$reply" ]]; then
            echo "Exiting..."
            exit 0
        fi
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
ping archlinux.org -c 1 &>/dev/null
if [ $? -ne 0 ]; then
    echo "${COLOR_RED}Error: No network connectivity!${COLOR_RESET}"
    echo "Current network configuration:"
    indent 'networkctl'
    echo
    indent 'ip link'
    exit 1
fi

echo "-----"

echo "Loading Hungarian keyboard layout..."
ask_proceed_quiet
loadkeys hu

# TODO: setfont from /usr/share/kbd/consolefonts

echo "-----"

echo "Verifying that the system was booted in UEFI mode..."
ask_proceed_quiet
ls /sys/firmware/efi/efivars &>/dev/null
if [ $? -ne 0 ]; then
    echo "${COLOR_RED}Warning: The system wasn't booted in UEFI mode!${COLOR_RESET}";
    ask_proceed
fi

echo "-----"

echo "Updating the system clock to use network time data..."
ask_proceed_quiet
timedatectl set-ntp true
echo "New time and date configuration:"
indent 'timedatectl status'
ask_proceed_quiet

echo "-----"

echo "Old disk layout on /dev/sda:"
indent 'fdisk -l /dev/sda'
echo
indent 'lsblk -o "NAME,PATH,SIZE,TYPE,FSTYPE,MOUNTPOINT"'
ask_proceed_quiet

echo "-----"

echo "Backing up current partition layout..."
ask_proceed_quiet
sfdisk --dump /dev/sda > sda.dump
echo "The contents of ./sda.dump:"
ask_proceed_quiet
indent 'cat sda.dump'

echo "-----"

echo "Creating new partition layout..."
ask_proceed_quiet
# sfdisk --wipe /dev/sda4F68BCE3-E8CD-4DB1-96E7-FBCAF984B709
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
# sfdisk /dev/sda << sfdisk_script

echo "-----"

echo "Creating new filesystems..."
ask_proceed_quiet
# mkfs.fat -F32 /dev/sda1
# mkswap /dev/sda2
# mkfs.ext4 /dev/sda3

echo "-----"

echo "Mounting new filesystems..."
ask_proceed_quiet
# mkdir /mnt/efi
# mount /dev/sda1 /mnt/efi
# swapon /dev/sda2
# mount /dev/sda3 /mnt

echo "-----"

echo "New disk layout on /dev/sda:"
indent 'fdisk -l /dev/sda'
echo
indent 'lsblk -o "NAME,PATH,SIZE,TYPE,FSTYPE,MOUNTPOINT"'
ask_proceed_quiet

echo "-----"

echo "Generating new mirror file..."
ask_proceed_quiet
cp /etc/pacman.d/mirrorlist /etc/pacman.d/mirrorlist.bak
echo "Original mirrorfile backed up to /etc/pacman.d/mirrorlist.bak"
curl "https://www.archlinux.org/mirrorlist/?country=HU&protocol=https&ip_version=4&use_mirror_status=on" > /etc/pacman.d/mirrorlist 2>/dev/null
sed -i 's/#Server/Server/g' /etc/pacman.d/mirrorlist
echo "The new mirrors:"
indent "cat /etc/pacman.d/mirrorlist"
ask_proceed_quiet

echo "-----"

echo "Installing essential packages..."
echo "The following packages will be installed:"
packagelist="7z base base-devel cron curl dhcpcd diff efibootmgr ftp git grub htop intel-ucode jo jq less linux linux-firmware man-db man-pages mc openssh openssl rsync scp sudo texinfo tmux vi vim wget wpa_supplicant zip"
# fdisk dhclient dhcping iw iwd network-manager firefox terminator transmission transmission-cli
# codecs? nvidia-drivers?  hwinfo neofetch lshw tex pandoc sftp security? pwn? nvim neovim?
# python3.8 desktop-environment i3 docker vlc spotify todoist vscode/atom/sublime calendar? bitwarden
# nano GNU-binutils? moreutils [another shell? zsh, fish, ...]
# polkit? udev mlocate
# pulseaudio pavucontrol
# xorg-xinit xorg-server xorg-xrandr
# i3 gnu-free-fonts
# terminator
# dmenu
# polybar? yay?
echo $packagelist
ask_proceed
# pacstrap /mnt $packagelist # &>/dev/null

echo "-----"

echo "Generating fstab file..."
ask_proceed_quiet
# genfstab -U /mnt >> /mnt/etc/fstab
echo "Here is the new fstab file:"
cat /mnt/etc/fstab

echo "-----"

echo "Copying over resource and environment files..."
ask_proceed_quiet
# cp ./vimrc /mnt/vimrc
# cp ./bashrc /mnt/bashrc

echo "-----"

echo "Performing install2.sh in chroot..."
ask_proceed
# cp ./install2.sh /mnt/install2.sh
# arch-chroot /mnt /bin/bash -c "chmod +x install2.sh && ./install2.sh"

echo "-----"

echo "Done!"
echo "Please reboot"
ask_proceed_quiet
