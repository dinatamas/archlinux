#!/bin/bash

function ask_proceed {
    read -s -p "Press Enter to proceed, Ctrl+C to abort..." _
    echo -en "\033[2K"; printf "\r"
}

while [ $# -gt 0 ]; do
    arg="$1"
    shift
    case $arg in
        -h | --help)
            cat << EOM
Start this script in the initial archlinux virtual console.
Please make sure to read through the official Installation Guide.
Configuration of this script can be done by editing it.
EOM
	    exit 0
            ;;
        *)
            echo "Invalid argument: $arg"
            exit 1
            ;;
     esac
done

echo "Verifying that the computer has network connectivity..."
ask_proceed
ping archlinux.org -c 3 &>/dev/null
if [ $? -ne 0 ]; then
    echo "Error: No network connectivity!"
    exit 1
fi

echo "Loading keyboard layout..."
ask_proceed
loadkeys hu

echo "Verifying that the system was booted in UEFI mode..."
ask_proceed
ls /sys/firmware/efi/efivars &>/dev/null
if [ $? -ne 0 ]; then
    echo "Warning: The system wasn't booted in UEFI mode!";
    ask_proceed
fi

echo "Backing up current partition layout to sda.dump..."
ask_proceed
sfdisk --dump /dev/sda > sda.dump

echo "Creating new partition layout..."
echo "If you proceed, all will be erased from sda."
ask_proceed
read -r -d '' sfdisk_script << EOM
label: gpt
/dev/sda1 : size=500MiB type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B
/dev/sda2 : size=4GiB type=0657FD6D-A4AB-43C4-84E5-0933C84B4F4F
/dev/sda3 : type=4F68BCE3-E8CD-4DB1-96E7-FBCAF984B709
EOM
echo "$sfdisk_script" | sfdisk --wipe always /dev/sda

echo "Creating new filesystems..."
ask_proceed
mkfs.fat -F32 /dev/sda1
mkswap /dev/sda2
mkfs.ext4 /dev/sda3

echo "Mounting new filesystems..."
ask_proceed
mkdir /mnt/efi
mount /dev/sda1 /mnt/efi
swapon /dev/sda2
mount /dev/sda3 /mnt

echo "Generating new mirror file..."
ask_proceed
curl "https://www.archlinux.org/mirrorlist/?country=DE&protocol=https&ip_version=4&use_mirror_status=on" > ./mirrorlist &>/dev/null
sed -i 's/#Server/Server/g' ./mirrorlist
mv ./mirrorlist /etc/pacman.d/mirrorlist

echo "Installing packages..."
ask_proceed
packagelist="base base-devel efibootmgr git grub intel-ucode linux linux-firmware"
packagelist+="man-db man-pages texinfo"
packagelist+="networkmanager"
packagelist+="cronie curl diffutils emacs less openssh openssl rsync sudo util-linux wget"
packagelist+="bzip2 gzip p7zip unzip zip"
packagelist+="cfdisk fdisk htop hwinfo lshw mc neofetch"
packagelist+="tmux transmission-cli"
packagelist+="powerline"
packagelist+="docker jo jq python3.8"
packagelist+="alsa-utils pavucibtrik pciutils pulseaudio usbutils"
packagelist+="dmenu gnu-free-fonts i3 xorg-server xorg-xinit xorg-xrandr"
packagelist+="firefox terminator"
pacstrap /mnt $packagelist

echo "Generating fstab file..."
ask_proceed
genfstab -U /mnt >> /mnt/etc/fstab

echo "Copying over resource and environment files..."
ask_proceed
mkdir /mnt/archlinux
cp -r ./* /mnt/archlinux/

echo "Executing install2.sh in chroot..."
ask_proceed
arch-chroot /mnt /bin/bash -c "cd /archlinux && chmod +x install2.sh && ./install2.sh"

echo "Unmounting /mnt to check for errors..."
umount /mnt/efi
umount /mnt
swapoff /dev/sda2

echo "Done!"
echo "Please reboot."
ask_proceed
