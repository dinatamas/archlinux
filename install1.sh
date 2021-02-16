#!/bin/bash
#
# This is the first installation script.
# Start this script in the initial archlinux virtual console.
#
# For details visit:
# https://wiki.archlinux.org/index.php/Installation_guide
#
function ask_proceed {
    read -s -p "Press Enter to proceed, Ctrl+C to abort..." _
    # This will clear the previously printed prompt line.
    echo -en "\033[2K"; printf "\r"
}

function echo_and_log {
    echo "$1" | tee -a ./install.log
}

if [ $# -gt 0 ]; do
    cat << EOM
Start this script in the initial archlinux virtual console.
Please make sure to read through the README.
EOM
fi

echo_and_log "Logging to ./install.log..."
ask_proceed
> ./install.log  # Empty the log file.

echo_and_log "Verifying that the computer has network connectivity..."
ask_proceed
ping archlinux.org -c 1 &>> ./install.log
if [ $? -ne 0 ]; then
    echo_and_log "Error: No network connectivity!"
    exit 1
fi

echo_and_log "Loading keyboard layout..."
ask_proceed
loadkeys hu

echo_and_log "Verifying that the system was booted in UEFI mode..."
ask_proceed
ls /sys/firmware/efi/efivars &> /dev/null
if [ $? -ne 0 ]; then
    echo_and_log "Warning: The system wasn't booted in UEFI mode!";
    ask_proceed
fi

echo_and_log "Backing up current partition layout to sda.dump..."
ask_proceed
sfdisk --dump /dev/sda > sda.dump

echo_and_log "Creating new partition layout..."
echo_and_log "If you proceed, all data will be erased from sda."
ask_proceed
read -r -d '' sfdisk_script << EOM
label: gpt
/dev/sda1 : size=500MiB type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B
/dev/sda2 : size=4GiB type=0657FD6D-A4AB-43C4-84E5-0933C84B4F4F
/dev/sda3 : type=4F68BCE3-E8CD-4DB1-96E7-FBCAF984B709
EOM
echo "$sfdisk_script" | sfdisk --wipe always /dev/sda &>> ./install.log

echo_and_log "Creating new filesystems..."
ask_proceed
mkfs.fat -F32 /dev/sda1 &>> ./install.log
mkswap /dev/sda2 &>> ./install.log
mkfs.ext4 /dev/sda3 &>> ./install.log

echo_and_log "Mounting new filesystems..."
ask_proceed
mkdir /mnt/efi &>> ./install.log
mount /dev/sda1 /mnt/efi &>> ./install.log
swapon /dev/sda2 &>> ./install.log
mount /dev/sda3 /mnt &>> ./install.log

echo_and_log "Generating new mirror file..."
ask_proceed
curl -S -o ./mirrorlist "https://www.archlinux.org/mirrorlist/?country=DE&protocol=https&ip_version=4&use_mirror_status=on" 2> ./install.log
sed -i 's/#Server/Server/g' ./mirrorlist &>> ./install.log
mv ./mirrorlist /etc/pacman.d/mirrorlist &>> ./install.log

echo_and_log "Installing packages..."
ask_proceed
packagelist="  acpilight alsa-utils atom"
packagelist+=" base base-devel bzip2"
packagelist+=" cronie curl"
packagelist+=" diffutils docker"
packagelist+=" efibootmgr emacs"
packagelist+=" firefox fish flameshot"
packagelist+=" git gnu-free-fonts gparted grub gvfs-mtp gzip"
packagelist+=" htop hwinfo"
packagelist+=" i3 intel-ucode"
packagelist+=" jq"
packagelist+=" less linux linux-firmware lshw"
packagelist+=" man-db man-pages mc"
packagelist+=" neofetch networkmanager nitrogen ntfs-3g"
packagelist+=" openssh openssl"
packagelist+=" p7zip pavucontrol pciutils powerline pulseaudio"
packagelist+=" qbittorrent"
packagelist+=" re2 rofi rsync"
packagelist+=" sudo"
packagelist+=" texinfo thunar tmux"
packagelist+=" unrar unzip usbutils util-linux"
packagelist+=" vlc"
packagelist+=" wget"
packagelist+=" xclip xcompmgr xorg-server xorg-xinit xorg-xrandr xterm-unicode"
packagelist+=" zip"
pacstrap /mnt $packagelist &>> ./install.log

echo_and_log "Generating fstab file..."
ask_proceed
genfstab -U /mnt >> /mnt/etc/fstab

echo_and_log "Copying over resource and environment files..."
ask_proceed
cp -rT /archlinux /mnt &>> ./install.log

echo_and_log "Executing install2.sh in chroot..."
ask_proceed
arch-chroot /mnt /bin/bash -c "cd /archlinux && chmod +x install2.sh && ./install2.sh"

echo_and_log "Unmounting mount points to check for errors..."
umount /mnt/efi &>> ./install.log
umount /mnt &>> ./install.log
swapoff /dev/sda2 &>> ./install.log

echo_and_log "Done!"
echo_and_log "Please reboot."
ask_proceed
