#!/bin/bash

function ask_proceed {
    read -s -p "Press Enter to proceed, Ctrl+C to abort..." _
    echo -en "\033[2K"; printf "\r"
}

if [ $# -gt 0 ]; then
    cat << EOM
This script is not intended to be run manually.
Please use install1.sh instead.
EOM
fi

echo "Entering chroot..."

echo "Loading keyboard layout..."
ask_proceed
loadkeys hu

echo "Updating the system clock to use network time..."
ask_proceed
timedatectl set-ntp true

echo "Setting local time..."
ask_proceed
ln -sf /usr/share/zoneinfo/Europe/Budapest /etc/localtime

echo "Synchronizing hardware clock..."
ask_proceed
hwclock --systohc

echo "Generating locales..."
ask_proceed
sed -i 's/#en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/g' /etc/locale.gen
sed -i 's/#hu_HU.UTF-8 UTF-8/hu_HU.UTF-8 UTF-8/g' /etc/locale.gen
echo "LANG=en_US.UTF-8" > /etc/locale.conf
echo "KEYMAP=hu" > /etc/vconsole.conf
locale-gen

echo "Setting hostname..."
ask_proceed
echo "dinatamas-laptop" > /etc/hostname

echo "Setting localhost IP adresses..."
ask_proceed
echo "dinatamas-laptop" > /etc/hostname
echo "" >> /etc/hosts
echo "127.0.0.1    localhost" >> /etc/hosts
echo "::1          localhost" >> /etc/hosts
echo "127.0.1.1    dinatamas-laptop.localdomain dinatamas-laptop" >> /etc/hosts

echo "Setting root password..."
ask_proceed
passwd

echo "Fixing wifi issues..."
ask_proceed
echo "options rtw88_pci disable_aspm=1" > /etc/modprobe.d/rtw88_pci.conf
echo "options rtw88_core lps_deep_mode=0" > /etc/modprobe.d/rtw88_core.conf

echo "Copying over network configuration..."
ask_proceed
cp ./network/systemd/*.network /etc/systemd/network/
cp ./network/systemd/wpa_supplicant.conf /etc/wpa_supplicant/wpa_supplicant.conf

echo "Installing the GRUB boot loader..."
ask_proceed
mkdir /efi
mount /dev/sda1 /efi
grub-install --target=x86_64-efi --efi-directory=/efi --bootloader-id=GRUB

echo "Enabling Intel CPU microcode early loading..."
ask_proceed
grub-mkconfig -o /boot/grub/grub.cfg

echo "Creating a new user..."
ask_proceed
mkdir /home/dinatamas
useradd dinatamas

echo "Setting the password for new user..."
ask_proceed
passwd dinatamas

echo "Copying configuration files..."
ask_proceed


cp ./vimrc /home/dinatamas/.vimrc
mkdir /home/dinatamas/.vim/
cp ./bash_profile /home/dinatamas/.bash_profile
cp ./bashrc /home/dinatamas/.bashrc
cp ./tmux.conf /home/dinatamas/.tmux.conf
ln -sf /home/dinatamas/.vimrc /root/.vimrc
ln -sf /home/dinatamas/.vim/ /root/.vim/
ln -sf /home/dinatamas/.bash_profile /root/.bash_profile
ln -sf /home/dinatamas/.bashrc /root/.bashrc
ln -sf /home/dinatamas/.tmux.conf /root/.tmux.conf
chown -R dinatamas:dinatamas /home/dinatamas

echo "Configuring sudo..."
ask_proceed
groupadd sudo
usermod -aG sudo dinatamas

echo "Enabling color in pacman..."
ask_proceed
sed -i 's/#Color/Color/g' /etc/pacman.conf
