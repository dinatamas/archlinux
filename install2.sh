#!/bin/bash

function ask_proceed {
    read -s -p "Press Enter to proceed, Ctrl+C to abort..." _
    echo -en "\033[2K"; printf "\r"
}

function echo_and_log {
    echo "$1" | tee -a ./install.log
}

if [ $# -gt 0 ]; then
    cat << EOM
This script is not intended to be run manually.
Please use install1.sh instead.
EOM
fi

echo_and_log "Entering chroot..."

echo_and_log "Loading keyboard layout..."
ask_proceed
loadkeys hu

echo_and_log "Updating the system clock to use network time..."
ask_proceed
timedatectl set-ntp true &>> ./install.log

echo_and_log "Setting local time..."
ask_proceed
ln -sf /usr/share/zoneinfo/Europe/Budapest /etc/localtime

echo_and_log "Synchronizing hardware clock..."
ask_proceed
hwclock --systohc &>> ./install.log

echo "Generating locales..."
ask_proceed
sed -i 's/#en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/g' /etc/locale.gen
sed -i 's/#hu_HU.UTF-8 UTF-8/hu_HU.UTF-8 UTF-8/g' /etc/locale.gen
echo "LANG=en_US.UTF-8" > /etc/locale.conf
echo "KEYMAP=hu" > /etc/vconsole.conf
locale-gen &>> ./install.log

echo_and_log "Setting hostname..."
ask_proceed
echo "dinatamas-laptop" > /etc/hostname

echo_and_log "Setting localhost IP adresses..."
ask_proceed
echo "dinatamas-laptop" > /etc/hostname
echo "" >> /etc/hosts
echo "127.0.0.1    localhost" >> /etc/hosts
echo "::1          localhost" >> /etc/hosts
echo "127.0.1.1    dinatamas-laptop.localdomain dinatamas-laptop" >> /etc/hosts

echo_and_log "Setting root password..."
ask_proceed
passwd

echo_and_log "Fixing wifi issues..."
ask_proceed
echo "options rtw88_pci disable_aspm=1" > /etc/modprobe.d/rtw88_pci.conf
echo "options rtw88_core lps_deep_mode=0" > /etc/modprobe.d/rtw88_core.conf

echo_and_log "Copying over network configuration..."
ask_proceed
cp ./network/systemd/*.network /etc/systemd/network/
cp ./network/systemd/wpa_supplicant.conf /etc/wpa_supplicant/wpa_supplicant.conf

echo_and_log "Installing the GRUB boot loader..."
ask_proceed
mkdir /efi &>> ./install.log
mount /dev/sda1 /efi &>> ./install.log
grub-install --target=x86_64-efi --efi-directory=/efi --bootloader-id=GRUB &>> ./install.log

echo_and_log "Enabling Intel CPU microcode early loading..."
ask_proceed
echo "GRUB_FORCE_HIDDEN_MENU=\"true\"" >> /etc/default/grub
cp 31_hold_shift /etc/grub.d/
chmod a+x 31_hold_shift
grub-mkconfig -o /boot/grub/grub.cfg &>> ./install.log

echo_and_log "Creating a new user..."
ask_proceed
mkdir /home/dinatamas &>> ./install.log
useradd dinatamas &>> ./install.log

echo_and_log "Setting the password for new user..."
ask_proceed
passwd dinatamas

echo_and_log "Fixing wifi issues..."
echo "options rtw88_pci disable_aspm=1" > /etc/modprobe.d/rtw88_pci.conf
echo "options rtw88_core lps_deep_mode=0" > /etc/modprobe.d/rtw88_core.conf

echo_and_log "Copying configuration files..."
ask_proceed
chown -R root:root /archlinux
chmod -R 777 /archlinux
chmod 700 /archlinux/scripts/secrets.fish

for home in "/root" "/home/dinatamas"; do
    cp -rs /archlinux/config/ $home/.config/
    pushd $home/.config/ &>> ./install.log
    mkdir gnupg
    popd &>> ./install.log
done

echo_and_log "Configuring sudo..."
ask_proceed
groupadd sudo &>> ./install.log
usermod -aG sudo dinatamas &>> ./install.log
echo "dinatamas ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/dinatamas

echo_and_log "Enabling color in pacman..."
ask_proceed
sed -i 's/#Color/Color/g' /etc/pacman.conf

echo_and_log "Setting up SSH key..."
ask_proceed
ssh-keygen -q -t ed25519 -f "/home/dinatamas/.ssh/id_ed25519" -C "general" -N ""
chmod 700 /home/dinatamas/.ssh
touch /home/dinatamas/.ssh/authorized_keys
chmod 644 /home/dinatamas/.ssh/authorized_keys
touch /home/dinatamas/.ssh/known_hosts
chmod 644 /home/dinatamas/.ssh/known_hosts
touch /home/dinatamas/.ssh/config
chmod 644 /home/dinatamas/.ssh/config
chmod 600 /home/dinatamas/.ssh/id_ed25519
chmod 644 /home/dinatamas/.ssh/id_ed25519.pub
chown -R dinatamas:dinatamas /home/dinatamas/.ssh

echo_and_log "Setting main shell to fish..."
ask_proceed
sudo dinatamas chsh -s `which fish` &>> ./install.log

echo_and_log "Installing tmux package manager..."
git clone https://github.com/tmux-plugins/tpm /archlinux/config/tmux/plugins/tpm
