#!/bin/bash

COLOR_RED=$(tput setaf 1)
BOLD=$(tput bold)
COLOR_RESET=$(tput sgr0)

function ask_proceed() {
    while true; do
        read -p "Proceed? [y/N] " reply
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

echo
echo "=================="
echo "Entering chroot..."
echo "=================="
echo

echo "Loading Hungarian keyboard layout..."
ask_proceed_quiet
loadkeys hu

echo "-----"

echo "Updating the system clock to use network time data..."
ask_proceed_quiet
timedatectl set-ntp true

echo "Setting local time to Europe/Budapest..."
ask_proceed_quiet
ln -sf /usr/share/zoneinfo/Europe/Budapest /etc/localtime

echo "Synchronizing hardware clock..."
ask_proceed_quiet
hwclock --systohc

echo "New time and date configuration:"
indent 'timedatectl status'  # FIXME: still displays wrong status. Maybe exec bash -c...?
ask_proceed_quiet

echo "-----"

echo "Enabling HU and en_US locale..."
ask_proceed_quiet
sed -i 's/#en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/g' /etc/locale.gen
sed -i 's/#hu_HU.UTF-8 UTF-8/hu_HU.UTF-8 UTF-8/g' /etc/locale.gen
echo "LANG=en_US.UTF-8" > /etc/locale.conf
echo "KEYMAP=hu" > /etc/vconsole.conf
locale-gen

echo "-----"

echo "Setting hostname to dinatamas-laptop..."
ask_proceed_quiet
echo "dinatamas-laptop" > /etc/hostname

echo "Setting localhost IP adresses..."
ask_proceed_quiet
# TODO: An extra newline here?
echo "dinatamas-laptop" > /etc/hostname
echo "" >> /etc/hosts
echo "127.0.0.1    localhost" >> /etc/hosts
echo "::1          localhost" >> /etc/hosts
echo "127.0.1.1    dinatamas-laptop.localdomain dinatamas-laptop" >> /etc/hosts

echo "-----"

echo "Setting root password..."
ask_proceed
passwd
echo "The new root password has been set."

echo "-----"

echo "Fixing the wifi connection..."
ask_proceed
echo "options rtw88_pci disable_aspm=1" > /etc/modprobe.d/rtw88_pci.conf
echo "options rtw88_core lps_deep_mode=0" > /etc/modprobe.d/rtw88_core.conf
echo "ctrl_interface=/run/wpa_supplicant" > /etc/wpa_supplicant/wpa_supplicant.conf
echo "update_config=1" >> /etc/wpa_supplicant/wpa_supplicant.conf
# TODO: Separate fixwifi script?

echo "-----"

echo "Copying over network configuration..."
ask_proceed
cp ./networks/*.network /etc/systemd/network/

echo "-----"

echo "Configuring git globally..."
ask_proceed_quiet
git config --global user.name "dinatamaspal"
git config --global user.email "53911660+dinatamas@users.noreply.github.com"
# TODO: more git config options

echo "-----"

echo "Installing the GRUB boot loader..."
ask_proceed_quiet
mkdir /efi
mount /dev/sda1 /efi
grub-install --target=x86_64-efi --efi-directory=/efi --bootloader-id=GRUB
echo "Enabling Intel CPU microcode early loading..."
grub-mkconfig -o /boot/grub/grub.cfg

echo "-----"

# TODO: Review the following (John Hammond)
echo "Creating a new user called dinatamas..."
ask_proceed_quiet
mkdir /home/dinatamas
useradd dinatamas

echo "Setting the password for dinatamas..."
ask_proceed_quiet
passwd

echo "Copying configuration files..."
ask_proceed_quiet
cp ./vimrc /home/dinatamas/.vimrc
cp ./bashrc /home/dinatamas/.bashrc
rm -rf archlinux/ 
chown -R dinatamas:dinatamas /home/dinatamas

echo "Configuring sudo..."
ask_proceed_quiet
groupadd sudo
usermod -aG sudo dinatamas
sed -i 's/# %sudo/%sudo/g' /etc/sudoers
echo "dinatamas ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

echo "-----"

echo "Done!"
ask_proceed_quiet
exit

echo
echo "================="
echo "Exiting chroot..."
echo "================="
echo
