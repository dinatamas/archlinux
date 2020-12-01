#!/bin/bash

##############################################################################
# General script-utilities.                                                  #
##############################################################################

COLOR_RED=$(tput setaf 1)
BOLD=$(tput bold)
COLOR_RESET=$(tput sgr0)

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

##############################################################################
# This is where the installation script begins.                              #
##############################################################################

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

echo "-----"

echo "Enabling HU and en_US locale..."
ask_proceed_quiet
sed -i 's/#en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/g' /etc/locale.gen
sed -i 's/#hu_HU.UTF-8 UTF-8/hu_HU.UTF-8 UTF-8/g' /etc/locale.gen
echo "LANG=en_US.UTF-8" > /etc/locale.conf
echo "KEYMAP=hu" > /etc/vconsole.conf
locale-gen

echo "Copying console fonts..."
ask_proceed_quiet
cp ./fonts/cli/* /usr/share/kbd/consolefonts/

echo "Changing default console font..."
ask_proceed_quiet
echo "FONT=\"ter-powerline-v14n-custom\"" >> /etc/vconsole.conf

echo "-----"

echo "Setting hostname to dinatamas-laptop..."
ask_proceed_quiet
echo "dinatamas-laptop" > /etc/hostname

echo "Setting localhost IP adresses..."
ask_proceed_quiet
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

echo "-----"

echo "Copying over network configuration..."
ask_proceed_quiet
cp ./network/systemd/*.network /etc/systemd/network/
cp ./network/systemd/wpa_supplicant.conf /etc/wpa_supplicant/wpa_supplicant.conf

echo "-----"

echo "Configuring git globally..."
ask_proceed_quiet
git config --global user.name "dinatamaspal"
git config --global user.email "53911660+dinatamas@users.noreply.github.com"
git config --global core.editor "vim"
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

echo "Creating a new user called dinatamas..."
ask_proceed_quiet
mkdir /home/dinatamas
useradd dinatamas

echo "Setting the password for dinatamas..."
ask_proceed_quiet
passwd dinatamas

echo "Copying configuration files..."
ask_proceed_quiet
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
ask_proceed_quiet
groupadd sudo
usermod -aG sudo dinatamas
sed -i 's/# %sudo/%sudo/g' /etc/sudoers

echo "Verifying sudo..."
indent 'visudo -c'
ask_proceed_quiet

echo "-----"

echo "Copying GUI fonts..."
ask_proceed_quiet
mkdir /home/dinatamas/.fonts
cp ./fonts/gui/* /home/dinatamas/.fonts
ln -sf /home/dinatamas/.fonts/ /root/.fonts/

echo "-----"

echo "Installing vim plugin manager..."
ask_proceed_quiet
curl -sfLo /home/dinatamas/.vim/autoload/plug.vim --create-dirs "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
vim /home/dinatamas/.vimrc +PlugInstall +q +q

echo "Enabling color in pacman..."
ask_proceed_quiet
sed -i 's/#Color/Color/g' /etc/pacman.conf

echo "Setting up powerline bash prompt..."
ask_proceed_quiet
sed -i 's/\(.*depth.*\)3/\11/' /usr/lib/python3.8/site-packages/powerline/config_files/themes/shell/__main__.json

echo "-----"

echo "Done!"
echo "The installation sources will remain in /archlinux."
ask_proceed_quiet
exit

echo
echo "================="
echo "Exiting chroot..."
echo "================="
echo
