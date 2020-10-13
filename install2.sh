#!/bin/bash

COLOR_RED=$(tput setaf 1)
BOLD=$(tput bold)
COLOR_RESET=$(tput sgr0)

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

echo
echo "=================="
echo "Entering chroot..."
echo "=================="
echo

echo "Setting local time to Europe/Budapest..."
ask_proceed_quiet
ln -sf /usr/share/zoneinfo/Europe/Budapest /etc/localtime

echo "-----"

echo "Synchronizing hardware clock..."
ask_proceed_quiet
hwclock --systohc

echo "-----"

echo "Enabling HU and en_US locale..."
ask_proceed_quiet
sed -i '' /etc/locale.gen
sed -i '' /etc/locale.gen
echo "LANG=en_US.UTF-8" > /etc/locale.conf
echo "KEYMAP=hu" > /etc/vconsole.font

echo "-----"

echo "Setting hostname to dinatamas-laptop..."
ask_proceed_quiet

echo "-----"

echo "Setting localhost IP adresses..."
ask_proceed_quiet
#an extra newline here?
echo "dinatamas-laptop" > /etc/hostname
echo "127.0.0.1    localhost" >> /etc/hosts
echo "::1          localhost" >> /etc/hosts
echo "127.0.1.1    dinatamas-laptop.localdomain dinatamas-laptop" >> /etc/hosts

echo "-----"

echo "Setting root password..."
while true; do
    echo -n "New root password:"
    read -s reply1
    echo -en "\nNew root password again: "
    read -s reply2
    if [ $reply1 == $reply2 ]; then
        echo "$reply1" | passwd
        break
    fi
    echo -e "\\nnThe passwords do not match"
done
echo -e "\nThe new root password has been set."

echo "-----"

echo "Fixing the wifi connection..."
ask_proceed
echo "options rtw88_pci disable_aspm=1" > /etc/modprobe.d/rtw88_pci.conf
echo "options rtw88_core lps_deep_mode=0" > /etc/modprobe.d/rtw88_core.conf
echo "ctrl_interface=/run/wpa_supplicant" > /etc/wpa_supplicant/wpa_supplicant.conf
echo "update_config=1" >> /etc/wpa_supplicant/wpa_supplicant.conf
# copy wifi network entries (or generate via wpa_passphrase)
# separate fixwifi script?

echo "-----"

echo "Configuring git globally..."
git config --global user.name "dinatamaspal"
git config --global user.email "53911660+dinatamas@users.noreply.github.com"

echo "-----"

echo "Installing the GRUB boot loader..."
ask_proceed_quiet
grub-install --target=x86_64-efi --efi-directory=/efi --bootloader-id=GRUB
echo "Enabling Intel CPU microcode early loading..."
#TODO: intel-ucode (microcode setup)
grub-mkconfig -o /boot/grub/grub.cfg

echo "-----"

# Review the following (John Hammond)
mkdir /home/dinatamas
useradd dinatamas
groupadd sudo
usermod -aG sudo dinatamas
sed -i 's/# %sudo/%sudo/g' /etc/sudoers
echo "dinatamas ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
cp ./vimrc /home/dinatamas/.vimrc
rm ./vimrc
cp ./bashrc /home/dinatamas/.bashrc
rm ./bashrc
chown -R dinatamas:dinatamas /home/dinatamas

echo "-----"

echo "Done!"
ask_proceed_quiet
umount /mnt/efi
umount /mnt
#TODO: use fuser to check mount point usage
exit

echo
echo "================="
echo "Exiting chroot..."
echo "================="
echo
