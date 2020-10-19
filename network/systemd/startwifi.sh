#!/bin/bash

COLOR_YELLOW=$(tput setaf 3)

function indent() {
    eval "$@" |& sed "s/^/\t/" ; return "$PIPESTATUS"
}

if [ $UID != 0 ]; then
    echo "Please run this script as root."
    exit 1
fi

if [ ! -z $(grep "disable_aspm" /etc/modprobe.d/rtw88_core.conf ) ]; then
    echo "${COLOR_RED}Warning: rtw88 configuration may be missing!${COLOR_RESET}"
fi

# Start network services
systemctl start systemd-networkd
systemctl start systemd-resolved
sleep 1

# Turn wifi on
networkctl up wlo1
sleep 1

# Show wifi driver kernel messages
dmesg | grep "rtw_8822be" | grep -v -e "Modules linked in" | tail -n 10

# Start wpa_supplicant
wpa_supplicant -B -i wlo1 -c /etc/wpa_supplicant/wpa_supplicant.conf

# Obtain IP address
dhcpcd
sleep 1

# Note: to add a new wifi network, edit the configuration file
# /etc/wpa_supplicant/wpa_supplicant.conf, by adding new networks
# like the examples already included in there.
# The entries can be generated using wpa_passphrase.
