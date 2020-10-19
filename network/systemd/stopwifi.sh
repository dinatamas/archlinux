#!/bin/bash

function indent() {
    eval "$@" |& sed "s/^/\t/" ; return "$PIPESTATUS"
}

if [ $UID != 0 ]; then
    echo "Please run this script as root."
    exit 1
fi

# Turn wifi off
networkctl down wlo1
sleep 1

# Stop wpa_supplicant
killall wpa_supplicant

# Stop dhcpcd
killall dhcpcd

# Stop network daemons
systemctl stop systemd-networkd
systemctl stop systemd-resolved
