#!/bin/bash

function indent() {
    eval "$@" |& sed "s/^/\t/" ; return "$PIPESTATUS"
}

# TODO: ensure root

# Turn wifi off
networkctl down wlo1
sleep 2

# Stop wpa_supplicant
killall wpa_supplicant
# TODO: multiple wpa_supplicant instances?

# Stop dhcpcd
killall dhcpcd
# TODO: multiple dhcpcd instances?

# Print daemon status
echo "Current systemd-networkd status"
indent 'systemctl status systemd-networkd'
echo "Current systemd-resolved status"
indent 'systemctl status systemd-resolved'

# Stop network daemons
systemctl stop systemd-networkd
systemctl stop systemd-resolved
