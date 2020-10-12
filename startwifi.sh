#!/bin/bash

function indent() {
    eval "$@" |& sed "s/^/\t/" ; return "$PIPESTATUS"
}

# TODO: ensure root
# TODO: ensure fix_wifi is in place
# TODO: ensure wpa_supplicant.conf is OK

# Start network services
systemctl start systemd-networkd
systemctl start systemd-resolved
sleep 2

# Query the status
echo "Current networkctl status of wlo1:"
#indent 'networkctl status wlo1'
echo "Current ip link status of wlo1:"
#indent 'ip link show wlo1'

# Turn wifi on
networkctl up wlo1
sleep 2

# Show wifi driver kernel messages
dmesg | grep "rtw_8822be" | grep -v -e "Modules linked in"

# Start wpa_supplicant
wpa_supplicant -B -i wlo1 -c /etc/wpa_supplicant/wpa_supplicant.conf

# Show the available wifi networks
wpa_cli scan
wpa_cli scan_results

# Obtain IP address
dhcpcd
sleep 4

# Verify connectivity
ping -c 1 archlinux.org

# Note: to add a new wifi network, edit the configuration file
# /etc/wpa_supplicant/wpa_supplicant.conf, by adding new networks
# like the examples already included in there.
# The entries can be generated using wpa_passphrase.
