#!/usr/bin/env fish

set WALLPAPER (find "/archlinux/wallpapers/" -type f | sort -R | tail -1)
nitrogen --head=0 --set-scaled --force-setter=xinerama $WALLPAPER
nitrogen --head=1 --set-scaled --force-setter=xinerama $WALLPAPER &>/dev/null
