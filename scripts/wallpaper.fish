#!/usr/bin/env fish

switch (count $argv)
case 0
  set WALLPAPER (find "/archlinux/wallpapers/" -type f | sort -R | tail -1)
case 1
  set WALLPAPER (find "/archlinux/wallpapers/" -type f -path "*"(echo $argv[1])"*" | sort -R | tail -1)
end

nitrogen --head=0 --set-scaled --force-setter=xinerama "$WALLPAPER"
nitrogen --head=1 --set-scaled --force-setter=xinerama "$WALLPAPER" &>/dev/null
echo "$WALLPAPER"
