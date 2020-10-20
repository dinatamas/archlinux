if [ $TERM -eq "linux" ]; then
    export TERM="linux-16color"
fi

echo -en "\e]P0eee8d5" # Black          -- Base2
echo -en "\e]P1dc322f" # Red            -- Red
echo -en "\e]P2859900" # Green          -- Green
echo -en "\e]P3b58900" # Yellow         -- Yellow
echo -en "\e]P4268bd2" # Blue           -- Blue
echo -en "\e]P5d33682" # Magenta        -- Magenta
echo -en "\e]P62aa198" # Cyan           -- Cyan
echo -en "\e]P7073642" # White          -- Base02
echo -en "\e]P8fdf6e3" # Gray           -- Base3
echo -en "\e]P9dc322f" # Bright red     -- Red
echo -en "\e]PA859900" # Bright green   -- Green
echo -en "\e]PBb58900" # Bright yellow  -- Yellow
echo -en "\e]PC268bd2" # Bright blue    -- Blue
echo -en "\e]PEd33682" # Bright magenta -- Magenta
echo -en "\e]PD2aa198" # Bright cyan    -- Cyan
echo -en "\e]PF002b36" # Bright white   -- Base03
clear

# Should 9 become Orange instead? #cb4b16
# Should D become VIolet instead? #6c71c4
