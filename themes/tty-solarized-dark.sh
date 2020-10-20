if [ $TERM -eq "linux" ]; then
    export TERM="linux-16color"
fi

echo -en "\e]P0073642" # Black          -- Base02
echo -en "\e]P1dc322f" # Red            -- Red
echo -en "\e]P2859900" # Green          -- Green
echo -en "\e]P3b58900" # Yellow         -- Yellow
echo -en "\e]P4268bd2" # Blue           -- Blue
echo -en "\e]P5d33682" # Magenta        -- Magenta
echo -en "\e]P62aa198" # Cyan           -- Cyan
echo -en "\e]P7eee8d5" # White          -- Base2
echo -en "\e]P8002b36" # Grey           -- Base03
echo -en "\e]P9cb4b16" # Bright red     -- Orange
echo -en "\e]PA586e75" # Bright green   -- Base01
echo -en "\e]PB657b83" # Bright yellow  -- Base00
echo -en "\e]PC839496" # Bright blue    -- Base0
echo -en "\e]PE93a1a1" # Bright cyan    -- Base1
echo -en "\e]PD6c71c4" # Bright magenta -- Violet
echo -en "\e]PFfdf6e3" # Bright white   -- Base3
clear
