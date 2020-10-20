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
echo -en "\e]P8fdf6e3" # Grey           -- Base3
echo -en "\e]P9cb4b16" # Bright red     -- Orange
echo -en "\e]PA93a1a1" # Bright green   -- Base1
echo -en "\e]PB839496" # Bright yellow  -- Base0
echo -en "\e]PC657b83" # Bright blue    -- Base00
echo -en "\e]PE586e75" # Bright magenta -- Base01
echo -en "\e]PD6c71c4" # Bright cyan    -- Violet
echo -en "\e]PF002b36" # Bright white   -- Base03
clear
