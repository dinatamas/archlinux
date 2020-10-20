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
echo -en "\e]P9dc322f" # Bright red     -- Red
echo -en "\e]PA859900" # Bright green   -- Green
echo -en "\e]PBb58900" # Bright yellow  -- Yellow
echo -en "\e]PC268bd2" # Bright blue    -- Blue
echo -en "\e]PE2aa198" # Bright cyan    -- Cyan
echo -en "\e]PDd33682" # Bright magenta -- Magenta
echo -en "\e]PFfdf6e3" # Bright white   -- Base3
clear

# Should 9 become Orange instead? #cb4b16
# Should D become Violet instead? #6c71c4
