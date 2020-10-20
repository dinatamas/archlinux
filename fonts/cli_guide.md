# Guide: Manuplating CLI fonts

I used the following steps to create a font that fulfills the following criteria:
* It can be used from the Linux virtual console (tty).
* It support 16 colors (thus contains less than 256 glyphs).
* It supports the ISO-8859-2 standard (including Hungarian).
* It supports the Powerline characters.
* It looks nice.

## Modifying PSF fonts

1. `wget https://github.com/powerline/fonts/raw/master/Terminus/PSF/ter-powerline-v14n.psf.gz`
1. `gunzip ter-powerline-v14n.psf.gz`
1. Download and install psftools.
1. `psfd ter-powerline-v14n.psf > ter-powerline-v14n.txt`
1. Remove the "unwanted" character blocks from ter-powerline-v14n.txt.
1. Modify the "required" characters: e.g. move the wave into the "middle".
1. Make sure that the "blank" character is at number 32.
1. Use the fixpsfnumbers.py script to "fix" the numbers.
1. `psfc output.txt > ter-powerline-v14n-custom.psf`
1. Verify the file's type (and glyph numbers) via `file ter-powerline-v14n-custom.psf`.
1. Verify that the conversion was OK:
    1. `psfd ter-powerline-v14n-custom.psf > ter-powerline-v14n-custom.txt`
    1. `diff output.txt ter-powerline-v14n-custom.txt`

## Using the font with colors

1. `cp ter-powerline-v14n-custom.psf /usr/share/kbd/consolefonts/`
1. `setfont ter-powerline-v14n-custom`
1. `clear`
1. `showconsolefont`
1. (x=`tput op` y=`printf %76s`;for i in {0..15};do o=00$i;echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x;done)
