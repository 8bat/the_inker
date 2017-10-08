#!/bin/sh
#
# Copyright (C) 2017, Andrew Sayers.
#
# This file is part of The Inker
#
# The Inker is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 2.
#
# The Inker is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

if [ -z "$@" ]
then
    cat <<EOF
Usage: $0 <directory> [ ... ]

Convert the PNM images created by The Inker to (animated) GIFs

Requires Image Magick (https://www.imagemagick.org/)
EOF
    exit 1
fi

if ! convert --version >/dev/null  2>/dev/null
then
    cat <<EOF
This program requires \`convert\` from the ImageMagick package to be available
on the command-line.

Please install ImageMagick (imagemagick.org) and try again.
EOF
    exit 1
fi

for DIR in "$@"
do
    for N in $( seq 0 255 )
    do
        if [ -e "$DIR/$N-a.pnm" ]
        then
            echo -n "$N\r"
            if [ -e "$DIR/$N-b.pnm" ]
            then
               # Images with flashing colours will have "-a" and "-b" PNMs.
               # The ZX Spectrum displays 50 frames per second,
               # and flashes every 16 frames (320ms):
               convert -delay 32 "$DIR/$N"-[ab].pnm -loop 0 "$DIR/$N.gif"
            else
               convert           "$DIR/$N-a.pnm"            "$DIR/$N.gif"
            fi
        fi
    done
done
