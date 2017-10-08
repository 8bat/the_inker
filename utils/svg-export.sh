#!/bin/sh
#
# Create PNG representations of SVG images
# (so you can compare them to the bitmap images)

for N in $( seq 0 256 )
do
    if [ -e "images/$N.svg" ]
    then
        echo -n "\r$N... "
        if ! RESULT="$( inkscape --file="images/$N.svg" --export-width=256 --export-height=96 --export-png="images/$N.svg.png" 2>&1 )"
        then
            echo failed.
            echo "$RESULT" >&2
        fi
    fi
done
