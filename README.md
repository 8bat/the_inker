The Inker
=========

Prototype application for converting [UnQuill](http://www.seasip.info/Unix/UnQuill/) output to other formats.

This was designed as a way to explore the issues in converting old 8-bit adventure games.  It may well work with a variety of games, but has not been thoroughly tested and is no longer under active development.

At the time of writing, the [8-Bit Adventure Toolkit](https://github.com/8bat/8bat) is being written based on lessons learnt here.  Bug reports and feature requests are welcome, but are more likely to be included in that project than back-ported into this one.


Quick Start
-----------

1. Get a snapshot of your game, as described on [the main site](https://8bat.github.io/)

2. Decompile your game with UnQuill:

        ./unquill*/unquill mygame.sna > mygame.txt

3. Generate PNM and SVG versions of the graphics:

        ./script/the_inker.pl --out=mygame-graphics/ mygame.txt

4. Optionally convert PNM images to GIF (see below):

        ./script/convert-to-gif.sh mygame-graphics/

5. Optionally convert the game to [ngPAWS](http://www.ngpaws.com/) format:

        ./script/the_translator.pl --out=mygame-ngpaws --images=mygame-graphics mygame.txt


PNM vs. SVG
-----------

Normal image formats are based on [raster graphics](https://en.wikipedia.org/wiki/Raster_graphics) - they specify the colour of each pixel in a grid.  SVG uses [vector graphics](https://en.wikipedia.org/wiki/Vector_graphics) instead - it specifies properties of abstract shapes.  Quill "images" aren't exactly images at all, they're more like *little programs* that draw images using a mixture of raster- and vector-like commands.

To create PNM images, The Inker works like an emulator that runs Quill image-programs.  It should create perfect replicas of the images that would be created by The Quill.

To create SVG images, The Inker translates image-programs from The Quill's language to SVG's language.  This should produce attractive results in most cases, but can have difficulty translating some commands.

SVG images tend to be a less accurate representation of the original, and often need to be cleaned up manually before they look right.  But unlike raster images, they can be made to look good at any size without recreating the image from scratch.

There are many SVG editors available - you can even edit SVGs in Notepad - but [Inkscape](https://inkscape.org/) is probably the most widely-used.


PNM vs. GIF
-----------

This prototype produces PNM images because it was more convenient during development.  The actual images created by The Quill are more similar to GIFs - they can have one or two frames, and up to 16 colours.

If you want to convert your PNM images to GIF and have installed [ImageMagick](imagemagick.org), you can use `convert-to-gif.sh` to convert them:

    ./script/convert-to-gif.sh mygame-graphics/

Alternatively, you can create GIFs with your favourite graphics package:

1. Load any `<room>-a.pnm` image
2. If there is a `<room>-b.pnm`, load `<room>-b.pnm` as a second frame in the image
3. set each frame to display for 320 milliseconds
4. Save the image as `<room>.gif`

Creating GIFs is entirely optional - most graphics packages support PNM, and you might prefer to work with SVG images anyway.


License
-------

The Inker is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, version 2.

The Inker is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with The Inker.  If not, see <http://www.gnu.org/licenses/>.


See Also
--------

1. [The Interactive Fiction Archive's list of Quill and PAWS-related software](https://www.ifarchive.org/indexes/if-archiveXprogrammingXquill.html)
2. [The QUILL & PAW Reservoir](https://gilsoftpawreservoir.wordpress.com/)
3. [ngPAWS](https://github.com/Utodev/ngPAWS)
