One of the most complicated issues in The Inker is deciding which lines describe a filled area.

If you get a message `Could not fill area <number>`, please [file a bug report](https://github.com/8bat/the_inker/issues/new).  The report should include:

* the full text of the error
* the UnQuill source
* the contents of the `error/` folder

Debugging fill errors
=====================

If you want to debug The Inker itself, here's some advice:

If you use GIMP, go to *Filters > Python-Fu > Console* and paste in the full contents of `utils/gimp_helpers.py`.  Then when you get a fill error, do:

    load_error_image(<room-number>)

This will load the room bitmap, error bitmap and path list.

If you use another graphics package, see the comments in `utils/gimp_helpers.py` for instructions.

Colours in `error/*.pnm` have the following meanings:

* black - this pixel is outside the filled area
* white - this pixel is on the border of the filled area
* grey  - this pixel is inside the filled area
* red   - this pixel is both inside and on the border of the filled area (should be impossible)
