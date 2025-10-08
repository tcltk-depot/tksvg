tksvg
======

This package adds support to read the SVG image format from Tk.
The actual code to parse and raster the SVG comes from nanosvg.

Example usage:

	package require tksvg
	set img [image create photo -file orb.svg]
	pack [label .l -image $img]
 
 Note: this package is not required for Tk 9.0, as this functionality is included in the core.
 The package index file simulates a load on this version but actually does nothing.
