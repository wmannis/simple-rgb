# SIMPLE-RGB

This library was designed to be used by another library which
generates SVG files.  It is currently focused on the small set of
basic color manipulations (lightening, compliments, etc) you might use
to generate a color palette for a GUI or web page.

(March 2019) The current all-singing, all-dancing color manipulation
library is [https://github.com/privet-kitty/dufy](Dufy), which you
should consider if you have serious color manipulation needs.

## Package
The package name is *`:simple-rgb`*, with a single nickname, *`:rgb`*.

It depends on no other libraries, but you will need
[http://common-lisp.net/project/lift/](LIFT) to run the unit tests. 

## Conditions and Restarts

_[condition]_  
**`hsv-type-error`**  
This condition is raised by *`hsv`* if any of the three values passed
to it falls outside the range 0.0 through  1.0. 

There are no restarts.

## Functions
_[function]_  
**`rgb`** _`r g b` => rgb-vector_  
This is the only way to create a literal RGB value the rest of the
library will accept.  Each color element is an integer between 0 and
255.  Anything outside this range, or of a non-integer type, will
raise a `type-error` condition. 

_[function]_  
**`rgb=`** _`a b` => boolean_  
Tests RGB equality.

_[function]_  
**`hsv`** _`h s v` => hsv-vector_  
This is the only way to create a literal HSV value the rest of the
library will accept.  Each of the values has to be a float between 0.0
and 1.0.  A range or type problem will result in a `hsv-type-error`
condition being raised. 

Note that some HSV libraries represent the _h_ element in degrees.
This one does not. 

_[function]_  
**`mix-rgb`** _`a b &key (alpha 0.5)` => rgb-vector_  
Blends two RGB colors.  By default the mix is even, but by setting the
`:alpha` keyword you can weight the mix, with 0.0 simply returning the
first argument, 1.0 the second. 

_[function]_  
**`lighten-rgb`** _`a` => rgb-vector_  
Lightens a color by mixing it with white with an alpha of 0.5.  For
different shading use *`mix-rgb`* directly. 

_[function]_  
**`darken-rgb`** _`a` => rgb-vector_  
Darkens a color by mixing it with black with an alpha of 0.5.  For
different shading use *`mix-rgb`* directly. 

_[function]_  
**`greyscale-rgb`** _`a` => rgb-vector_  
Converts a color to greyscale by setting each color element to the
value _0.3r + 0.59g + 0.11b_.  See:
[http://en.wikipedia.org/wiki/Grayscale](Grayscale). 

_[function]_  
**`invert-rgb`** _`a` => rgb-vector_  
Inverts a color.

_[function]_  
**`complement-rgb`** _`a` => rgb-vector_  
Creates the complement of the color using the forumla described at [http://livedocs.adobe.com/en_US/Illustrator/13.0/help.html?content=WS714a382cdf7d304e7e07d0100196cbc5f-6288.html](Adobe).

_[function]_  
**`rgb->hsv`** _`a` => hsv-vector_  
Converts an RGB value to HSV.

_[function]_  
**`hsv->rgb`** _`a` => rgb-vector_  
Converts and HSV value to RGB.
  
_[function]_  
**`rotate-hsv`** _`a rotation` => hsv-vector_  
Rotates the hue of a HSV color.  The rotation is expressed in degrees,
rather than the 0.0-1.0 range used by the rest of the library.
Creating palettes of complimentary and contrasting colors is often
thought of in terms of rotating a color wheel, and I've used degrees
here since that's more natural for me, at least, to think about. 

_[function]_
**`rotate-rgb`** _`a rotation` => rgb-vector_  
This convenience function is equivalent to: 

    (hsv->rgb (rotate-hsv (rgb->hsv a) rotation))

_[function]_  
**`xmlify-rgb`** _`a &optional (stream nil)` => x/html-color-string_  
Turns an RGB value into a string of the form `#FFAADD`.  Optionally
you can specify a stream designator of the sort `format` expects.  By
default it returns the string. 

_[function]_  
**`parse`** _s => rgb-vector_
This converts an HTML color string into an RGB value.
