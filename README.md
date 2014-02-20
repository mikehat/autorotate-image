autorotate-image
================

A simple tool to fix images. Usually, scanned images are

    * slightly (or quite) rotated
    * have a background around the page or photo

The autorotate command line tool

    * trims away a background color
    * looks for straight edges at the right, left, top and bottom
    * rotates and trims the image
    * saves it to a file, converting the format as necessary

'autorotate' is written in Haskell, but relies heavily on the ImageMagick
library. It is tested with ghc (7.6.3) and ImageMagick 6.8.6.3 on Fedora 20.

To build, use make. You'll need to
    * yum install ImageMagick ImageMagick-devel ImageMagick-libs and
    * cabal install imagemagick


Algorithm
=========

Rotation angle is determined by looking at the slopes of lines connecting all
combinations of edge points along each edge. The most frequent values are
averaged, resulting in a highly accurate angle. Since all four edges are detected,
even torn pages and folded corners get handled so long as there are some straight,
perpendicular lines along the perimeter.

The algorithm is not especially efficient, and takes as long as the rotation and
trimming operations.

Usage
=====

$ autorotate in-file.png out-file.png

There are some command line switches. Read autorotate.hs or try

$ autorotate --help

