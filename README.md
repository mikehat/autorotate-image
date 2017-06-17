autorotate-image
================

A simple tool to fix images. Usually, scanned images are

* slightly (or quite) rotated
* have a background around the page or photo

The _autorotate_ command line tool

* trims away a background color
* looks for straight edges at the right, left, top and bottom
* rotates and trims the image
* saves it to a file, converting the format as necessary

_autorotate_ is written in Haskell, but relies heavily on the _ImageMagick_
library.

Tested on
* _Fedora_ 20 with  _ghc_ (7.6.3) and _ImageMagick_ 6.8.6.3
* _Ubuntu_ 16.04 with _ghc_ (7.10.3) and _ImageMagick_ 6.8.0.0

Installation
============

Prerequisites
-------------

Fedora:

    yum install ImageMagick ImageMagick-devel ImageMagick-libs

Ubuntu:

    sudo apt-get install imagemagick libmagic++-dev cabal-install

Build and install
-----------------

    cabal install

Usage
=====

    $ autorotate in-file.png out-file.png

There are some command line switches. Read `Main.hs` or try

    $ autorotate --help


Algorithm
=========

Rotation angle is determined by looking at the slopes of lines connecting all
combinations of edge points along each edge. The most frequent values are
averaged, resulting in a highly accurate angle. Since all four edges are detected,
even torn pages and folded corners get handled so long as there are some straight,
perpendicular lines along the perimeter.

The algorithm is not especially efficient, and takes as long as the rotation and
trimming operations.
