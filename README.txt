HYPEROPERATIONS PACKAGE VERSION 0.1

OVERVIEW
--------

Hyperoperations is a package that supports hyperoperations sequences in
Mathematica, including those of Bennett, Frappier, and Goodstein.
Tetration, the superlogarithm, and super-roots can be calculated for
most (but not all!) numerical values.

For instructions on using the package, please see HyperoperationsManual.pdf.

If you are curious about how the package works, feel free to read the source
code of the package, Hyperoperations.m.  The file s25 contains a critical
function of the superlogarithm; the manual has more information, as well as
instructions for generating other approximations.

If you find anything that needs correcting, from a typo to a bug to a serious
mathematical blunder, feel free to tell me by emailing abgroene AT mtu DOT edu.

INSTALLING
----------

To use Hyperoperations, download Hyperoperations.zip and follow these steps:

1. Unzip Hyperoperations.zip.

2. Place the files Hyperoperations.m and s25 in your Mathematica working
directory. You can find out what your working directory is with

 Directory[]

or set a new one with

 SetDirectory[new path]

3. Load the package with the command

 Needs["Hyperoperations`"]

It is also possible to use Get or its shorthand form << to load the package.
You may encounter a "shadow" error if you have already used or defined
symbols with the same names as symbols in the package. If you do, run

 Remove[symbol]

for each name that appears with an error message.

4. For brief help on a function, type

 ?function

For a list of all functions in the Hyperoperations package, type

 ?Hyperoperations`*