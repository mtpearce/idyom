# IDyOM: Information Dynamics of Music

Version 1.3

Copyright 2001-2015, Marcus Pearce [marcus.pearce@qmul.ac.uk](mailto:marcus.pearce@qmul.ac.uk)

This program is distributed under the terms of the GNU General Public
License.  See COPYING for details.

## Documentation

Documentation on how to install and use the system can be found on the
IDyOM wiki at Sound Software:

[https://code.soundsoftware.ac.uk/projects/idyom-project/wiki](https://code.soundsoftware.ac.uk/projects/idyom-project/wiki)

The design of the system and its underlying principles are documented
in Marcus Pearce's PhD thesis "The Construction and Evaluation of
Statistical Models of Melodic Structure in Music Perception and
Composition" (2005) included here as 'thesis.pdf'. Please cite this 
thesis in any publications using this software.

## Requirements

* SBCL [http://www.sbcl.org](http://www.sbcl.org)
* Emacs [http://www.gnu.org/software/emacs/](http://www.gnu.org/software/emacs/)
* Quicklisp [http://www.quicklisp.org/](http://www.quicklisp.org/)
* Sqlite [http://www.sqlite.org/](http://www.sqlite.org/)

See the [wiki](https://code.soundsoftware.ac.uk/projects/idyom-project/wiki) for further details on installation.

## News

### 2015-03-25

v1.3: new features and enhancements include:

* package mtp-admin renamed to idyom-db
* clearer printing of information during viewpoint selection
* add min-links to allow specifying a minimum number of constituent viewpoints in a link
* improvements to handling of voices/spines in Kern import
* ensure that phrase boundaries are correct before tied notes in Kern import
* fix last-element and penultimate-element for empty sequences
* add utility functions for permutations and rotation

#### 2014-12-31

v1.2: small enhancements including:

* remove verbose output during data import
* linked viewpoint creation is now agnostic to order of constituent viewpoints
* user may omit trailing forward slash when specifying directories

### 2014-09-27:

v1.1: new features include:

* an extended and improved representation of music objects
* a time protocol for music objects
* extraction of melodies from polyphonic pieces using skyline algorithm
* some generalisation of features in the generation code
* greater efficiency in accessing music-objects from the database
* allow specialisation of viewpoints on user-defined classes
* a caching parameter to control whether LTM resampling sets are cached
* some fixes to Lilypond export (thanks to J Forth)
* installation directories are created if need be (thanks to J Forth)

### 2014-06-04:

v1.0: first public release, I am grateful to Jeremy Gow and 
David Lewis for their contributions to this release, and also to 
Niels Chr. Hansen and Roger Dean for user feedback.
