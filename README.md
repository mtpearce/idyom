# IDyOM: Information Dynamics of Music

Version 1.4

Copyright 2001-2016, Marcus Pearce [marcus.pearce@qmul.ac.uk](mailto:marcus.pearce@qmul.ac.uk)

This program is distributed under the terms of the GNU General Public License.  See COPYING for details.

## Documentation

Documentation on how to install and use the system can be found on the IDyOM wiki at Sound Software:

[https://code.soundsoftware.ac.uk/projects/idyom-project/wiki](https://code.soundsoftware.ac.uk/projects/idyom-project/wiki)

The design of the system and its underlying principles are documented in Marcus Pearce's PhD thesis "The Construction and Evaluation of Statistical Models of Melodic Structure in Music Perception and Composition" (2005) included here as 'thesis.pdf'. Please cite this thesis in any publications using this software.

## Requirements

* SBCL       [http://www.sbcl.org](http://www.sbcl.org)
* Emacs      [http://www.gnu.org/software/emacs](http://www.gnu.org/software/emacs)
* Quicklisp  [http://www.quicklisp.org](http://www.quicklisp.org)
* Sqlite     [http://www.sqlite.org](http://www.sqlite.org)

See the [wiki](https://code.soundsoftware.ac.uk/projects/idyom-project/wiki) for further details on installation.

## News

### 2016-05-03

v1.4

* new feature: new function IDYOM-DB:COPY-DATASETS for copying and merging datasets
* new feature: new function UTILS:REMOVE-BY-POSITION in utilities
* new feature: new module for information-theoretic detection of grouping boundaries (see Pearce et al., 2010, Perception, 39, 1367-1391).
* new feature: new top-level variable in IDYOM:IDYOM to control whether existing output-files are overwritten (:overwrite nil)
* enhancement: greater randomness in the generation of resampling sets (the random state is now reset in the call to random)
* enhancement: allow the user to pass a random-state as a top-level argument to dataset-generation
* enhancement: add model configuration (STM, LTM, LTM+, BOTH or BOTH+) as a top-level argument in the generation module
* enhancement: the root and earth nodes in the PPM* module are no longer global variables
* enhancement: add a variable to control whether MIDI pitch bends affect pitch in MIDI import module (*apply-pitchbend*)
* enhancement: ignore empty compositions when importing data
* optimization: improved efficiency in generating the order-1 distribution in the PPM module (thanks to T Hedges)
* optimization: improved efficiency in generating viewpoint sequences
* optimization: generate transition counts more efficiently in the PPM module
* bug-fix: greater elegance in the viewpoint generation macros

### 2015-03-25

v1.3: new features and enhancements include:

* package mtp-admin renamed to idyom-db
* clearer printing of information during viewpoint selection
* add min-links to allow specifying a minimum number of constituent viewpoints in a link
* improvements to handling of voices/spines in Kern import
* ensure that phrase boundaries are correct before tied notes in Kern import
* fix last-element and penultimate-element for empty sequences
* add utility functions for permutations and rotation

### 2014-12-31

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

v1.0: first public release, I am grateful to Jeremy Gow and David Lewis 
for their contributions to this release, and also to 
Niels Chr. Hansen and Roger Dean for user feedback.
