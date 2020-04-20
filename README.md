# IDyOM: Information Dynamics of Music

Version 1.6

Copyright 2001-2020, the IDyOM development team (see CREDITS).

This program is distributed under the terms of the GNU General Public
License.  See COPYING for details. 

## Documentation

Documentation on how to install and use the system can be found on the
IDyOM wiki at Sound Software:

https://code.soundsoftware.ac.uk/projects/idyom-project/wiki

The design of the system and its underlying principles are documented
in Marcus Pearce's PhD thesis "The Construction and Evaluation of
Statistical Models of Melodic Structure in Music Perception and
Composition" (2005) included here as 'thesis.pdf'. Please cite this 
thesis in any publications using this software.

## Requirements

* SBCL       http://www.sbcl.org/
* Emacs      http://www.gnu.org/software/emacs/
* Quicklisp  http://www.quicklisp.org/
* Sqlite     http://www.sqlite.org/

See the wiki for further details on installation.

## News

### v1.6

* PPM*: update excluded counts are now incremented at the highest-order matching state (reported by Peter Harrison)
* PPM*: with update exclusion enabled, if the highest-order matching state is deterministic and there are no lower-order deterministic states, the update excluded count is used, rather than the full count
* PPM*: virtual states are now allocated correctly when more than one exists on a string transition
* PPM*: new parameter for the top-level idyom function to enable/disable exclusion 
* Viewpoints: basic temporal viewpoints (onset, bioi) are now imported correctly from Kern files when there are rests in the first bar
* Viewpionts: the metaccent and posinbar viewpoints are undefined when the time signature is not specified
* Viewpoints: added threaded viewpoints for scale degree and used clearer naming conventions for threaded viewpoints
* Utils: added SD and Pearson correlation
* Data import: don't increment voice number for empty MIDI tracks
* Apps: new top-level function in the segmentation code: (segmentation:idyom-segmentation ...)
* Apps: new app for Krumhansl-Schmuckler key-finding (key-finding:find-keys ...)
* Apps: new control parameters for generation (generation:idyom-generation ...)
* Output: output information gain as the KL divergence between the predictive distribution before and after a note is processed
* Output: when predicting onset, label the distribution with ioi values rather than onset values

### 2018-07-12: v1.5

* New top-level option to specify the separator to use in output files (thanks to Peter M. C. Harrison)
* new output formatting code for greater efficiency and accuracy, fixing misidentified columns when predicting onset (thanks to Peter M. C. Harrison)
* fixed uneven splits in the creation of resampling sets (thanks to Peter M. C. Harrison)
* new top-level option to specify whether output files are written during viewpoint selection
* fix bug preventing viewpoint selection (reported by Niels Chr. Hansen and Ben Gold)
* viewpoint selection works for max-links = 1 and more than one target viewpoint (reported by Ben Gold)
* fix compilation issue with recent versions of SBCL (reported by Ben Gold, tested on SBCL 1.4.8)
* added timebase parameter in the music-objects code to allow for different timebases to be specified (thanks to Bastiaan van der Weij)
* more efficient implementation of CARTESIAN-PRODUCT (thanks to Bastiaan van der Weij) 
* new macro DEFINE-THREADED-VIEWPOINTS for flexibly creating threaded viewpoints
* new utility functions: CUMSUM, RANGE, MD5-SUM-OF-LISTS, COPY-SLOT-VALUES, INITIALISE-UNBOUND-SLOTS, SHUFFLE, ANY-DUPLICATED, RANDOM-SELECT
* midi track numbers are indexed from 1 rather than 0 (for consistency with Kern import)
* avoid negative deltast in midi import when successive notes overlap in a melody
* add random-subset argument to IDYOM-DB:COPY-DATASETS to allow copying random subsets of a specified size
* extended documentation on running IDyOM, especially with respect to viewpoint selection (thanks to Ben Gold)

### 2016-05-03: v1.4 

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

### 2015-03-25: v1.3

* package mtp-admin renamed to idyom-db
* clearer printing of information during viewpoint selection
* add min-links to allow specifying a minimum number of constituent viewpoints in a link
* improvements to handling of voices/spines in Kern import
* ensure that phrase boundaries are correct before tied notes in Kern import
* fix last-element and penultimate-element for empty sequences
* add utility functions for permutations and rotation

### 2014-12-31: v1.2

* remove verbose output during data import
* linked viewpoint creation is now agnostic to order of constituent viewpoints
* user may omit trailing forward slash when specifying directories

### 2014-09-27: v1.1

* an extended and improved representation of music objects
* a time protocol for music objects
* extraction of melodies from polyphonic pieces using skyline algorithm
* some generalisation of features in the generation code
* greater efficiency in accessing music-objects from the database
* allow specialisation of viewpoints on user-defined classes
* a caching parameter to control whether LTM resampling sets are cached
* some fixes to Lilypond export (thanks to J Forth)
* installation directories are created if need be (thanks to J Forth)

### 2014-06-04: v1.0

First public release. I am grateful to Jeremy Gow and David Lewis for
their contributions to this release, and also to Niels Chr. Hansen and
Roger Dean for user feedback.
