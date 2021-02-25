Viewpoints
========== 

Defining viewpoints
-------------------

Viewpoints are defined using the `DEFINE-VIEWPOINT` macro. 

The STATIC-ALPHABET flag indicates that the alphabet function is context free. That is, it can be set prior to modeling the dataset. This is an optimization that avoids having to call the alphabet function before predicting each event.

There are three optional keyword arguments to this macro:

:function

This one is important. The argument to this keyword will be used as the viewpoint function. The viewpoint function will receive a sequence of events of type EVENT-CLASS which is bound to the EVENTS symbol. When no viewpoint function is defined, the viewpoint will return NIL for all event sequences.

:function*

With function* an optional *inverse viewpoint function* can be defined which takes a sequence of events and a viewpoint element as arguments. The events are bound to the EVENTS symbol. Note that this will include the event currently predicted. This allows conditioning the inverse viewpoint function on the current event which would constitute cheating in the mulitple viewpoint systems formalism but is still useful in some cases. For example when it is necessary to know the value of BARLENGTH of the current event. Providing an inverse viewpoint function is an optimization. The inverse viewpoint function can be derived online but this is a costly procedure.

:alphabet

An optional function may be provided that returns the alphabet of the viewpoint given a sequence of events (again including the currently predicted event which should be ignored in most cases). Like the inverse viewpoint function, providing the alphabet function is an optimization. The alphabet of a derived viewpoint can be determined online but this is a costly procedure. Expecially in 

Built-in viewpoints
-------------------

Basic viewpoints
^^^^^^^^^^^^^^^^

Start time
""""""""""

The onset time of an event.

Duration
""""""""

Duration of an event.

Key signature
"""""""""""""

An integer indicating the position of the key signature on a line of fifths with C major = 0. Corresponds to a positive count of the number of sharps or a negative count of the number of flats (as long as the range is limited to +/-7, avoiding double accidentals).

Mode
""""

Mode reflects the prevailing mode using a general representation that is intended to cover the church modes as well as major/minor. 0 = major; 9 = minor reflecting the fact that the minor mode corresponds to rotation of the pitch class set corresponding to its relative major scale by 9 semitones (see Balzano, 1982). This allows a crude representation of the simpler common modes: Dorian is 2, Lydian 5, etc.

Tempo
"""""

The current tempo in beats per minute.

Pulses
""""""

The numerator of the time signature.

Bar length
""""""""""

Number of basic time units (expressed in `MD_TIMEBASE` units) per bar.
(define-basic-viewpoint barlength ((events md:music-sequence))

Chromatic pitch
"""""""""""""""

Pitch as a midi note number

Morphetic pitch
"""""""""""""""

David Meredith's morphetic pitch: count of name-notes (white notes) up or down from middle C = 35.

Accidental
""""""""""

Inflection of note name, so 0 for a natural, 1 for a single sharp, 2 for a double sharp, -1 for a flat and so on.

Delta-start
"""""""""""

Gap between last note and its predecessor (returns 0 for first note).  
  
Basic inter-onset interval
""""""""""""""""""""""""""
Inter onset interval between ultimate and penultimate onsets (returns interval from 0 to onset for first note).

Phrase
""""""

1 if event begins a phrase, -1 if it ends a phrase. Otherwise 0.

Dynamics
""""""""

======= ===========
Element Explanation
======= ===========
-11     ppppp
-9      pppp
-7      ppp
-5      pp
-3      p
-1      mp
1       mf
3       f
5       ff
7       fff 
9       ffff
11      fffff
======= ===========

Note ornaments
""""""""""""""

0 = no ornament; 1 = accacciatura; 2 = mordent; 3 = trill.

======= ===========
Element Explanation
======= ===========
0       No ornament
1       Accacciatura
2       Mordent
3       Trill
======= ===========

Voice
"""""

Voice number in a score.

Comma
"""""

Presence of breath mark.

======= ===========
Element Explanation
======= ===========
0       No comma
1       Comma
======= ===========

Articulation
""""""""""""

======= ===========
Element Explanation
======= ===========
0       No articulation mark
1       Staccato
2       Staccatissimo
3       Sforzando
4       Marcato
======= ===========

Vertical interval
"""""""""""""""""

David Sear's vertical interval.

Derived viewpoints
^^^^^^^^^^^^^^^^^^

Derived viewpoints have five parameters: their typeset, the look-back function, the alphabet function and the viewpoint itself.

Inter-onset interval
""""""""""""""""""""

Typeset: (`ONSET`)

Alphabet: (let (( 

Static alphabet: no

Look-back: 1

Function: 

    (multiple-value-bind (e1 e2)
        (values-list (last events 2))
      (let ((onset1 (onset (list e1)))
            (onset2 (onset (list e2))))
        (- onset2 onset1)))
