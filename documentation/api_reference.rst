API Reference
=============

Utils
-----

.. cl:package:: idyom-db

Database
--------

Database for storage and retrieval of music.

.. cl:package:: idyom-db

Generics
^^^^^^^^

Data import
^^^^^^^^^^^

Data export 
^^^^^^^^^^^

Music objects
-------------

Representation language for music objects.

.. cl:package:: music-objects

Extended sequence
^^^^^^^^^^^^^^^^^

Time
^^^^

Music objects
^^^^^^^^^^^^^

Viewpoints
----------

.. cl:package:: viewpoints

Classes
^^^^^^^

.. cl:type:: viewpoint

.. cl:type:: basic

.. cl:type:: derived

.. cl:type:: test

.. cl:type:: threaded

.. cl:type:: linked

Generics
^^^^^^^^

.. cl:generic:: viewpoint-sequences

.. cl:generic:: viewpoint-element

.. cl:generic:: viewpoint-alphabet

.. cl:generic:: viewpoint-typeset

.. cl:generic:: viewpoint-links

.. cl:generic:: basic-p

.. cl:generic:: viewpoint-name

.. cl:generic:: viewpoint-type

.. cl:generic:: viewpoint-equal

.. cl:generic:: in-typeset-p

.. cl:generic:: viewpoint-element-equal

.. cl:generic:: basic-element

.. cl:generic:: inverse-viewpoint-function-defined-p

Methods
^^^^^^^

.. cl:method:: viewpoint-sequences viewpoint common-lisp:t

.. cl:method:: viewpoint-sequence viewpoint music-data:music-composition

.. cl:method:: viewpoint-element viewpoint common-lisp:list

.. cl:method:: viewpoint-alphabet viewpoint 

.. cl:method:: viewpoint-typeset viewpoint

.. cl:method:: viewpoint-links viewpoint 

.. cl:method:: basic-p viewpoint

.. cl:method:: viewpoint-name viewpoint

.. cl:method:: viewpoint-type viewpoint

.. cl:method:: viewpoint-equal viewpoint viewpoint

.. cl:method:: in-typeset-p viewpoints::basic viewpoint

.. cl:method:: viewpoint-element-equal viewpoints::basic viewpoint common-lisp:t common-lisp:t

.. cl:method:: viewpoint-element-equal viewpoints::basic viewpoints::linked common-lisp:t common-lisp:t

.. cl:method:: basic-element viewpoints::basic viewpoints::basic common-lisp:t common-lisp:t
 
.. cl:method:: basic-element viewpoint viewpoints::basic common-lisp:t common-lisp:t

.. cl:method:: basic-element viewpoints::linked viewpoints::basic common-lisp:t common-lisp:t

.. cl:method:: inverse-viewpoint-function-defined-p viewpoint

.. cl:method:: inverse-viewpoint-function-defined-p viewpoints::linked

Extensions
^^^^^^^^^^

.. cl:method:: set-alphabet-from-context viewpoint common-lisp:t common-lisp:t 

.. cl:method:: set-alphabet-from-dataset viewpoint common-lisp:t

.. cl:method:: alphabet->events viewpoint music-data:music-composition

Functions
^^^^^^^^^

.. cl:variable:: +undefined+

.. cl:function:: register-basic-type 

.. cl:function:: get-basic-types 

.. cl:function:: undefined-p

.. cl:function:: get-viewpoints

.. cl:function:: get-viewpoint

.. cl:function:: attribute-equal

.. cl:function:: list-basic

.. cl:function:: list-derived

.. cl:function:: list-threaded

.. cl:function:: list-test

.. cl:function:: list-viewpoints
	  
.. cl:function:: predictors

.. cl:function:: predictable


PPM* Statistical Models
-----------------------

.. cl:package:: ppm

Multiple viewpoint systems
--------------------------

Prediction using multiple viewpoint systems (MVS).

.. cl:package:: mvs

Prediction sets
^^^^^^^^^^^^^^^

.. cl:package:: prediction-sets

Applications
------------


IDyOM
^^^^^

.. cl:package:: idyom

.. cl:function:: idyom

Resampling
^^^^^^^^^^

.. cl:package:: resampling

Viewpoint-selection
^^^^^^^^^^^^^^^^^^^

Segmentation
^^^^^^^^^^^^

Generation
^^^^^^^^^^

