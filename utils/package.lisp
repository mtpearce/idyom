;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-05 18:54:17 marcusp>                           
;;;; Time-stamp: <2018-08-10 09:47:01 marcusp>                           
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage #:utils
  (:use #:cl)
  (:export "ROUND-TO-NEAREST-DECIMAL-PLACE" "AVERAGE" "GENERATE-INTEGERS"
	   "POWERSET" "QUOTIENT" "FACTORIAL" "N-PERMUTATIONS" "N-COMBINATIONS"
	   "RANGE" "CUMSUM" "MD5-SUM-OF-LIST" "SHUFFLE"
           "NTH-ROOT" "ANY-DUPLICATED"
           "INSERTION-SORT" "CARTESIAN-PRODUCT" "FLATTEN" "COMBINATIONS"
	   "FLATTEN-ORDER" "COUNT-FREQUENCIES" "NUMERIC-FREQUENCIES"
           "FIND-DUPLICATES" "ROTATE" "PERMUTATIONS" "REMOVE-BY-POSITION"
           "RANDOM-SELECT"
           "LIST->STRING" "STRING-APPEND" "SPLIT-STRING"
           "COPY-INSTANCE" "COPY-SLOT-VALUES" "INITIALISE-UNBOUND-SLOTS"
           "LAST-ELEMENT" "PENULTIMATE-ELEMENT" "LAST-N" "BUTLAST-N"
	   "NMAPCAR" "NPOSITION" "NPOSITIONS" "NMEMBER" "NMIN" "NSELECTFIRST"
           "ALIST->HASH-TABLE" "HASH-TABLE->ALIST" "HASH-TABLE->SORTED-ALIST"
           "READ-OBJECT-FROM-FILE" "FILE-EXISTS" "WRITE-OBJECT-TO-FILE"
           "CD" "PWD" "ENSURE-DIRECTORY"
           "COLLECT-GARBAGE" "SHELL-COMMAND")
  (:documentation "Utility functions of general use."))

(defpackage #:python
  (:use #:cl)
  (:export "ALIST->DICT" "PLIST->DICT" "LIST->LIST")
  (:documentation "Utility functions for exporting python code."))



