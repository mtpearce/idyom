;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-05 18:54:17 marcusp>                           
;;;; Time-stamp: <2017-05-09 18:41:11 peter>                           
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage #:utils
  (:use #:cl)
  (:export "ROUND-TO-NEAREST-DECIMAL-PLACE" "AVERAGE" "GENERATE-INTEGERS"
	   "POWERSET" "QUOTIENT" "FACTORIAL" "N-PERMUTATIONS" "N-COMBINATIONS"
	   "RANGE" "CUMSUM" "MD5-SUM-OF-LIST" "SHUFFLE" "GREATEST-COMMON-MULTIPLE"
           "NTH-ROOT" "STRING-APPEND" "SPLIT-STRING" "ANY-DUPLICATED"
           "INSERTION-SORT" "CARTESIAN-PRODUCT" "FLATTEN" "COMBINATIONS"
	   "FLATTEN-ORDER" "COUNT-FREQUENCIES" "NUMERIC-FREQUENCIES"
           "FIND-DUPLICATES" "ROTATE" "PERMUTATIONS" "REMOVE-BY-POSITION"
           "COPY-INSTANCE" "COPY-SLOT-VALUES" "INITIALISE-UNBOUND-SLOTS"
           "LAST-ELEMENT" "PENULTIMATE-ELEMENT" "LAST-N" "BUTLAST-N"
	   "SORT-SYMBOLS" "NMAPCAR" "NPOSITION" "NPOSITIONS" "NMEMBER" "NMIN"
	   "NSELECTFIRST" "ALIST->HASH-TABLE" "HASH-TABLE->ALIST"
	   "HASH-TABLE->SORTED-ALIST" "READ-OBJECT-FROM-FILE" "FILE-EXISTS"
	   "WRITE-OBJECT-TO-FILE" "CD" "PWD" "ENSURE-DIRECTORY" "MAKE-PLIST"
	   "SET-EQUAL" "COLLECT-GARBAGE" "SHELL-COMMAND")
  (:documentation "Utility functions of general use."))

