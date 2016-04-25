;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-05 18:54:17 marcusp>                           
;;;; Time-stamp: <2016-04-25 11:07:18 marcusp>                           
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage #:utils
  (:use #:cl)
  (:export "ROUND-TO-NEAREST-DECIMAL-PLACE" "AVERAGE" "GENERATE-INTEGERS"
	   "POWERSET" "QUOTIENT" "FACTORIAL" "N-PERMUTATIONS" "N-COMBINATIONS"
           "NTH-ROOT" "STRING-APPEND" "SPLIT-STRING" 
           "INSERTION-SORT" "CARTESIAN-PRODUCT" "FLATTEN" "COMBINATIONS"
	   "FLATTEN-ORDER" "COUNT-FREQUENCIES" "NUMERIC-FREQUENCIES"
           "FIND-DUPLICATES" "ROTATE" "PERMUTATIONS" "REMOVE-BY-POSITION"
           "COPY-INSTANCE"
           "LAST-ELEMENT" "PENULTIMATE-ELEMENT" "LAST-N" "BUTLAST-N"
	   "NMAPCAR" "NPOSITION" "NPOSITIONS" "NMEMBER" "NMIN" "NSELECTFIRST"
           "ALIST->HASH-TABLE" "HASH-TABLE->ALIST" "HASH-TABLE->SORTED-ALIST"
           "READ-OBJECT-FROM-FILE" "FILE-EXISTS" "WRITE-OBJECT-TO-FILE"
           "CD" "PWD" "ENSURE-DIRECTORY"
           "COLLECT-GARBAGE" "SHELL-COMMAND")
  (:documentation "Utility functions of general use."))


