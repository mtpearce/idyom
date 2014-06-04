;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-05 18:54:17 marcusp>                           
;;;; Time-stamp: <2014-06-04 16:05:46 marcusp>                           
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage #:utils
  (:use #:cl)
  (:export "ROUND-TO-NEAREST-DECIMAL-PLACE" "AVERAGE" "GENERATE-INTEGERS"
	   "POWERSET" "QUOTIENT"
           "NTH-ROOT" "STRING-APPEND" "SPLIT-STRING" 
           "INSERTION-SORT" "CARTESIAN-PRODUCT" "FLATTEN" "COMBINATIONS"
	   "FLATTEN-ORDER" "COUNT-FREQUENCIES" "NUMERIC-FREQUENCIES"
           "FIND-DUPLICATES" "LAST-ELEMENT" "PENULTIMATE-ELEMENT"
	   "NMAPCAR" "NPOSITION" "NPOSITIONS" "NMEMBER" "NMIN" "NSELECTFIRST"
           "ALIST->HASH-TABLE" "HASH-TABLE->ALIST" "HASH-TABLE->SORTED-ALIST"
           "READ-OBJECT-FROM-FILE" "FILE-EXISTS" "WRITE-OBJECT-TO-FILE"
           "CD" "PWD" 
           "COLLECT-GARBAGE" "SHELL-COMMAND")
  (:documentation "Utility functions of general use."))


