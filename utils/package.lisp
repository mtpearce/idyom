;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-05 18:54:17 marcusp>                           
;;;; Time-stamp: <2022-06-22 20:21:01 marcusp>                           
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage #:utils
  (:use #:cl)
  (:export "ASK-USER-Y-N-QUESTION" "MESSAGE"
           "INITIALISE-PROGRESS-BAR" "UPDATE-PROGRESS-BAR"
           "DOLIST-PB" "DOTIMES-PB" "UPDATE-ALIST"
           "ROUND-TO-NEAREST-DECIMAL-PLACE" "APPROX-EQUAL"
           "AVERAGE" "GENERATE-INTEGERS" "SD" "COR"
	   "POWERSET" "QUOTIENT" "FACTORIAL" "N-PERMUTATIONS" "N-COMBINATIONS"
           "SAMPLE" "PARSE-NUMBER" "SUBTRACT-MOD-N"
           "SHUFFLE" "NTH-ROOT" "STRING-APPEND" "SPLIT-STRING" "ANY-DUPLICATED"
	   "RANGE" "CUMSUM" "MD5-SUM-OF-LIST" "SHUFFLE" 
           "NTH-ROOT" "ANY-DUPLICATED"
           "INSERTION-SORT" "CARTESIAN-PRODUCT" "FLATTEN" "COMBINATIONS"
	   "FLATTEN-ORDER" "COUNT-FREQUENCIES" "NUMERIC-FREQUENCIES"
           "FIND-DUPLICATES" "ROTATE" "PERMUTATIONS" "REMOVE-BY-POSITION"
           "RANDOM-SELECT"
           "LIST->STRING" "STRING-APPEND" "SPLIT-STRING"
           "COPY-INSTANCE"  "INSERT-AFTER" "ALL-EQL" "ALL-POSITIONS-IF"
           "REMOVE-NTH" "CSV->HASH-TABLE"
           "COPY-SLOT-VALUES" "INITIALISE-UNBOUND-SLOTS"
           "LAST-ELEMENT" "PENULTIMATE-ELEMENT" "LAST-N" "BUTLAST-N"
           "QUANTILES" "ASSIGN-TO-QUANTILE"
	   "NMAPCAR" "NPOSITION" "NPOSITIONS" "NMEMBER" "NMIN" "NSELECTFIRST"
           "ALIST->HASH-TABLE" "HASH-TABLE->ALIST" "HASH-TABLE->SORTED-ALIST"
           "READ-OBJECT-FROM-FILE" "FILE-EXISTS" "WRITE-OBJECT-TO-FILE"
           "CD" "PWD" "ENSURE-DIRECTORY" "COPY-FILE" "RECURSIVELY-LIST-FILES"
           "COLLECT-GARBAGE" "SHELL-COMMAND"
           "DATAFRAME" "ADD-ROW" "BIND-BY-ROW" "REMOVE-COLUMNS-EXCEPT"
           "GET-COLUMN" "SORT-BY-COLUMNS" "PRINT-DATA"
           "AS-DATAFRAME" "WRITE-CSV"
           "DATA" "NUM-ROWS")
  (:documentation "Utility functions of general use."))

(defpackage #:python
  (:use #:cl)
  (:export "ALIST->DICT" "PLIST->DICT" "LIST->LIST")
  (:documentation "Utility functions for exporting python code."))



