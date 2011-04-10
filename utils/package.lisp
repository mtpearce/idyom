;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-             
;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2003-04-05 18:54:17 marcusp>                           
;;;; Time-stamp: <2011-04-10 11:13:34 marcusp>                           
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage #:utils
  (:use #:cl)
  (:export "ROUND-TO-NEAREST-DECIMAL-PLACE" "AVERAGE" "GENERATE-INTEGERS" "POWERSET"
           "NTH-ROOT" "STRING-APPEND" "SPLIT-STRING" 
           "INSERTION-SORT" "CARTESIAN-PRODUCT" "FLATTEN" "COMBINATIONS"
           "FIND-DUPLICATES" 
           "ALIST->HASH-TABLE" "HASH-TABLE->ALIST" "HASH-TABLE->SORTED-ALIST"
           "READ-OBJECT-FROM-FILE" "FILE-EXISTS" "WRITE-OBJECT-TO-FILE"
           "CD" "PWD" 
           "COLLECT-GARBAGE" "SHELL-COMMAND")
  (:documentation "Utility functions of general use."))


