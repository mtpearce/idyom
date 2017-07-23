;;;; ======================================================================
;;;; File:       descriptives.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-07-23 12:30:38 peter>                          
;;;; Time-stamp: <2017-07-23 12:42:09 peter>                           
;;;; ======================================================================
;;;;
;;;; DESCRIPTION 
;;;;
;;;;   Utility functions for computing descriptive statistics for a
;;;;   given musical dataset.
;;;;
;;;; ======================================================================

(cl:in-package #:descriptives)

(defgeneric count-n-grams (data n &key output-csv overwrite-csv)
  (:documentation "Counts <n>-grams in <data>. If <output-csv> is provided
then the output is saved as a csv file to path <output-csv>, as long as 
either no object exists at that location or <overwrite-csv> is not null.
The n-grams are returned as an EQL hash table where the keys are lists, each
list being a list of viewpoint elements corresponding to an n-gram, and the 
values are integer counts."))
  
