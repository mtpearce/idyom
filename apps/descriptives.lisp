;;;; ======================================================================
;;;; File:       descriptives.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-07-23 12:30:38 peter>                          
;;;; Time-stamp: <2017-07-24 10:02:46 peter>                           
;;;; ======================================================================
;;;;
;;;; DESCRIPTION 
;;;;
;;;;   Utility functions for computing descriptive statistics for a
;;;;   given musical dataset.
;;;;
;;;; ======================================================================

(cl:in-package #:descriptives)

;;;; Counting utilities

(defclass count-table ()
  ((data :accessor %data :initform (make-hash-table :test 'equal)
	 :documentation "Stores counts of objects with equality test #'equal.")))

(defgeneric add-count (object count count-table)
  (:documentation "Increments the counter for <object> in <count-table> by <count>,
adding a new entry for <object> if it does not exist in <count-table>.
<count-table> is destructively updated and returned."))

(defmethod add-count (object (count number) (count-table count-table))
  (let ((prev-count (gethash object (%data count-table))))
    (setf (gethash object (%data count-table))
	  (if (null prev-count)
	      count
	      (+ prev-count count)))
    count-table))

(defgeneric get-count (object count-table)
  (:documentation "Gets the count for <object> in <count-table>."))
(defmethod get-count (object (count-table count-table))
  (let ((entry (gethash object (%data count-table))))
    (if (null entry) 0 entry)))

(defgeneric combine (x y)
  (:documentation "Combines two objects <x> and <y>, possibly destructively."))

(defmethod combine ((x count-table) (y count-table))
  (loop for object being each hash-key of (%data y)
       using (hash-value added-count)
       do (add-count object added-count x)
       finally (return x)))

;;;; Counting n grams

(defgeneric count-n-grams (data n &key output-csv overwrite-csv)
  (:documentation "Counts <n>-grams in <data>. If <output-csv> is provided
then the output is saved as a csv file to path <output-csv>, as long as 
either no object exists at that location or <overwrite-csv> is not null.
The n-grams are returned as an EQL hash table where the keys are lists, each
list being a list of viewpoint elements corresponding to an n-gram, and the 
values are integer counts."))

