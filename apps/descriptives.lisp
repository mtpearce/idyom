;;;; ======================================================================
;;;; File:       descriptives.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-07-23 12:30:38 peter>                          
;;;; Time-stamp: <2017-07-24 12:22:24 peter>                           
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

(defmethod print-object ((object count-table) stream)
  (let* ((output (loop
		    for object being each hash-key of (%data object)
		    using (hash-value count)
		    collect (cons (princ-to-string object) count)))
	 (output (sort output #'string< :key #'car))
	 (num-objects (length output)))
    (format stream "<COUNT-TABLE (COUNT = ~A)>" num-objects)
    (dolist (x output)
      (format stream "~%~A - ~A" (car x) (cdr x)))))

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

(defgeneric count-n-grams (data n)
  (:documentation "Counts <n>-grams in <data>.
Counting is done using the #'equal predicate (or whatever is implemented 
in the count-table methods). Final n-gram counts are returned as a count-table
object."))

(defmethod count-n-grams ((data list) (n integer))
  (assert (> n 0))
  (labels ((recursive-count (remainder n running-count)
	     (if (< (length remainder) n)
		 running-count
		 (recursive-count (cdr remainder) n
				  (add-count (subseq remainder 0 n)
					     1 running-count)))))
    (recursive-count data n (make-instance 'count-table))))

;;;; Counting n-grams of viewpoint elements

(defgeneric count-viewpoint-n-grams
    (data n viewpoint)
  (:documentation "Counts <n>-grams of viewpoint-elements in <data>.
Counting is done using the #'equal predicate (or whatever is implemented 
in the count-table methods). Final n-gram counts are returned as a count-table
object."))

(defmethod count-viewpoint-n-grams
    (data n (viewpoint symbol))
  (count-viewpoint-n-grams data n (viewpoints:get-viewpoint viewpoint)))

(defmethod count-viewpoint-n-grams
    ((data md:music-sequence) n (viewpoint viewpoint))
  (count-n-grams (viewpoint-sequence viewpoint data) n))

(defmethod count-viewpoint-n-grams
    ((data list) n (viewpoint viewpoint))
  (reduce #'combine
	  (mapcar #'(lambda (composition)
		      (count-n-grams (viewpoint-sequence viewpoint
							 composition)
				     n))
		  data)))


