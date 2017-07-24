;;;; ======================================================================
;;;; File:       descriptives.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-07-23 12:30:38 peter>                          
;;;; Time-stamp: <2017-07-24 17:04:00 peter>                           
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
    (format stream "<COUNT-TABLE (UNIQUE OBJECT COUNT = ~A)>" num-objects)
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
    (data n (viewpoint list))
  (count-viewpoint-n-grams data n (viewpoints:get-viewpoint viewpoint)))

(defmethod count-viewpoint-n-grams
    ((data md:music-sequence) n (viewpoint viewpoints:viewpoint))
  (count-n-grams (viewpoints:viewpoint-sequence viewpoint data) n))

(defmethod count-viewpoint-n-grams
    ((data list) n (viewpoint viewpoints:viewpoint))
  (reduce #'combine
	  (mapcar #'(lambda (composition)
		      (count-n-grams (viewpoints:viewpoint-sequence viewpoint
								    composition)
				     n))
		  data)))

;;;; Converting n-grams to transition probabilities

(defclass transition-probabilities ()
  ((data :accessor data :initarg :data
	:documentation "Object storing transition probabilities.
Transition probabilities are stored in the <data> slot. This slot 
should be occupied by a list each element of which corresponds
to a unique transition. These elements should themselves be lists,
the first element of which gives the context, the second giving 
the continuation, the third giving the context count, the fourth giving 
the continuation count, and the fifth giving the resulting MLE probability.")))

(defgeneric write-csv (object path))
(defmethod write-csv ((object transition-probabilities) path)
  (let* ((data (sort (copy-list (data object))
		     #'string<
		     :key #'(lambda (x) (princ-to-string (second x)))))
	 (data (sort data
		     #'string<
		     :key #'(lambda (x) (princ-to-string (first x)))))
	 (contexts (loop for x in data collect (first x)))
	 (continuations (loop for x in data collect (second x)))
	 (context-counts (loop for x in data collect (third x)))
	 (continuation-counts (loop for x in data collect (fourth x)))
	 (probabilities (loop for x in data collect (fifth x)))
	 (data (loop
		  for context in contexts
		  for continuation in continuations
		  for context-count in context-counts
		  for continuation-count in continuation-counts
		  for probability in probabilities
		  collect (list context continuation
				context-count continuation-count
				probability)))
	 (output (cons (list "context" "continuation"
			     "context_count" "continuation_count"
			     "probability")
		       data)))
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (cl-csv:write-csv output :stream stream))))

(defmethod print-object ((object transition-probabilities) stream)
  (let* ((data (data object))
	 (num-transitions (length data)))
    (format stream "<TRANSITION PROBABILITIES (COUNT = ~A)>" num-transitions)
    (when (> num-transitions 0)
      (let* ((contexts (loop for x in (data object) collect (first x)))
	     (continuations (loop for x in (data object) collect (second x)))
	     (probabilities (loop for x in (data object) collect (third x))))
	(flet ((max-string-width (string-list)
		 (apply #'max (mapcar #'(lambda (x) (length (princ-to-string x))) string-list))))
	  (let* ((context-col-width (max 10 (max-string-width contexts)))
		 (continuation-col-width (max 15 (max-string-width continuations)))
		 (probability-col-width 9)
		 (total-width (+ context-col-width continuation-col-width
				 probability-col-width)))
	    (flet ((print-separator ()
		       (format stream "~%~A"
			       (make-sequence 'string total-width :initial-element #\-)))
		   (print-header ()
		     (format stream "~%~10A~15A~15A" "Context" "Continuation" "Probability"))
		   (print-data ()
		     (loop
			for context in contexts
			for continuation in continuations
			for probability in probabilities
			do (format stream
				   "~%~vA~vA~v$"
				   context-col-width context
				   continuation-col-width continuation
				   probability-col-width probability))))
	      (print-separator)
	      (print-header)
	      (print-data))))))))
	       
(defgeneric n-grams->transition-probabilities (n-grams)
  (:documentation "Converts n-grams to transition probabilities using
maximum-likelihood estimation (i.e. no escape probabilities)."))

(defmethod n-grams->transition-probabilities ((n-grams count-table))
  (let* ((context-counts
	  (loop with context-counts = (make-instance 'count-table)
	     for n-gram being each hash-key of (%data n-grams)
	     using (hash-value count)
	     do (add-count (butlast n-gram) count context-counts)
	     finally (return context-counts))))
    (make-instance
     'transition-probabilities
     :data (loop
	      for n-gram being each hash-key of (%data n-grams)
	      using (hash-value continuation-count)
	      collect (let* ((context (butlast n-gram))
			     (continuation (car (last n-gram)))
			     (context-count (get-count context context-counts))
			     (probability (coerce (/ continuation-count context-count)
						  'double-float)))
			(list context continuation
			      context-count continuation-count
			      probability))))))

(defun get-viewpoint-transition-probabilities (data n viewpoint)
  (n-grams->transition-probabilities (count-viewpoint-n-grams data (1+ n) viewpoint)))

