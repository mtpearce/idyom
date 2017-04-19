;;;; ======================================================================
;;;; File:       kern2db-tests.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-04-19 13:09:09 peter>                            
;;;; Time-stamp: <2017-04-19 22:40:57 peter>                           
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; This code defines tests for the kern2db package.

(cl:in-package #:kern2db)

;;;=====================
;;;* Files and folders *
;;;=====================

;; Files and folders
(defparameter *temp-dir* (merge-pathnames
			  (make-pathname :directory
					 '(:relative "temp" "tests"))
			  cl-user::*idyom-root*))
(ensure-directories-exist *temp-dir*)

;;;=====================
;;;* Utility functions *
;;;=====================

(defun write-temp-kern-file (id text)
  (let ((path (get-temp-kern-file-path id)))
    (with-open-file (stream path
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream text))))

(defun get-temp-kern-file-path (id)
  "Gets the file path for a temporary kern file with id <id>.
<id> should be an object that can be coerced to a string. 
The resulting file path will be located within *temp-dir*
and will have a kern extension."
  (let* ((name (format nil "~A" id))
	 (path (merge-pathnames
		(make-pathname :name name :type "krn")
		*temp-dir*)))
    path))

(defun delete-temp-kern-files ()
  "Deletes all temporary kern files."
  (let ((files (utils:recursively-list-files
		*temp-dir* :extensions '("krn"))))
    (dolist (file files)
      (delete-file file))))

(defun get-attribute-sequence (attribute events)
  "Returns a list comprising the values that <attribute>
takes in <events>."
  (assert (symbolp attribute))
  (assert (listp events))
  (mapcar #'(lambda (event) (cadr (assoc attribute event)))
	  events))

(defun simulate-import (input id)
  "Simulates the import of a kern file with text <input>, 
returning the result of the function convert-kern-file
when applied to that kern file. <id> is used to determine
the file name for the temporary kern file."
  (assert (stringp input))
  (write-temp-kern-file id input)
  (convert-kern-file (get-temp-kern-file-path id)))

(defun make-kern-test (test-prefix input &rest outputs)
  "Makes a 5am test with name prefix <test-prefix> (string), which tests
whether the output of importing the kern file located at path <input>
corresponds to <outputs>. Each list provided in <outputs>
should correspond to a list where the first element is 
a symbol corresponding to the event attribute of interest,
e.g. onset, and the remaining elements correspond to the 
list of values that that event attribute takes in the
output sequence."
  (assert (stringp test-prefix))
  (assert (stringp input))
  (assert (not (null outputs)))
  (let ((num-outputs (length outputs)))
    (loop
       for output in outputs
       for n from 1
       do (eval
	   (let* ((attribute (car output))
		  (values (cdr output))
		  (test-name (if (> num-outputs 1)
				 (format nil "~A-~A"
					 (string-upcase test-prefix)
					 (symbol-name attribute))
				 (string-upcase test-prefix))))
	     (if (member (intern test-name) (5am:test-names))
		 (utils:message
		  (format nil "Warning: redefining 5am test ~A."
			  test-name)))
	     `(5am:test ,(intern test-name)
		(5am:is
		 (equalp
		  (progn
		    (write-temp-kern-file ,test-name ,input)
		    (get-attribute-sequence
		     ',attribute
		     (cdr (assoc :events
				 (convert-kern-file
				  (get-temp-kern-file-path
				   ,test-name))))))
		  ',values))))))))

;;;=========
;;;* Tests *
;;;=========

;;;; Manual tests can be done using the simulate-import function
;;;; e.g.

(simulate-import
 "
**kern
*k[f#]
*G:
*M2/4
*MM84
=1
2d
=2
2f#
=3
8E'
8D'
8C'
16r
16B
"
 "simple-import")


;;;; Automatic tests can be done with the 5am package, as follows:

(5am:def-suite kern2db)
(5am:in-suite kern2db)

;; Simple example test with macro
(make-kern-test "simple"  "
**kern
*k[f#]
*G:
*M2/4
*MM84
=1-
2d
=2
2f#
=3
8E'
8D'
8C'
16r
16B
"
		'(:onset 0 48 96 108 120 138)
		'(:cpitch 62 66 52 50 48 59))

;; Testing upbeats
(make-kern-test "upbeat" "
**kern
*k[f#]
*G:
*M2/4
*MM84
8d
8d
=1-
2d
=2
2f#"
		'(:onset 24 36 48 96)
		'(:bar 0 0 1 2)
		'(:posinbar 24 36 0 0))

;; bar (current bar number)
(make-kern-test "bar" "
**kern
*k[f#]
*G:
*M2/4
*MM84
4d
=1-
2d
=2
2f#
=3
4g#
4g#"
		'(:bar 0 1 2 3 3))

(make-kern-test "bar-4-part" "
**kern	**kern	**kern	**kern
*M4/4	*M4/4	*M4/4	*M4/4
*MM100	*MM100	*MM100	*MM100
8cL	4e	4g	4cc
8BJ	.	.	.
=1	=1	=1	=1
4A	4e	4a	4cc
4E	8eL	4g	4b
.	8dJ	.	.
4F	4c	8gL	4a
.	.	8fJ	.
8CL	4c	8eL	4g
8DJ	.	8fJ	.
=2	=2	=2	=2
4E	4.c	4g	4cc
8FL	.	8aL	4dd
8GJ	8B	8gJ	.
4C;	4c;	4g;	4ee;
4C	8cL	4g	[4ee
.	8dJ	.	.
=3	=3	=3	=3
4c	8eL	8gL	8eeL]
.	8fJ	8aJ	8ddJ
4G#	8eL	4b	4ee
.	8dJ	.	.
8AL	4.c	4.a	4ee
8GJ	.	.	.
8FL	.	.	4dd
8GJ	8B	8g	.
=4	=4	=4	=4
"
		(append (list :posinbar)
			(make-list 4 :initial-element 72)
			(make-list 1 :initial-element 84)
			(make-list 4 :initial-element 0)
			(make-list 4 :initial-element 24)
			(make-list 1 :initial-element 36)
			(make-list 4 :initial-element 48)
			(make-list 1 :initial-element 60)
			(make-list 4 :initial-element 72)
			(make-list 2 :initial-element 84)
			(make-list 4 :initial-element 0)
			(make-list 3 :initial-element 24)
			(make-list 3 :initial-element 36)
			(make-list 4 :initial-element 48)
			(make-list 4 :initial-element 72)
			(make-list 1 :initial-element 84)
			(make-list 3 :initial-element 0)
			(make-list 3 :initial-element 12)
			(make-list 4 :initial-element 24)
			(make-list 1 :initial-element 36)
			(make-list 4 :initial-element 48)
			(make-list 1 :initial-element 60)
			(make-list 2 :initial-element 72)
			(make-list 3 :initial-element 84))
		(append (list :bar)
			(make-list 5 :initial-element 0)
			(make-list 20 :initial-element 1)
			(make-list 19 :initial-element 2)
			(make-list 21 :initial-element 3)))


;;;============
;;;* Clean up *
;;;============

(delete-temp-kern-files)
