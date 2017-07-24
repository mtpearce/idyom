;;;; ======================================================================
;;;; File:       descriptives-tests.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-07-24 09:34:35 peter>                             
;;;; Time-stamp: <2017-07-24 10:00:59 peter>                           
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; This code defines tests for the descriptives package.

(cl:in-package #:descriptives)

(5am:def-suite descriptives)
(5am:in-suite descriptives)

(5am:def-suite count-table :in descriptives)
(5am:in-suite count-table)

(5am:test count-table-ex-1
  (5am:is (let ((table (make-instance 'count-table)))
	    (add-count 'cat 2 table)
	    (add-count 'dog 1 table)
	    (add-count 'dog 3 table)
	    (and (eql (get-count 'cat table) 2)
		 (eql (get-count 'dog table) 4)
		 (eql (get-count 'fish table) 0)))))

(5am:test count-table-ex-2
  (5am:is (let ((t1 (make-instance 'count-table))
		(t2 (make-instance 'count-table)))
	    (add-count 'cat 2 t1)
	    (add-count 'dog 3 t1)
	    (add-count 'cat 3 t2)
	    (add-count 'dog 1 t2)
	    (add-count 'fish 7 t2)
	    (let ((t3 (combine t1 t2)))
	      (and (eql (get-count 'cat t3) 5)
		 (eql (get-count 'dog t3) 4)
		 (eql (get-count 'fish t3) 7))))))
	    
