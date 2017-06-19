;;;; ======================================================================
;;;; File:       tests.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-06-19 18:16:00 peter>                           
;;;; Time-stamp: <2017-06-19 18:26:14 peter>                           
;;;; ======================================================================

(cl:in-package #:interfaces)

(5am:def-suite interfaces)
(5am:in-suite interfaces)

;;;; call-r
(5am:def-suite call-r :in interfaces)
(5am:in-suite call-r)

(5am:test call-r-ex-1
  (5am:is (equal (coerce (car (call-r "c <- a + 1"
				      (cons "a" (vector 1 2 3 4 5))
				      "c"))
			 'list)
		 '(2 3 4 5 6))))

(5am:test call-r-ex-2
  (5am:is (equal (coerce (car (call-r "c <- a * 2"
				      (cons "a" (vector 1 2 3 4 5))
				      "c"))
			 'list)
		 '(2 4 6 8 10))))
