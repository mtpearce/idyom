;;;; ======================================================================
;;;; File:       tests.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-04-27 16:17:12 peter>                         
;;;; Time-stamp: <2017-05-01 10:31:15 peter>                           
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; This code defines tests for the utils package.

(cl:in-package #:utils)

(5am:def-suite utils)
(5am:in-suite utils)

;;;; quantiles
(5am:def-suite quantiles :in utils)
(5am:in-suite quantiles)

;; These tests can be reproduced in R by using the quantile
;; function with type = 4
(5am:test quantiles-ex-1
  (5am:is (equal (quantiles (loop for i from 1 to 9 collect i) 5)
		 '(1.8 3.6 5.4 7.2))))
(5am:test quantiles-ex-2
  (5am:is (equal (quantiles (list 1.3 2.75 3.2 0 54) 4)
		 '(0.325 2.025 3.0875))))
(5am:test quantiles-ex-3
  (5am:is (equal (quantiles (loop for i from 1 to 10 collect i) 5)
		 '(2 4 6 8))))
(5am:test quantiles-ex-4
  (5am:is (equal (quantiles (list 1 5 4 3 2 7 9 8 10 6) 5)
		 '(2 4 6 8))))

;;;; assign-to-quantile
(5am:def-suite assign-to-quantile :in utils)
(5am:in-suite assign-to-quantile)

(5am:test assign-to-quantile-ex-1
  (5am:is (eql (assign-to-quantile 1.0 '(1.6 3.3)) 1)))
(5am:test assign-to-quantile-ex-2
  (5am:is (eql (assign-to-quantile 2 '(1.6 3.3)) 2)))
(5am:test assign-to-quantile-ex-3
  (5am:is (eql (assign-to-quantile 3.5 '(1.6 3.3)) 3)))

(5am:test assign-to-quantile-ex-4
  (5am:is (eql (assign-to-quantile -100 '(1 2 3 4 5)) 1)))
(5am:test assign-to-quantile-ex-5
  (5am:is (eql (assign-to-quantile 1.5 '(1 2 3 4 5)) 2)))
(5am:test assign-to-quantile-ex-6
  (5am:is (eql (assign-to-quantile 500 '(1 2 3 4 5)) 6)))
