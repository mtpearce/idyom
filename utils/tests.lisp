;;;; ======================================================================
;;;; File:       tests.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-04-27 16:17:12 peter>                         
;;;; Time-stamp: <2017-10-11 09:25:42 peter>                           
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
		 '(2.0 4.0 6.0 8.0))))
(5am:test quantiles-ex-4
  (5am:is (equal (quantiles (list 1 5 4 3 2 7 9 8 10 6) 5)
		 '(2.0 4.0 6.0 8.0))))

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

;;;; Dataframes
(5am:def-suite dataframe :in utils)
(5am:in-suite dataframe)

(defun make-test-dataframe ()
  (let ((df (make-instance 'dataframe)))
    (add-row (alist->hash-table '((:day 1) (:hour 2) (:value 0.3))) df)
    (add-row (alist->hash-table '((:day 2) (:hour 2) (:value 0.7))) df)
    (add-row (alist->hash-table '((:day 3) (:hour 2) (:value 0.1))) df)
    (add-row (alist->hash-table '((:day 1) (:hour 1) (:value 0.4))) df)
    (add-row (alist->hash-table '((:day 2) (:hour 1) (:value 0.2))) df)
    (add-row (alist->hash-table '((:day 3) (:hour 1) (:value 0.0))) df)
    df))

(5am:def-suite sort-by-columns :in dataframe)
(5am:in-suite sort-by-columns)

(5am:test sort-by-columns-ex-1
  (5am:is (equal (get-column :day (sort-by-columns (make-test-dataframe) '(:day :hour)))
		 '(1 1 2 2 3 3))))
(5am:test sort-by-columns-ex-2
  (5am:is (equal (get-column :hour (sort-by-columns (make-test-dataframe) '(:day :hour)))
		 '(1 2 1 2 1 2))))
    
;;;; numeric

(5am:def-suite numeric :in utils)
(5am:in-suite numeric)

(5am:test approx-equal-ex-1
  (5am:is (approx-equal 1 1)))
(5am:test approx-equal-ex-2
  (5am:is (approx-equal 15.000001 15)))
(5am:test approx-equal-ex-3
  (5am:is (not (approx-equal 15.00001 15.0))))
