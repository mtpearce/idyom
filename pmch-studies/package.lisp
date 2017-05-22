;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-05-15 13:33:20 peter>                          
;;;; Time-stamp: <2017-05-22 13:40:41 peter>                           
;;;; ======================================================================

(cl:in-package #:cl-user)

;;; ======================================================================
;;; Study 1
;;; ======================================================================

(defpackage #:pmch-s1
  (:use #:common-lisp)
  (:export "ANALYSE-VIEWPOINT" "ANALYSE-VIEWPOINTS" "ANALYSE-ALL-VIEWPOINTS"
	   "*H-VP-1-OF-3*" "*H-VP-2-OF-3*" "*H-VP-3-OF-3*"
	   "*H-VP-1-OF-8*" "*H-VP-2-OF-8*" "*H-VP-3-OF-8*" "*H-VP-4-OF-8*"
	   "*H-VP-5-OF-8*" "*H-VP-6-OF-8*" "*H-VP-7-OF-8*" "*H-VP-8-OF-8*")
  (:documentation "Utility functions for Peter Harrison's study on harmony representations"))

