;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-05-15 13:33:20 peter>                          
;;;; Time-stamp: <2017-07-26 19:18:27 peter>                           
;;;; ======================================================================

(cl:in-package #:cl-user)

;;; ======================================================================
;;; Study 1
;;; ======================================================================

(defpackage #:pmch-s1
  (:use #:common-lisp)
  (:export "ANALYSE-VIEWPOINT" "ANALYSE-VIEWPOINTS" "ANALYSE-ALL-VIEWPOINTS"
	   "RUN-STUDY-1")
  (:documentation "Utility functions for Peter Harrison's study on harmony representations"))

;;; ======================================================================
;;; Study 3
;;; ======================================================================

(defpackage #:pmch-s3
  (:use #:common-lisp)
  (:export)
  (:documentation "Utility functions for Peter Harrison's PhD study 3"))

