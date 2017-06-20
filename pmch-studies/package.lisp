;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-05-15 13:33:20 peter>                          
;;;; Time-stamp: <2017-06-20 16:59:30 peter>                           
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

