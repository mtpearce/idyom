;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-05-15 13:33:20 peter>                          
;;;; Time-stamp: <2018-01-11 18:00:49 peter>                           
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
  (:export "GENERATE-STIMULI")
  (:documentation "Utility functions for Peter Harrison's PhD study 3"))

;;; ======================================================================
;;; From Learning to Creativity (Ioanna Zioga collaboration)
;;; ======================================================================

(defpackage #:pmch-l2c
  (:use #:common-lisp)
  (:documentation "Utility functions for the Learning To Creativity project with Ioanna Zioga."))

;;; ======================================================================
;;; PMCH utils
;;; ======================================================================

(defpackage #:pmch-utils
  (:use #:common-lisp)
  (:export "EXPORT-HARMONY-CORPORA")
  (:documentation "Misc. Peter's utility functions"))
