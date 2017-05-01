;;;; ======================================================================
;;;; File:       globals.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-04-27 12:36:01 peter>                           
;;;; Time-stamp: <2017-05-01 10:05:07 peter>                           
;;;; ======================================================================

;;;; This file contains global variables for the viewpoints package.

(cl:in-package #:viewpoints)

(defconstant +undefined+ '@ "The undefined symbol.")
(defvar *basic-types* nil) ;;(make-hash-table))

;; Initialised to nil, but later becomes a hash table containing
;; key strings corresponding to viewpoint names and value simple vectors
;; corresponding to quantiles for continuous viewpoints
(defparameter *viewpoint-quantiles* nil)

;; Boolean, determines whether continuous viewpoints are to be discretised
;; or not. Referred to by individual viewpoint functions. 
;; Typically will be set to nil when the viewpoint quantiles are being
;; calibrated, and set to t when quantiles have been calibrated.
(defparameter *discretise-viewpoints* nil)


