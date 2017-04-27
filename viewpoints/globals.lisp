;;;; ======================================================================
;;;; File:       globals.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-04-27 12:36:01 peter>                           
;;;; Time-stamp: <2017-04-27 17:04:14 peter>                           
;;;; ======================================================================

;;;; This file contains global variables for the viewpoints package.

(cl:in-package #:viewpoints)

(defconstant +undefined+ '@ "The undefined symbol.")
(defvar *basic-types* nil) ;;(make-hash-table))

(defparameter *viewpoint-quantiles* nil)


