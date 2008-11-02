;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-             
;;;; ======================================================================
;;;; File:       params.lisp
;;;; Author:     Marcus  Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2003-06-16 18:54:17 marcusp>                           
;;;; Time-stamp: <2008-06-19 09:54:37 marcusp>                           
;;;; ======================================================================

(cl:in-package #:mvs)

(defvar *ep-cache-dir* nil "Directory for storing cached predictions.")

(defparameter *ltm-mixtures* t)
(defparameter *ltm-escape* :c) ;:a, :b, :c, :d, :x
(defparameter *ltm-order-bound* nil)
(defparameter *ltm-update-exclusion* nil)

(defparameter *stm-mixtures* t)
(defparameter *stm-escape* :x) ;:a, :b, :c, :d, :x
(defparameter *stm-order-bound* nil)
(defparameter *stm-update-exclusion* t)

(defparameter *marginalise-using-current-event* 2
  "A list of basic viewpoints which assume their full alphabets in
prediction rather than being marginalised out based on their value in
the current event: 1 = just basic viewpoint being predicted, 2 = all
basic viewpoints.")

(defparameter *models* :both+) ; :ltm, :ltm+, :stm, :both, :both+ 

;arithmetic-combination
;geometric-combination
;bayesian-combination
;ranked-combination
(defparameter *ltm-stm-combination* 'geometric-combination)
(defparameter *viewpoint-combination* 'geometric-combination)
(defparameter *ltm-stm-bias* 7)
(defparameter *viewpoint-bias* 2)
  


