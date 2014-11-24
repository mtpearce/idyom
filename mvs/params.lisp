;;;; ======================================================================
;;;; File:       params.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-06-16 18:54:17 marcusp>                           
;;;; Time-stamp: <2014-11-22 13:04:09 marcusp>                           
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

;;; Define a set of parameters for a memory store, e.g. an STM or LTM.
(defun memory-store-params (order-bound mixtures update-exclusion escape)
  `(:order-bound ,order-bound :mixtures ,mixtures :update-exclusion ,update-exclusion :escape ,escape))
;;; Default store specs
(defparameter *ltm-params* (memory-store-params mvs::*ltm-order-bound* mvs::*ltm-mixtures* mvs::*ltm-update-exclusion* mvs::*ltm-escape*))
(defparameter *stm-params* (memory-store-params mvs::*stm-order-bound* mvs::*stm-mixtures* mvs::*stm-update-exclusion* mvs::*stm-escape*))

(defparameter *marginalise-using-current-event* 2
  "Specifies the list of basic viewpoints which assume their full
alphabets in prediction rather than being marginalised out based on
their value in the current event. See mvs:set-model-alphabets for
details. Default = 2, all basic viewpoints.")

(defparameter *models* :both+) ; :ltm, :ltm+, :stm, :both, :both+ 

;arithmetic-combination
;geometric-combination
;bayesian-combination
;ranked-combination
(defparameter *ltm-stm-combination* 'geometric-combination)
(defparameter *viewpoint-combination* 'geometric-combination)
(defparameter *ltm-stm-bias* 7)
(defparameter *viewpoint-bias* 2)
