;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-05 18:54:17 marcusp>                           
;;;; Time-stamp: <2023-05-22 13:25:42 marcusp>                           
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage #:prediction-sets
  (:use #:cl #:utils #:viewpoints)
  (:export #:dataset-prediction #:sequence-prediction #:event-prediction
           #:prediction-viewpoint #:prediction-set #:prediction-element
           #:prediction-index #:prediction-weights #:prediction-event #:prediction-order
           #:make-event-prediction #:make-dataset-prediction #:make-sequence-prediction
           #:multiply-predictions #:combine-distributions #:arithmetic-combination
           #:geometric-combination #:ranked-combination #:bayesian-combination
           #:average-codelengths #:average-codelength #:codelengths #:codelength
           #:shannon-entropy #:shannon-entropies #:event-predictions
           #:sequence-probability #:normalise-distribution #:flat-distribution)
  (:documentation "Entropy based performance metrics, function for
combining probability distributions and other utilities for use with
distributions."))

(defpackage #:multiple-viewpoint-system  
  (:use #:cl #:utils #:ppm #:prediction-sets #:viewpoints)
  (:nicknames #:mvs)
  (:export #:model-dataset #:model-sequence #:model-event
           #:set-model-alphabets
           #:mvs #:make-mvs #:set-mvs-parameters #:mvs-basic 
           #:count-viewpoints #:get-event-array #:operate-on-models
           #:combine-predictions #:set-ltm-stm-combination 
           #:set-models #:get-models #:with-models 
           #:set-ltm-stm-bias #:set-viewpoint-bias #:set-viewpoint-combination 
           #:combine-ltm-stm-distributions #:combine-viewpoint-distributions 
           #:store-ep-cache #:load-ep-cache #:initialise-ep-cache 
           #:disable-ep-cache #:cache-ep #:cached-ep #:*ep-cache-dir*
           #:memory-store-params
           #:*marginalise-using-current-event*)
  (:documentation "A multiple viewpoint system."))

