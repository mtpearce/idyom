;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-05 18:54:17 marcusp>                        
;;;; Time-stamp: <2023-05-22 13:27:58 marcusp>                           
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage #:apps
  (:use #:cl #:utils #:md #:mvs)
  (:export #:*root-dir* #:dataset-modelling-filename)
  (:documentation "Miscellaneous variables & utils for applications."))

(defpackage #:idyom
  (:use #:cl)
  (:export #:idyom #:conklin90 #:conkwit95 #:pearce05)
  (:documentation "Main access to IDyOM functionality."))

(defpackage #:resampling 
  (:use #:cl #:utils #:md #:viewpoints #:ppm #:mvs #:prediction-sets)
  (:export #:idyom-resample #:output-information #:format-information-content
           #:cached-dataset-prediction 
           #:build-ep-cache #:select-viewpoints-for-generation 
           #:get-resampling-sets #:get-training-set #:get-test-set 
           #:get-long-term-models #:monodies-to-lists
           #:test-combinations)
  (:documentation "Prediction of datasets using cross-validation."))

(defpackage #:viewpoint-selection
  (:use #:cl #:utils)
  (:export #:run-best-first #:run-hill-climber 
           #:load-vs-cache #:store-vs-cache #:initialise-vs-cache
           #:dataset-viewpoint-selection)
  (:documentation "Selection of viewpoints."))

