;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-05 18:54:17 marcusp>                        
;;;; Time-stamp: <2014-02-07 23:10:27 marcusp>                           
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage #:apps
  (:use #:cl #:utils #:md #:mvs)
  (:export "POPULATE-DATABASE" "DATASET-MODELLING-FILENAME")
  (:documentation "Miscellaneous variables & utils for applications."))

(defpackage #:idyom
  (:use #:cl)
  (:export "IDYOM" "CONKLIN90" "CONKWIT95" "PEARCE05")
  (:documentation "Main access to IDyOM functionality."))

(defpackage #:inference
  (:use #:cl)
  (:export "INFER-METER")
  (:documentation "Categorical inference functionality."))

(defpackage #:rhythms
  (:use #:cl)
  (:export "IOI-LIST->EVENT-SEQUENCE" "GRID->GRID-EVENTS"
	   "AGBEKOR" "SHAVE-AND-A-HAIRCUT" "SON-CLAVE")
  (:documentation "Utility functions for rhythms"))

(defpackage #:resampling 
  (:use #:cl #:utils #:md #:viewpoints #:ppm #:mvs #:prediction-sets)
  (:export "IDYOM-RESAMPLE" "OUTPUT-INFORMATION-CONTENT" "FORMAT-INFORMATION-CONTENT"
           "INFORMATION-CONTENT-PROFILES" "CACHED-DATASET-PREDICTION" 
           "BUILD-EP-CACHE" "SELECT-VIEWPOINTS-FOR-GENERATION" 
           "GET-RESAMPLING-SETS" "GET-TRAINING-SET" "GET-TEST-SET" 
           "GET-LONG-TERM-MODELS"
           "TEST-COMBINATIONS")
  (:documentation "Prediction of datasets using cross-validation."))

(defpackage #:viewpoint-selection
  (:use #:cl #:utils)
  (:export "RUN-BEST-FIRST" "RUN-HILL-CLIMBER" 
           "LOAD-VS-CACHE" "STORE-VS-CACHE" "INITIALISE-VS-CACHE"
           "DATASET-VIEWPOINT-SELECTION")
  (:documentation "Selection of viewpoints."))

(defpackage #:generation 
  (:use #:cl #:utils #:md #:viewpoints #:ppm #:mvs #:prediction-sets 
        #:resampling)
  (:export "DATASET-GENERATION" "GENERATE-CHORALES")
  (:documentation "Generation of melodic compositions."))
