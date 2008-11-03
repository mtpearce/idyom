;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-             
;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2003-04-05 18:54:17 marcusp>                           
;;;; Time-stamp: <2008-11-03 14:24:39 marcusp>                           
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage :prediction-sets
  (:use #:cl #:utils #:viewpoints)
  (:export "DATASET-PREDICTION" "COMPOSITION-PREDICTION" "EVENT-PREDICTION"
           "PREDICTION-VIEWPOINT" "PREDICTION-SET" "PREDICTION-ELEMENT"
           "PREDICTION-INDEX" "MAKE-EVENT-PREDICTION"
           "MAKE-DATASET-PREDICTION" "MAKE-SEQUENCE-PREDICTION"
           "COMBINE-DISTRIBUTIONS" "ARITHMETIC-COMBINATION"
           "GEOMETRIC-COMBINATION" "RANKED-COMBINATION" "BAYESIAN-COMBINATION"
           "AVERAGE-CODELENGTHS" "AVERAGE-CODELENGTH" "CODELENGTHS"
           "SHANNON-ENTROPIES" "EVENT-PREDICTIONS"
           "SEQUENCE-PROBABILITY" "NORMALISE-DISTRIBUTION" "FLAT-DISTRIBUTION")
  (:documentation "Entropy based performance metrics, function for
combining probability distributions and other utilities for use with
distributions."))

(defpackage #:multiple-viewpoint-system  
  (:use #:cl #:utils #:ppm #:prediction-sets #:viewpoints)
  (:nicknames #:mvs)
  (:export "MODEL-DATASET" "MODEL-SEQUENCE" "MODEL-EVENT"
           "SET-MODEL-ALPHABETS"
           "MVS" "MAKE-MVS" "SET-MVS-PARAMETERS" "MVS-BASIC" 
           "COUNT-VIEWPOINTS" "GET-EVENT-ARRAY" "OPERATE-ON-MODELS"
           "COMBINE-PREDICTIONS" "SET-LTM-STM-COMBINATION" 
           "SET-MODELS" "GET-MODELS" "WITH-MODELS" 
           "SET-LTM-STM-BIAS" "SET-VIEWPOINT-BIAS" "SET-VIEWPOINT-COMBINATION" 
           "COMBINE-LTM-STM-DISTRIBUTIONS" "COMBINE-VIEWPOINT-DISTRIBUTIONS" 
           "STORE-EP-CACHE" "LOAD-EP-CACHE" "INITIALISE-EP-CACHE" 
           "DISABLE-EP-CACHE" "CACHE-EP" "CACHED-EP" "*EP-CACHE-DIR*"
           "*MARGINALISE-USING-CURRENT-EVENT*")
  (:documentation "A multiple viewpoint system."))

