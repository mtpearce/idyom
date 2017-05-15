;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-05 18:54:17 marcusp>                           
;;;; Time-stamp: <2017-05-15 01:46:14 peter>                           
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage :prediction-sets
  (:use #:cl #:utils #:viewpoints)
  (:export "DATASET-PREDICTION" "SEQUENCE-PREDICTION" "EVENT-PREDICTION"
           "PREDICTION-VIEWPOINT" "PREDICTION-SET" "PREDICTION-ELEMENT"
           "PREDICTION-INDEX" "PREDICTION-WEIGHTS" "PREDICTION-EVENT" "PREDICTION-ORDER"
           "MAKE-EVENT-PREDICTION" "MAKE-DATASET-PREDICTION" "MAKE-SEQUENCE-PREDICTION"
           "MULTIPLY-PREDICTIONS" "COMBINE-DISTRIBUTIONS" "ARITHMETIC-COMBINATION"
           "GEOMETRIC-COMBINATION" "RANKED-COMBINATION" "BAYESIAN-COMBINATION"
           "AVERAGE-CODELENGTHS" "AVERAGE-CODELENGTH" "CODELENGTHS" "CODELENGTH"
           "SHANNON-ENTROPY" "SHANNON-ENTROPIES" "EVENT-PREDICTIONS"
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
	   "FORMAT-EVENT-PREDICTION" "COMBINE-EVENT-PROBABILITIES"
	   "ADD-RESULTS-TO-DATAFRAME"
           "STORE-EP-CACHE" "LOAD-EP-CACHE" "INITIALISE-EP-CACHE" 
           "DISABLE-EP-CACHE" "CACHE-EP" "CACHED-EP" "*EP-CACHE-DIR*"
	   "MEMORY-STORE-PARAMS"
           "*MARGINALISE-USING-CURRENT-EVENT*")
  (:documentation "A multiple viewpoint system."))

