(cl:defpackage #:ppm-star
  (:use #:cl #:psgraph)
  (:nicknames #:ppm)
  (:export "PPM" "GET-ROOT" "REINITIALISE-PPM" "SET-PPM-PARAMETERS"
           "SET-ALPHABET" "INCREMENT-SEQUENCE-FRONT" "INCREMENT-EVENT-FRONT"
           "MODEL-DATASET" "MODEL-SEQUENCE" "PPM-MODEL-EVENT" 
           "MODEL-SENTINEL-EVENT" "INITIALISE-VIRTUAL-NODES" 
           "WRITE-MODEL-TO-POSTSCRIPT" "WRITE-MODEL-TO-FILE"
           "READ-MODEL-FROM-FILE" "GET-MODEL"
           ;; From ppm-ui.lisp
           "PPM-PREDICT" "BUILD-MODEL" "NGRAM-FREQUENCIES")
  (:documentation "Prediction by Partial Match modelling including methods for model initialisation, construction and prediction."))
