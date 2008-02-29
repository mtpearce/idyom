(cl:defpackage #:ppm-star
  (:use #:cl) ;; #:psgraph #:utils)
  (:nicknames #:ppm)
  (:export "PPM" "*ROOT*" "MAKE-PPM" "REINITIALISE-PPM" "SET-PPM-PARAMETERS"
           "SET-ALPHABET" "INCREMENT-SEQUENCE-FRONT" "INCREMENT-EVENT-FRONT"
           "MODEL-DATASET" "MODEL-SEQUENCE" "MODEL-EVENT"
           "MODEL-SENTINEL-EVENT" "INITIALISE-VIRTUAL-NODES"
           "WRITE-MODEL-TO-POSTSCRIPT" "WRITE-MODEL-TO-FILE"
           "READ-MODEL-FROM-FILE" "GET-MODEL" "GET-ALPHABET")
  (:documentation "Prediction by Partial Match modelling including
methods for model initialisation, construction and prediction."))