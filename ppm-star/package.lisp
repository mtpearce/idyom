(cl:defpackage #:ppm-star
  (:use #:cl #:psgraph)
  (:nicknames #:ppm)
  (:export #:ppm #:get-root #:reinitialise-ppm #:set-ppm-parameters
           #:set-alphabet #:increment-sequence-front #:increment-event-front
           #:model-dataset #:model-sequence #:ppm-model-event 
           #:model-sentinel-event #:initialise-virtual-nodes 
           #:write-model-to-postscript #:write-model-to-file
           #:read-model-from-file #:get-model
           ;; from ppm-ui.lisp
           #:ppm-predict #:build-model #:ngram-frequencies)
  (:documentation "Prediction by Partial Match modelling including methods for model initialisation, construction and prediction."))
