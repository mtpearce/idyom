;;;; ======================================================================
;;;; File:       generics.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2008-11-03 14:07:53 marcusp>
;;;; Time-stamp: <2018-08-14 14:55:36 marcusp>
;;;; ======================================================================

(cl:in-package #:ppm)

;; top-level functions
(defgeneric model-dataset (model dataset &key construct? predict? 
                           &allow-other-keys))
(defgeneric model-sequence (model sequence &key construct? predict?
                            &allow-other-keys))
(defgeneric ppm-model-event (model event &key construct? predict? 
                               &allow-other-keys))
(defgeneric model-sentinel-event (model location))

;; Utility functions 
(defgeneric dataset-length (ppm))
(defgeneric dataset-sequence-length (ppm sequence-index))
(defgeneric set-model-front (ppm sequence-index event-index))
(defgeneric set-event-front (ppm event-index))
(defgeneric set-sequence-front (ppm sequence-index))
(defgeneric increment-event-front (ppm))
(defgeneric increment-sequence-front (ppm))
(defgeneric set-alphabet (ppm alphabet))
(defgeneric alphabet-size (ppm))
(defgeneric drop (ppm n label))
(defgeneric empty-p (ppm label))
(defgeneric increment-branch-index (ppm))
(defgeneric increment-leaf-index (ppm))
(defgeneric set-branch-record (ppm index record))
(defgeneric set-leaf-record (ppm index record))
(defgeneric get-label (ppm branch-or-leaf))
(defgeneric set-label (ppm branch-or-leaf label))
(defgeneric get-brother (ppm branch-or-leaf))
(defgeneric get-record (ppm branch-or-leaf))
(defgeneric get-count (ppm branch-or-leaf excluded))
(defgeneric get-node-index (ppm branch-or-leaf))
(defgeneric instantiate-label (ppm label &key full))
(defgeneric get-length (ppm label))
(defgeneric get-order (ppm location))
(defgeneric get-matching-child (ppm node symbol))
(defgeneric get-matching-brother (ppm first-child match))
(defgeneric list-children (ppm node))
(defgeneric get-symbol (ppm index))
(defgeneric add-event-to-model-dataset (ppm symbol))

;; initialisation
(defgeneric reinitialise-ppm (ppm))
(defgeneric initialise-nodes (ppm))
(defgeneric initialise-virtual-nodes (ppm))
(defgeneric set-ppm-parameters (ppm &key mixtures escape order-bound 
                                update-exclusion))

;; Suffix tree construction
(defgeneric ukkstep (ppm node location symbol construct?))
(defgeneric update-slink (ppm node location &key occurs? slink))
(defgeneric occurs? (ppm location symbol))
(defgeneric canonise (ppm location sequence))
(defgeneric get-next-location (ppm location))
(defgeneric insert-relevant-suffix (ppm location))
(defgeneric insert-leaf (ppm branch))
(defgeneric split-location (ppm location))

;; Maintaining counts
(defgeneric increment-counts (ppm location novel?))
(defgeneric get-virtual-node-count (ppm location excluded))

;; State selection
(defgeneric select-state (ppm location))

;; Probability estimation
(defgeneric get-distribution (ppm location))
(defgeneric compute-mixture (ppm distribution location excluded 
                             &key update-exclusion escape))
(defgeneric next-distribution (ppm distribution transition-counts node-count
                               excluded weight escape))
(defgeneric next-probability (ppm pair transition-counts node-count excluded
                              weight escape))
(defgeneric weight (ppm node-count child-count))
(defgeneric transition-counts (ppm location update-exclusion))
(defgeneric child-count (ppm transition-counts))
(defgeneric transition-count (ppm symbol transition-counts))
(defgeneric node-count (ppm transition-counts excluded-list))
(defgeneric order-minus1-distribution (ppm distribution excluded escape
                                      update-exclusion))
(defgeneric order-minus1-probability (ppm update-exclusion))

;; Model i/o 
(defgeneric write-model-to-file (ppm filename))
(defgeneric dataset->alist (ppm))
(defgeneric write-model-to-postscript (ppm filename))
