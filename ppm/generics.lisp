;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       generics.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2004-10-28 11:56:59 marcusp>
;;;; Time-stamp: <2008-11-03 12:06:28 marcusp>
;;;; ======================================================================

(cl:in-package #:prediction-sets)

(defgeneric average-codelengths (prediction))
(defgeneric average-codelength (prediction))
(defgeneric codelengths (prediction))
(defgeneric shannon-entropies (prediction))
(defgeneric event-prediction (event-prediction))
(defgeneric event-predictions (prediction))
(defgeneric sequence-probability (sequence-prediction))


(cl:in-package #:ppm-star)

(defgeneric model-dataset (model dataset &key construct? predict? 
                           &allow-other-keys))
(defgeneric model-sequence (model sequence &key construct? predict?
                            &allow-other-keys))
(defgeneric ppm-model-event (model event &key construct? predict? 
                               &allow-other-keys))
(defgeneric model-sentinel-event (model location))

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
(defgeneric get-count (ppm branch-or-leaf &optional excluded))
(defgeneric get-node-index (ppm branch-or-leaf))
(defgeneric instantiate-label (ppm label))
(defgeneric get-length (ppm label))
(defgeneric get-order (ppm location))
(defgeneric get-matching-child (ppm node symbol))
(defgeneric get-matching-brother (ppm first-child match))
(defgeneric list-children (ppm node))
(defgeneric get-symbol (ppm index))
(defgeneric add-event-to-model-dataset (ppm symbol))

(defgeneric reinitialise-ppm (ppm))
(defgeneric initialise-nodes (ppm))
(defgeneric initialise-virtual-nodes (ppm))
(defgeneric set-ppm-parameters (ppm &key mixtures escape order-bound 
                                update-exclusion))

(defgeneric ukkstep (ppm node location symbol construct?))
(defgeneric update-slink (ppm node location &key occurs? slink))
(defgeneric occurs? (ppm location symbol))
(defgeneric canonise (ppm location sequence))
(defgeneric get-next-location (ppm location))
(defgeneric insert-relevant-suffix (ppm location))
(defgeneric insert-leaf (ppm branch))
(defgeneric split-location (ppm location))
(defgeneric increment-counts (ppm location novel?))
(defgeneric get-virtual-node-count (ppm location &optional excluded))
(defgeneric get-distribution (ppm location))
(defgeneric select-state (ppm location))
(defgeneric probability-distribution (ppm location selected?))
(defgeneric compute-mixture (ppm distribution location excluded 
                             &key up-ex escape))
(defgeneric next-distribution (ppm distribution transition-counts node-count
                               excluded weight escape))
(defgeneric next-probability (ppm pair transition-counts node-count excluded
                              weight escape))
(defgeneric weight (ppm node-count child-count))
(defgeneric transition-counts (ppm location up-ex))
(defgeneric child-count (ppm transition-counts))
(defgeneric transition-count (ppm symbol transition-counts))
(defgeneric node-count (ppm transition-counts excluded-list))
(defgeneric order-minus1-distribution (ppm distribution excluded escape
                                      up-ex))
(defgeneric order-minus1-probability (ppm up-ex))
(defgeneric write-model-to-file (ppm filename))
(defgeneric dataset->alist (ppm))
(defgeneric write-model-to-postscript (ppm &key filename))


(cl:in-package #:multiple-viewpoint-system)

(defgeneric count-viewpoints (mvs))
(defgeneric get-event-array (mvs sequence))
(defgeneric operate-on-models (mvs operation &key models ltm-args stm-args))
(defgeneric set-model-alphabets (mvs event events viewpoint ltm stm 
                                     unconstrained))
(defgeneric get-basic-viewpoint (mvs derived-viewpoint))
(defgeneric sequence-prediction-sets (mvs events event-prediction-sets))
(defgeneric dataset-prediction-sets (mvs sequence-prediction-sets))
(defgeneric set-mvs-parameters (mvs &key ltm-order-bound ltm-mixtures
                                ltm-update-exclusion ltm-escape stm-order-bound
                                stm-mixtures stm-update-exclusion stm-escape))

(defgeneric model-event (model event events &key construct? predict? 
                         &allow-other-keys))
