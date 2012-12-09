;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       main.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2010-11-01 15:19:57 marcusp>
;;;; Time-stamp: <2012-12-09 20:19:12 jeremy>
;;;; ======================================================================

(cl:in-package #:idyom)

(defvar *cpitch-viewpoints* '(;; Chromatic pitch
                              :cpitch       ; chromatic pitch (midi pitch number)
                              :cpitch-class ; octave equivalent pitch class (chroma)
			      :cents
                              :tessitura    ; 3 values: whether a note is between 66 (G#4) and 74 (D5), above, or below this range
                              ;; Pitch interval
                              :cpint        ; pitch interval in semitones 
                              :cpint-size   ; absolute size of pitch interval
                              :cpcint       ; pitch interval class (mod 12) 
                              :cpcint-size  ; absolute size of pitc interval class
                              ;; Contour
                              :contour      ; contour (-1, 0, 1)
                              :newcontour   ; boolean: whether or not contour is the same as the previous contour
                              ;; Tonality
                              :cpintfip     ; pitch interval from the first note in the piece
                              :cpintfref    ; chromatic scale degree
                              ;:inscale      ; boolean: whether or not the note is in the scale
                              ))

(defvar *reduced-cpitch-viewpoints* '(:cpitch :cpitch-class :cpint :cpint-size :contour :newcontour))

(defvar *bioi-viewpoints* '(:bioi           ; inter-onset interval
                            :bioi-ratio     ; ratio between consecutive inter-onset intervals
                            :bioi-contour   ; contour between consecutive inter-onset intervals
                            ))


;;; 
;;;
(defun idyom (dataset-id target-viewpoints source-viewpoints
              &key 
	      ;; Dataset IDs for LTM pretraining
              pretraining-ids 
	      ;; Resampling
	      (k 10) ; Number of cross-validation folds (:full = LOO CV)
              resampling-indices ; Evaluate only certain resampling subsets
	      ;; Model options
	      (models :both+)
	      (ltmo mvs::*ltm-params*) (stmo mvs::*stm-params*)
              ;; Viewpoint selection 
	      (basis (case (car target-viewpoints)
		       (:bioi *bioi-viewpoints*)
		       (t *cpitch-viewpoints*)))
              (dp nil) (max-links 2)
              ;; Output 
              (detail 3) (output-path nil))
  "IDyOM top level: computes information profiles for basic
   target-viewpoints over a dataset (dataset-id), using a set of
   source-viewpoints, which can be specified or selected
   automatically.  The LTM is optionally pretrained on multiple
   datasets (pretraining-ids) and/or other members of the target
   dataset using k-fold cross validation (AKA resampling)."
  ;; Select source viewpoints, if requested
  (when (eq source-viewpoints :select)
    (format t "~&Selecting viewpoints for the ~A model on dataset ~A predicting viewpoints ~A.~%" 
            models dataset-id target-viewpoints)
    (let* (; Generate candidate viewpoint systems
	   (viewpoint-systems (generate-viewpoint-systems target-viewpoints basis max-links))
           ; Select viewpoint system
	   (selected (viewpoint-selection:dataset-viewpoint-selection
                      dataset-id target-viewpoints viewpoint-systems
                      :dp dp :pretraining-ids pretraining-ids
                      :k k :resampling-indices resampling-indices
                      :models models :ltmo ltmo :stmo stmo)))
      (setf source-viewpoints selected)))
  ;; Derive target viewpoint IC profile from source viewpoints
  (multiple-value-bind (predictions filename)
      (resampling:idyom-resample dataset-id target-viewpoints source-viewpoints
                                     :pretraining-ids pretraining-ids
				     :k k :resampling-indices resampling-indices
                                     :models models :ltmo ltmo :stmo stmo)
    (when output-path
      (resampling:format-information-content predictions (concatenate 'string output-path "/" filename) dataset-id detail))
    (resampling:output-information-content predictions detail)))


(defun generate-viewpoint-systems (basic-viewpoints basis-vps max-links)
  (if (= (length basic-viewpoints) 1)
      ;; Single basic viewpoint
      (progn (format t "Generating candidate viewpoints from: ~A~%" basis-vps)
	     (reverse 
	      (append basis-vps
		      (sort (remove-if #'(lambda (x) (or (null x) (< (length x) 2) 
							 (> (length x) max-links)))
				       (utils:powerset basis-vps))
			    #'(lambda (x y) (< (length x) (length y)))))))
      ;; TODO: more than 1 basic viewpoint
      (format t "~&Unable to handle more than 1 basic-viewpoint at present.~%")))



;;; 170: unmeasured prelude
;;; 30: 120 hymns
;;; 130: flute corpus
;;; 250: Persian melodies
;;
;; (defun main (dataset-id pretraining-ids k dp basic-viewpoints viewpoints)
;;   (dolist (models '(:stm :ltm :ltm+ :both :both+))
;;     (format t "~&Dataset: ~A; Model: ~A~%" dataset-id models)
;;     (viewpoint-selection:dataset-viewpoint-selection dataset-id basic-viewpoints viewpoints 
;;                                                      :dp dp
;;                                                      :models models
;;                                                      :pretraining-ids pretraining-ids
;;                                                      :k k)))

