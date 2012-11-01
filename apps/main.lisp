;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       viewpoint-selection.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2010-11-01 15:19:57 marcusp>
;;;; Time-stamp: <2012-07-02 17:05:13 marcusp>
;;;; ======================================================================

;; TODO
;; ----
;; 1. extend to other basic viewpoints
;; 2. extend to multiple basic viewpoints
;; 3. allow lists of options in arguments
;; 4. allow construction of new viewpoint frameworks
;; 5. record viewpoint weights in output files
;; 6. extend to more than one dataset

(cl:in-package #:idyom)

(defvar *cpitch-viewpoints* '(;; Chromatic pitch
                              :cpitch       ; chromatic pitch (midi pitch number)
                              :cpitch-class ; octave equivalent pitch class (chroma)
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

(defvar *bioi-viewpoints* '(
                            :bioi           ; inter-onset interval
                            :bioi-ratio     ; ratio between consecutive inter-onset intervals
                            :bioi-contour   ; contour between consecutive inter-onset intervals
                            ))

(defun idyom (dataset-id basic-attributes attributes
              &key 
              ;; dataset-prediction parameters
              pretraining-ids (k 10) (models :both+)
              resampling-indices
              (ltm-order-bound mvs::*ltm-order-bound*)
              (ltm-mixtures mvs::*ltm-mixtures*)
              (ltm-update-exclusion mvs::*ltm-update-exclusion*)
              (ltm-escape mvs::*ltm-escape*)
              (stm-order-bound mvs::*stm-order-bound*)
              (stm-mixtures mvs::*stm-mixtures*)
              (stm-update-exclusion mvs::*stm-update-exclusion*)
              (stm-escape mvs::*stm-escape*)
              ;; output parameters
              (detail 3)
              (output-path nil)
              ;; viewpoint selection parameters
              (dp nil)
              (max-links 2))
  (when (eq attributes :select)
    (format t "~&Selecting viewpoints for the ~A model on dataset ~A predicting viewpoints ~A.~%" 
            models dataset-id basic-attributes)
    (let* ((attributes-universe (generate-viewpoint-systems basic-attributes max-links))
           (selected (viewpoint-selection:dataset-viewpoint-selection
                      dataset-id basic-attributes attributes-universe
                      :dp dp
                      :pretraining-ids pretraining-ids
                      :resampling-indices resampling-indices
                      :k k
                      :models models
                      :ltm-order-bound ltm-order-bound
                      :ltm-mixtures ltm-mixtures
                      :ltm-update-exclusion ltm-update-exclusion
                      :ltm-escape ltm-escape
                      :stm-order-bound stm-order-bound
                      :stm-mixtures stm-mixtures
                      :stm-update-exclusion stm-update-exclusion
                      :stm-escape stm-escape)))
      (setf attributes selected)))
  (multiple-value-bind (predictions filename)
      (resampling:dataset-prediction dataset-id basic-attributes attributes
                                     :pretraining-ids pretraining-ids
                                     :resampling-indices resampling-indices
                                     :k k
                                     :models models
                                     :ltm-order-bound ltm-order-bound
                                     :ltm-mixtures ltm-mixtures
                                     :ltm-update-exclusion ltm-update-exclusion
                                     :ltm-escape ltm-escape
                                     :stm-order-bound stm-order-bound
                                     :stm-mixtures stm-mixtures
                                     :stm-update-exclusion stm-update-exclusion
                                     :stm-escape stm-escape)
    (when output-path
      (resampling:format-information-content predictions (concatenate 'string output-path "/" filename) dataset-id detail))
    (resampling:output-information-content predictions detail)))

(defun generate-viewpoint-systems (basic-viewpoints max-links)
  (if (= (length basic-viewpoints) 1)
      ;; single basic viewpoint
      (let ((derived-viewpoints
             (case (car basic-viewpoints)
               (:cpitch *cpitch-viewpoints*)
               (:bioi *bioi-viewpoints*)
               ;; TODO: extend to other basic viewpoints
               (t (format t "~&No derived viewpoints available for ~A.~%" (car basic-viewpoints))))))
        (reverse 
         (append derived-viewpoints 
                 (sort (remove-if #'(lambda (x) (or (null x) (< (length x) 2) 
                                                    (> (length x) max-links)))
                                  (utils:powerset derived-viewpoints))
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

