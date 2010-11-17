;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       gradus.lisp
;;;; Author:     marcusp <m.pearce@gold.ac.uk>
;;;; Created:    <2006-06-06 16:11:48 marcusp>
;;;; Time-stamp: <2007-01-15 10:42:52 marcusp>
;;;; ======================================================================

;; STM 
;; ========================================================================
;; System                                                Score
;; ------------------------------------------------------------------------
;;   NIL                                                   NIL
;;   ((CPINT CPINTFREF))                                   1.65553223198181
;;   ((CPITCH DELTAST) (CPINT CPINTFREF))                  1.61793905171382
;;   (CPINTFIB (CPITCH DELTAST) (CPINT CPINTFREF))         1.57369955283917
;;   ((CPINT INSCALE) CPINTFIB (CPITCH DELTAST) 
;;    (CPINT CPINTFREF))                                   1.54320646845752
;;   (CPITCH (CPINT INSCALE) CPINTFIB (CPITCH DELTAST) 
;;    (CPINT CPINTFREF))                                   1.53718968354707
;;   ((CPINT DELTAST) CPITCH (CPINT INSCALE) CPINTFIB 
;;    (CPITCH DELTAST) (CPINT CPINTFREF))                  1.52890608388879
;;   (THRBAR (CPINT DELTAST) CPITCH (CPINT INSCALE) 
;;    CPINTFIB (CPITCH DELTAST) (CPINT CPINTFREF))         1.52617529996579

(cl:defpackage #:gradus
  (:use #:cl)
  (:export #:gradus #:gradus->midi
           #:predict-gradus #:select-gradus)
  (:documentation "Analysis of Gradus by Philip Glass."))

(cl:in-package #:gradus)


;;; Representation 

(defun gradus ()
  (md:get-event-sequence 26 0))

(defun gradus->midi (path)
  (let ((db2midi::*default-tempo* 170))
    (db2midi::events->midi (gradus) path)))


;;; Modelling 

(defvar *gradus-features* 
  '(cpitch cpint cpintfref 
    (cpitch deltast) (cpint deltast) (cpintfref deltast)
    (cpint cpintfref)))
;;     (append #1='(cpitch cpint cpint-size contour newcontour 
;;                cpitch-class cpcint 
;;                cpintfref cpintfip cpintfib)
;;           (utils:cartesian-product #1# '(deltast))
;;           '((cpint cpintfref) (cpintfref mode) (cpint inscale))
;;           '(thrbar)))

(defparameter *datasets* '(0 1 7))

(defun select-gradus (&key start-state (datasets *datasets*) (path t) 
                      (models :both+) (order nil))
  (viewpoint-selection:run-hill-climber 
   *gradus-features*
   start-state
   #'(lambda (x) 
       (car 
        (prediction-sets:average-codelengths 
         (predict-gradus x datasets :path path :models models :order order))))
   :desc))

(defun predict-gradus (attributes training-ids &key (path t)
                       (models :both+) (order nil))
  (let* ((training-set (apply #'md:get-event-sequences training-ids))
         (viewpoints (viewpoints:get-viewpoints attributes))
         (cpitch-viewpoint 
          (car (viewpoints:get-basic-viewpoints (list 'cpitch) training-set)))
         (gradus (gradus))
         (ltms (resampling:get-long-term-models viewpoints training-set 
                                                training-ids nil nil nil))
         (models (if (null training-ids) :stm models))
         (mvs::*models* models)
         (mvs::*ltm-order-bound* order)
         (mvs::*stm-order-bound* order)
         (mvs (mvs:make-mvs (list cpitch-viewpoint) viewpoints ltms)))
    (viewpoints:initialise-basic-viewpoints (list gradus))
    (let ((predictions (car (mvs:model-dataset mvs (list gradus) 
                                               :construct? t :predict? t))))
      (when path 
        (let ((actual-path 
               (if (stringp path) 
                   path 
                   (compute-path models training-ids order attributes))))
          (output-data gradus predictions actual-path)))
      predictions)))
    
(defun compute-path (models training-ids order attributes) 
  (format nil "~A~A~A~(~A~)-~{~A~^_~}~A~(~A~)-~{~(~A~)~^_~}.dat" 
          apps:*root-dir* 
          "R/gradus/data/"
          "gradus-"
          models
          (if (eql models :stm) (list "nil") training-ids)
          (if (null training-ids) "" "-")
          order
          (collapse-attribute-list attributes)))

(defun collapse-attribute-list (attributes)
  (mapcar #'(lambda (x) 
              (if (consp x)
                  (format nil "~{~(~A~)~^+~}" x)
                  (format nil "~(~A~)" x)))
          attributes))

(defun output-data (gradus predictions path)
  (let* ((entropies (car (prediction-sets:shannon-entropies predictions)))
         (codelengths (car (prediction-sets:codelengths predictions)))
         (probabilities 
          (mapcar #'cadr 
                  (car (prediction-sets:event-predictions predictions))))
         (eventnum (eventnum-column gradus))
         (rests (rest-column gradus))
         (pitches (pitch-column gradus)))
    (with-open-file (o path :direction :output :if-exists :supersede)
      (format o "~&event pitch rest probability entropy codelength~%")
      (mapc #'(lambda (ev pit r pr e c)
                (format o "~&~A ~A ~A ~,2F ~,2F ~,2F~%" 
                        ev pit r pr e c))
            eventnum pitches rests probabilities entropies codelengths)))
  path)
      
(defun eventnum-column (gradus)
  (let ((eventnums '()))
    (dotimes (i (length gradus) (nreverse eventnums))
      (push (1+ i) eventnums))))

(defun pitch-column (gradus)
  (let ((pitches '()))
    (dolist (event gradus (nreverse pitches))
      (push (md:get-attribute event 'cpitch) pitches))))

(defun rest-column (gradus)
  (let ((rests '(0)))
    (dolist (event (cdr gradus) (nreverse rests))
      (if (> (md:get-attribute event 'cpitch) 0)
          (push 1 rests) 
          (push 0 rests)))))

(defun write-onsets (path) 
  (let ((gradus (gradus)))
    (with-open-file (o path :direction :output :if-exists :supersede)
      (format o "onset barline~%")
      (dolist (e gradus)
        (let* ((onset (/ (md:get-attribute e 'onset) 12))
               (barline (if (zerop (mod onset 32)) 1 0)))
          (format o "~&~A ~A~%" onset barline))))))

