;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       two-pages.lisp
;;;; Author:     marcusp <m.pearce@gold.ac.uk>
;;;; Created:    <2006-06-06 16:11:48 marcusp>
;;;; Time-stamp: <2007-01-12 13:28:18 marcusp>
;;;; ======================================================================

#||
LTM Datasets: 0 1 7 
===========================================================================
   System                                                Score
---------------------------------------------------------------------------
   NIL                                                   NIL
   (CPINT)                                               0.31996297273698
   (CPINT-SIZE)                                          0.30776576957884
   (CPINTFIP CPINT-SIZE)                                 0.29069830122758
===========================================================================

LTM Datasets: nil 
=============================================================================
  System                                                Score
-----------------------------------------------------------------------------
  NIL                                                   NIL
  (CPINT)                                               0.3664170533537489d0
=============================================================================
||#


(cl:defpackage #:two-pages
  (:use #:cl)
  (:export #:two-pages #:predict-two-pages #:select-two-pages 
           #:two-pages->midi)
  (:documentation "Analysis of Two Pages by Philip Glass."))

(cl:in-package #:two-pages) 


;;; Representation 

(defvar *original-pitch-classes* (list 0 5 7 8 10)
  "The pitch classes of the five constituent notes in the original piece.")
(defvar *chromatic-pitch-classes* (list 0 1 2 3 4)
  "The pitch classes of the five constituent notes in a chromatic version.")
(defvar *whole-tone-pitch-classes* (list 0 2 4 6 8)
  "The pitch classes of the five constituent notes in a whole tone version.")
(defparameter *original-reference-cpitch* 67 
  "MIDI pitch number for the reference (lowest) pitch; default is G_4.")
(defparameter *original-reference-mpitch* 39
  "Morphetic pitch number for the reference (lowest) pitch; default is G_4.")
(defparameter *original-event-duration* 12  
  "The event duration in 96th note ticks; default is a quaver.")

(defvar *dataset-id* 100) 
(defvar *figures* (make-hash-table))
(defvar *repetitions* (make-hash-table))

(defun get-repetition (figure-number) 
  (gethash figure-number *repetitions*))

(defun set-repetition (figure-number repetitions) 
  (setf (gethash figure-number *repetitions*) repetitions))

(defun get-figure (figure-number) 
  (gethash figure-number *figures*))

(defun set-figure (figure-number figure) 
  (setf (gethash figure-number *figures*) figure))

(defun count-figures ()
  (hash-table-count *figures*))

(defun count-repetitions()
  (let ((count 0))
    (maphash #'(lambda (k v) 
                 (declare (ignore k))
                 (incf count v))
             *repetitions*)
    count))

(defun clear-figures ()
  (setf *figures* (make-hash-table)
        *repetitions* (make-hash-table)))

(defmacro define-figure ((number &optional (repetitions 1)) &body body)
  `(progn 
    ;;(format t "~&define-figure ~A~%" ,number)
    (set-figure ,number ,@body)
    (set-repetition ,number ,repetitions)
    ,number))

(defun rep (list n)
  "Return a new list consisting of N repetitions of LIST."
  (let ((result '()))
    (dotimes (i n (nreverse result))
      (dolist (e list)
        (push e result)))))

(defun get-part (figure-number) 
  (cond ((< 0 figure-number 8) 1)
        ((< figure-number 40) 2)
        ((< figure-number 56) 3) 
        ((< figure-number 73) 4)
        ((< figure-number 76) 5)
        (t (error "Invalid figure number: ~A" figure-number))))

(defun figure-length (figure-number) 
  (length (get-figure figure-number)))

;; PART 1: figures 1-7 
(define-figure ( 1 34) '(1 2 3 4 5))
(define-figure ( 2 18) '(1 2 3 4 5 1 2 3 4))
(define-figure ( 3 14) '(1 2 3 4 5 1 2 3 4 1 2 3))
(define-figure ( 4 15) '(1 2 3 4 5 1 2 3 4 1 2 3 1 2))
(define-figure ( 5 17) '(1 2 3 4 5 1 2 3 4 1 2 3))
(define-figure ( 6 22) '(1 2 3 4 5 1 2 3 4))
(define-figure ( 7 26) '(1 2 3 4 5))
;; PART 2: figures  8-41 [mm. 8-39] 
(define-figure ( 8 26) '(1 2 3 4 5 3))
(define-figure ( 9 18) '(1 2 3 4 5 3 4))
(define-figure (10 11) '(1 2 3 4 5 3 4 5))
(define-figure (11 16) '(1 2 3 4 5 3 4 5 3))
(define-figure (12 14) '(1 2 3 4 5 3 4 5 3 4))
(define-figure (13 11) '(1 2 3 4 5 3 4 5 3 4 5))
(define-figure (14  7) '(1 2 3 4 5 3 4 5 3 4 5 3))
(loop for beta  in '(3  4 5 6 7 8 9 10 12 14 16 18 16 14 12 10 9 8 7 6 5 4 3 2  1) 
      for alpha in '(11 6 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1 1 1 1 1 7 9 11 18)
      for fignum from 15
      do (define-figure (fignum alpha) 
           `(1 2 3 4 ,@(rep '(5 3 4) beta))))
;; PART 3: figures 42-59 [mm. 40-55a]  
(define-figure (40 3) '(1 2 3 4 1 2 3 4 5 3 4))
(loop for beta in  '(1 2 3 4 5 6 7 8 9 10 12 14 16 18 20)
      for alpha in '(7 6 3 1 1 1 1 1 1 1  1  1  1  1  1)
      for fignum from 41
      do (define-figure (fignum alpha) 
           `(,@(rep '(1 2 3 4 1 2 3) beta) 1 2 ,@(rep '(3 4 5 3 4) beta))))
;; PART 4: figures 60-78 [mm. 55b-71] 
(define-figure (56 19) '(3 4 5 3 4))
(loop for beta  in '(1 2 3 4 5 6 7 8 9 10 12 14 16 18 20)
      for alpha in '(9 6 3 4 1 1 1 1 1 1  1  1  1  1  1)
      for fignum from 57
      do (define-figure (fignum alpha)
           `(,@(rep '(2 3 4 5) beta) 3 4 5 3 4)))
(define-figure (72 21) '(2 3 4 5))
;; PART 5: figures 79-81 [mm. 72-4] 
(define-figure (73 14) '(1 2 3 4 5 2 3 4 5))
(define-figure (74  7) '(1 2 3 4 5 2 3 4 5 3 4 5))
(define-figure (75 10) '(1 2 3 4 5 2 3 4 5 3 4 5 4 5))

(defun two-pages->midi (path &rest args)
  (let ((db2midi::*default-tempo* 170))
    (db2midi::events->midi (apply #'two-pages args) path)))

(defun print-pitches (path reference-cpitch pitch-classes) 
  (let ((pitches (mapcar #'(lambda (x) (+ reference-cpitch x))
                         pitch-classes)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (dolist (e (two-pages:two-pages))
        (let* ((pitch (md:get-attribute e 'cpitch))
               (pnum (position pitch pitches)))
          (format out "~D~%" pnum))))))

(defun two-pages (&key (pitch-classes *original-pitch-classes*)
                  (reference-cpitch *original-reference-cpitch*)
                  (reference-mpitch *original-reference-mpitch*)
                  (event-duration *original-event-duration*)) 
  ;; &key repetitions 
  (let ((event-list '())
        (event-id 0))
    (dotimes (i (count-figures) (nreverse event-list)) 
      (let* ((fignum (1+ i))
             (figure (get-figure fignum)))
        (when figure 
          (dotimes (i (get-repetition fignum))
            (dolist (pitchnum figure)
              (push (two-pages-event event-id pitchnum event-duration 
                                     reference-cpitch reference-mpitch 
                                     pitch-classes)
                    event-list)
              (incf event-id))))))))

(defun two-pages-event (event-id pitchnum duration 
                        reference-cpitch reference-mpitch pitch-classes)
  (make-instance 'md:event
                 :dataset-id     *dataset-id* 
                 :composition-id 1
                 :event-id       event-id
                 :voice          1
                 :onset          (* event-id duration) 
                 :deltast        0 
                 :cpitch         (two-pages-cpitch pitchnum reference-cpitch 
                                                   pitch-classes)
                 :mpitch         (two-pages-mpitch pitchnum reference-mpitch)
                 :accidental     (two-pages-accidental pitchnum)
                 :dur            duration 
                 :keysig         -3 ; C
                 :mode           9 ; minor
                 :barlength      nil 
                 :pulses         nil 
                 :phrase         nil 
                 :dyn            nil 
                 :tempo          nil))

(defun two-pages-cpitch (n reference pitch-classes) 
  "Two Pages is composed of 5 pitches: G, C, D, Eb F. Return the MIDI
pitch number corresponding to the N+1th of these pitches."
  (+ (nth (1- n) pitch-classes) reference))

(defun two-pages-mpitch (n reference) 
  (+ (1- n) reference))

(defun two-pages-accidental (n) 
  (case n 
    (4 -1)
    (t 0)))


;;; Modelling 

(defvar *two-pages-features* 
  '(cpitch cpint cpintfref 
    (cpint cpintfref)
    ;; cpint-size contour newcontour 
    ;; cpitch-class cpcint 
    ;;cpintfip
    ))

(defparameter *datasets* '(0 1 7))

(defun select-two-pages (&key start-state (datasets *datasets*) (path t) 
                         (models :both+) (order nil))
  (viewpoint-selection:run-hill-climber 
   *two-pages-features*
   start-state
   #'(lambda (x) 
       (car 
        (prediction-sets:average-codelengths 
         (predict-two-pages x datasets :path path :models models 
                            :order order))))
   :desc))

(defun predict-two-pages (attributes training-ids &key (path t)
                          (models :both+) (order nil)
                          (pitch-classes *original-pitch-classes*)
                          (reference-cpitch *original-reference-cpitch*)
                          (reference-mpitch *original-reference-mpitch*)
                          (event-duration *original-event-duration*))
  (let* ((training-set (apply #'md:get-event-sequences training-ids))
         (viewpoints (viewpoints:get-viewpoints attributes))
         (cpitch-viewpoint 
          (car (viewpoints:get-basic-viewpoints (list 'cpitch) training-set)))
         (two-pages (two-pages :pitch-classes pitch-classes 
                               :reference-cpitch reference-cpitch
                               :reference-mpitch reference-mpitch
                               :event-duration event-duration))
         (ltms (resampling:get-long-term-models viewpoints training-set 
                                                training-ids nil nil nil))
         (models (if (null training-ids) :stm models))
         (mvs::*models* models)
         (mvs::*ltm-order-bound* order)
         (mvs::*stm-order-bound* order)
         (mvs (mvs:make-mvs (list cpitch-viewpoint) viewpoints ltms)))
    (viewpoints:initialise-basic-viewpoints (list two-pages))
    (let ((predictions (car (mvs:model-dataset mvs (list two-pages) 
                                               :construct? t :predict? t))))
      (when path 
        (let ((actual-path 
               (if (stringp path) 
                   path 
                   (compute-path models training-ids order attributes))))
          (output-data predictions actual-path reference-cpitch 
                       pitch-classes)))
      predictions)))
    
(defun compute-path (models training-ids order attributes) 
  (format nil "~A~A~A~(~A~)-~{~A~^_~}~A~(~A~)-~{~(~A~)~^_~}.dat" 
          apps:*root-dir* 
          "R/two-pages/data/"
          "two-pages-"
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

(defun output-data (predictions path reference-cpitch pitch-classes)
  (let* ((entropies (car (prediction-sets:shannon-entropies predictions)))
         (codelengths (car (prediction-sets:codelengths predictions)))
         (probabilities 
          (mapcar #'cadr 
                  (car (prediction-sets:event-predictions predictions))))
         (partnum (partnum-column))
         (fignum (fignum-column))
         (repnum (repnum-column)) ; repetitions in alpha process 
         (eventnum (eventnum-column))
         (pitches (pitch-column reference-cpitch pitch-classes)))
    (with-open-file (o path :direction :output :if-exists :supersede)
      (format o "~&part figure alpha event pitch probability entropy codelength~%")
      (mapc #'(lambda (p f r ev pit pr e c)
                (format o "~&~A ~A ~A ~A ~A ~,2F ~,2F ~,2F~%" 
                        p f r ev pit pr e c))
            partnum fignum repnum eventnum pitches probabilities entropies 
            codelengths)))
  path)
                
(defun partnum-column ()
  (let ((partnums '()))
    (dotimes (i (count-figures) (nreverse partnums))
      (let* ((fignum (1+ i))
             (partnum (get-part fignum)))
        (dotimes (j (get-repetition fignum))
          (dotimes (k (figure-length fignum))
            (push partnum partnums)))))))

(defun fignum-column ()
  (let ((fignums '()))
    (dotimes (i (count-figures) (nreverse fignums))
      (let ((fignum (1+ i)))
        (dotimes (j (get-repetition fignum))
          (dotimes (k (figure-length fignum))
            (push fignum fignums)))))))

(defun repnum-column ()
  (let ((repnums '()))
    (dotimes (i (count-figures) (nreverse repnums))
      (let ((fignum (1+ i)))
        (dotimes (j (get-repetition fignum))
          (let ((repnum (1+ j)))
            (dotimes (k (figure-length fignum))
              (push repnum repnums))))))))
      
(defun eventnum-column ()
  (let ((eventnums '()))
    (dotimes (i (count-figures) (nreverse eventnums))
      (let ((fignum (1+ i)))
        (dotimes (j (get-repetition fignum))
          (dotimes (k (figure-length fignum))
            (push (1+ k) eventnums)))))))

(defun pitch-column (reference pitch-classes)
  (let ((pitches '()))
    (dotimes (i (count-figures) (nreverse pitches))
      (let ((fignum (1+ i)))
        (dotimes (j (get-repetition fignum))
          (dolist (n (get-figure fignum))
            (push (two-pages-cpitch n reference pitch-classes) pitches)))))))
