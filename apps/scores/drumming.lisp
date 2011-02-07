;;;; ======================================================================
;;;; File:       drumming.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2011-02-07 12:57:59 marcusp>
;;;; Time-stamp: <2011-02-07 15:37:32 marcusp>
;;;; ======================================================================

(cl:defpackage #:drumming
  (:use #:cl)
  (:export #:drumming #:predict-drumming #:select-drumming #:drumming->midi)
  (:documentation "Analysis of Drumming (1971) by Steve Reich."))

(cl:in-package #:drumming) 

;;; Parameters

(defvar *pattern* '((9 58) (11 59) (8 59) (1 55) (2 59) (5 59) (3 61) (7 61)))
(defvar *repeats* 6)
(defvar *note-duration* 12)
(defvar *dataset-id* 1002)
(defvar *tempo* 140) 
(defvar *voice* 1)
(defvar *dynamics* 100)

;;; generate midi

(defun drumming->midi (path)
  (let ((db2midi::*default-tempo* *tempo*))
    (db2midi::event-lists->midi (list (drumming)) path :format 1 :program 1)))

;;; Representation

(defun drumming ()
  (let ((pitches (generate-pitches))
        (events '())
        (onset 0)
        (event-id 0)
        (previous-offset 0))
    (dolist (pitch pitches (nreverse events))
      (if (zerop pitch)
          (incf onset *note-duration*)
          (progn
            (push (drumming-event event-id onset (- onset previous-offset) pitch) events)
            (setf previous-offset (+ onset *note-duration*))
            (incf event-id)
            (incf onset *note-duration*))))))

(defun generate-figure (n)
  (let ((figure (make-list 12 :initial-element 0)))
    (dotimes (i n figure)
      (let ((note (nth i *pattern*)))
        (setf (elt figure (first note)) (second note))))))

(defun generate-pitches ()
  (let ((drumming '()))
    (dotimes (i 8 drumming)
      (let ((figure (generate-figure (1+ i))))
        (dotimes (j *repeats*)
          (setf drumming (append drumming figure)))))))

(defun drumming-event (event-id onset deltast cpitch)
  (make-instance 'amuse-mtp::mtp-event
                 :dataset-id     *dataset-id* 
                 :composition-id 1
                 :event-id       event-id
                 :voice          *voice*
                 ;;:onset          onset
                 ;;:dur            *note-duration*
                 :time           onset
                 :interval       *note-duration*
                 :deltast        deltast
                 :bioi           (if (= event-id 0) deltast (+ deltast *note-duration*))
                 :cpitch         cpitch
                 :mpitch         nil
                 :accidental     nil
                 :keysig         6 
                 :mode           0
                 :barlength      nil
                 :pulses         nil 
                 :phrase         nil
                 ;;:ornament       0
                 :dyn            *dynamics*
                 :tempo          *tempo*))


;;; Modelling 

(defun predict-drumming (dataset-id)
  (multiple-value-bind (a b c)
      (resampling:output-information-content  
       (resampling:dataset-prediction dataset-id '(dyn bioi) '(dyn bioi) :models :stm :k 1))
    (declare (ignore a b))
    c))
