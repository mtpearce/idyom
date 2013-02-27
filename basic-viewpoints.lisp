;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       basic-viewpoints.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@eecs.qmul.ac.uk>
;;;; Created:    <2013-01-24 15:00:00 jeremy>
;;;; Time-stamp: <2013-02-27 14:05:26 jeremy>
;;;; ======================================================================

(cl:in-package #:viewpoints)

;;; Basic Viewpoints 

;; Start time
(define-basic-viewpoint onset (events)
  (amuse:timepoint (last-element events)))

;; Cents above C-1
(define-basic-viewpoint cpitch (events)
  (amuse:midi-pitch-number (last-element events)))

;; Duration
(define-basic-viewpoint dur (events)
  (amuse:duration (last-element events)))

;;  Integer indicating the position of the key signature on a line of
;;  fifths with C major = 0. Roughly corresponds to a positive count
;;  of the number of sharps or a negative count of the number of flats
;;  (corresponds exactly if the range is limited as indicated in the
;;  table to +/-7, since this avoids double accidentals, but that
;;  limitation appears unjustified in the general case).
(define-basic-viewpoint keysig (events)
  (amuse:key-signature-sharps 
   (car (amuse:get-applicable-key-signatures 
         (last-element events)
         nil))))

;; NB: By default, amuse returns midi mode indicators: i.e., 0 =
;; major; 1 = minor. The viewpoints code has a more general
;; representation that is intended to cover the church modes as
;; well: 0 = major; 9 = minor reflecting the fact that the minor
;; mode corresponds to rotation of the pitch class set corresponding
;; to its relative major scale by 9 semitones (see Balzano, 1982). 
;;
;; Allows a crude representation of the simpler common modes: Dorian
;; is 2, Lydian 5, etc
(define-basic-viewpoint mode (events)
  (let ((midi-mode (amuse:key-signature-mode 
                    (car (amuse:get-applicable-key-signatures 
                          (last-element events)
                          nil)))))
    (case midi-mode 
      (0 0)
      (1 9))))

;; Currently applicable tempo in bpm
(define-basic-viewpoint tempo (events)
  (amuse:bpm 
   (car (amuse:get-applicable-tempi
         (last-element events)
         nil))))

;; Beats/pulses/tactus units in a bar (uses amuse:beat-units-per-bar
;; on the applicable time signature - this may give interesting
;; results for unusual cases).
(define-basic-viewpoint pulses (events)
  (amuse:beat-units-per-bar
   (car (amuse:get-applicable-time-signatures
         (last-element events)
         nil))))

;; Basic time units (ticks) in a bar, based on the timebase
;; (N.B. Viewpoints export information to a database and so avoid the
;; arbitrary fractions used in amuse as a whole)
(define-basic-viewpoint barlength (events)
  (let* ((last-element (last-element events))
         (timesig (car (amuse:get-applicable-time-signatures last-element nil)))
         (timebase (* (amuse:duration (amuse:crotchet last-element)) 4)))
    (/ (* (amuse:beat-units-per-bar timesig) timebase)
       (amuse:beat-units timesig))))

;; Gap between last note and its predecessor (returns 0 for first
;; note).
(define-basic-viewpoint deltast (events)
  (if (slot-exists-p (car (last events)) 'amuse-mtp::deltast)
      ;; Special case for MTP-EVENT
      (amuse-mtp::%mtp-deltast (car (last events)))
      (let* ((last-element (last-element events))
             (penultimate-element (penultimate-element events)))
        (cond ((and last-element penultimate-element)
               (- (amuse:timepoint last-element)
                  (+ (amuse:timepoint penultimate-element)
                     (amuse:duration penultimate-element))))
              ((and last-element (null penultimate-element))
               0)
              (t nil)))))

;; Inter Onset Interval between ultimate and penultimate onsets
;; (returns 0 for first note).
(define-basic-viewpoint bioi (events)
  ;; BIOI = Basic IOI (IOI as a basic feature) 
  (if (slot-exists-p (car (last events)) 'amuse-mtp::bioi)
      ;; Special case for MTP-EVENT
      (amuse-mtp::%mtp-bioi (car (last events)))
      (let* ((last-element (last-element events))
             (penultimate-element (penultimate-element events)))
        (cond ((and last-element penultimate-element)
               (- (amuse:timepoint last-element)
                  (amuse:timepoint penultimate-element)))
              ((and last-element (null penultimate-element))
               0)
              (t nil)))))

;; 1 if event begins a phrase, -1 if it ends a phrase. Otherwise 0.
(define-basic-viewpoint phrase (events) 
  (let ((after  (amuse-segmentation:ground-truth-segmenter-after (car events)))
        (before (amuse-segmentation:ground-truth-segmenter-before (car events)))
        (last-element (last-element events)))
    (cond ((= 1 (amuse-segmentation:boundary-strength after last-element nil))
           1)
          ((= 1 (amuse-segmentation:boundary-strength before last-element nil))
           -1)
          (t 0))))

;; Meredith's morphetic pitch - count of name-notes (white notes) up
;; or down from middle C = 35 (N.B. This number itself is 12 greater
;; than the one used for the same purpose by AMuSE. I don't remember
;; why or which DM himself uses - DL)
(define-basic-viewpoint mpitch (events) 
  (let ((result nil))
    (handler-case 
        (setf result 
              (+ (amuse:diatonic-pitch-mp
                  (last-element events))
                 12))
      (error () (setf result viewpoints:+undefined+)))
    result))

;; Inflection of name note, so 0 for a natural, 1 for a single sharp,
;; 2 for a double sharp, -1 for a flat and so on. Uses
;; amuse:diatonic-pitch-accidental
(define-basic-viewpoint accidental (events)
  (let ((result nil))
    (handler-case 
        (setf result (amuse:diatonic-pitch-accidental 
                      (amuse:diatonic-pitch (last-element events))))
      (error () (setf result viewpoints:+undefined+)))
    result))

;; Dynamics: ppppp = -11; pppp = -9; ppp = -7; pp = -5; p = -3; mp =
;; -1; mf = 1; f = 3; ff = 5; fff = 7; ffff = 9; fffff = 11
(define-basic-viewpoint dyn (events)
  ;; TODO: we need an amuse interface to dynamics to make this general
  (let ((dyn (amuse-mtp::%mtp-dyn (car (last events)))))
    (if dyn
        dyn        
        viewpoints:+undefined+)))

;; Note ornaments (0 = no ornament; 1 = accacciatura; 2 = mordent; 3 =
;; trill
(define-basic-viewpoint ornament (events)
  ;; TODO: we need an amuse interface to ornaments to make this general
  (let ((x (amuse-mtp::%mtp-ornament (car (last events)))))
    (if x x viewpoints:+undefined+)))

;; Voice number in a score (voice 0 assumed to be the monody
(define-basic-viewpoint voice (events)
  ;; TODO: we need an amuse interface to voicing to make this general
  (let ((x (amuse-mtp::%mtp-voice (car (last events)))))
    (if x x viewpoints:+undefined+)))

;; 0 = no comma; 1 = comma (breath mark)
(define-basic-viewpoint comma (events)
  ;; TODO: we need an amuse interface to commas to make this general
  (let ((x (amuse-mtp::%mtp-comma (car (last events)))))
    (if x x viewpoints:+undefined+)))

;; 0 = no articulation mark; 1 = staccato; 2 = staccatissimo; 3 =
;; sforzando; 4 = marcato
(define-basic-viewpoint articulation (events)
  ;; TODO: we need an amuse interface to articulations to make this general
  (let ((x (amuse-mtp::%mtp-articulation (car (last events)))))
    (if x x viewpoints:+undefined+)))


 
                    


