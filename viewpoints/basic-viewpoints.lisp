;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       basic-viewpoints.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@eecs.qmul.ac.uk>
;;;; Created:    <2013-01-24 15:00:00 jeremy>
;;;; Time-stamp: <2013-04-17 16:48:53 jeremy>
;;;; ======================================================================

(cl:in-package #:viewpoints)

;;; Basic Viewpoints 

;; Start time
(define-basic-viewpoint onset (events)
  (md:onset (last-element events)))

;; Cents above C-1
(define-basic-viewpoint cpitch (events)
  (md:chromatic-pitch (last-element events)))

;; Duration
(define-basic-viewpoint dur (events)
  (md:duration (last-element events)))

;;  Integer indicating the position of the key signature on a line of
;;  fifths with C major = 0. Roughly corresponds to a positive count
;;  of the number of sharps or a negative count of the number of flats
;;  (corresponds exactly if the range is limited as indicated in the
;;  table to +/-7, since this avoids double accidentals, but that
;;  limitation appears unjustified in the general case).
(define-basic-viewpoint keysig (events)
  (md:key-signature (last-element events)))

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
  (md:mode (last-element events)))

;; Currently applicable tempo in bpm
(define-basic-viewpoint tempo (events)
  (md:tempo (last-element events)))

;; Beats/pulses/tactus units in a bar (uses amuse:beat-units-per-bar
;; on the applicable time signature - this may give interesting
;; results for unusual cases).
(define-basic-viewpoint pulses (events)
  (md:pulses (last-element events)))

;; Basic time units (ticks) in a bar, based on the timebase
;; (N.B. Viewpoints export information to a database and so avoid the
;; arbitrary fractions used in amuse as a whole)
(define-basic-viewpoint barlength (events)
  (md:barlength (last-element events)))

;; Gap between last note and its predecessor (returns 0 for first
;; note).  
(define-basic-viewpoint deltast (events)
  (if (slot-exists-p (car (last events)) 'mtp-data::deltast)
      ;; Special case for MTP-EVENT
      (md:deltast (last-element events))
      ;; Otherwise calculate it 
      (let* ((last-element (last-element events))
	     (penultimate-element (penultimate-element events)))
	(cond ((and last-element penultimate-element)
	       (- (md:onset last-element)
		  (+ (md:onset  penultimate-element)
		     (md:duration penultimate-element))))
	      ((and last-element (null penultimate-element))
	       0)
	      (t nil)))))
  



;; Inter Onset Interval between ultimate and penultimate onsets
;; (returns 0 for first note).
(define-basic-viewpoint bioi (events)
  ;; BIOI = Basic IOI (IOI as a basic feature) 
  (if (slot-exists-p (car (last events)) 'mtp-data::bioi)
      ;; Special case for MTP-EVENT
      (md:bioi (car (last events)))
      (let* ((last-element (last-element events))
	     (penultimate-element (penultimate-element events)))
	(cond ((and last-element penultimate-element)
	       (- (md:onset last-element)
		  (md:onset penultimate-element)))
	      ((and last-element (null penultimate-element))
	       0)
              (t nil)))))


;; 1 if event begins a phrase, -1 if it ends a phrase. Otherwise 0.
(define-basic-viewpoint phrase (events) 
  (let ((after  (music-segment:ground-truth-segmenter-after (car events)))
        (before (music-segment:ground-truth-segmenter-before (car events)))
        (last-element (last-element events)))
    (cond ((= 1 (music-segment:boundary-strength after last-element nil))
           1)
          ((= 1 (music-segment:boundary-strength before last-element nil))
           -1)
          (t 0))))

;; Meredith's morphetic pitch - count of name-notes (white notes) up
;; or down from middle C = 35 (N.B. This number itself is 12 greater
;; than the one used for the same purpose by AMuSE. I don't remember
;; why or which DM himself uses - DL)
(define-basic-viewpoint mpitch (events) 
  (md:morphetic-pitch (last-element events)))


;; Inflection of name note, so 0 for a natural, 1 for a single sharp,
;; 2 for a double sharp, -1 for a flat and so on. Uses
;; amuse:diatonic-pitch-accidental
(define-basic-viewpoint accidental (events)
  (md:accidental (last-element events)))


;; Dynamics: ppppp = -11; pppp = -9; ppp = -7; pp = -5; p = -3; mp =
;; -1; mf = 1; f = 3; ff = 5; fff = 7; ffff = 9; fffff = 11
(define-basic-viewpoint dyn (events)
  (md:dynamics (last-element events)))


;; Note ornaments (0 = no ornament; 1 = accacciatura; 2 = mordent; 3 =
;; trill
(define-basic-viewpoint ornament (events)
  (md:ornament (last-element events)))


;; Voice number in a score (voice 0 assumed to be the monody
(define-basic-viewpoint voice (events)
  (md:voice (last-element events)))


;; 0 = no comma; 1 = comma (breath mark)
(define-basic-viewpoint comma (events)
  (md:comma (last-element events)))


;; 0 = no articulation mark; 1 = staccato; 2 = staccatissimo; 3 =
;; sforzando; 4 = marcato
(define-basic-viewpoint articulation (events)
  (md:articulation (last-element events)))



 
                    


