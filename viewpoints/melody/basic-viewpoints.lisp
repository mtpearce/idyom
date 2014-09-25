;;;; ======================================================================
;;;; File:       basic-viewpoints.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2005-11-29 10:41:20 marcusp>
;;;; Time-stamp: <2014-09-18 13:38:16 marcusp>
;;;; ======================================================================

(cl:in-package #:viewpoints)

;;; Basic Viewpoints 

;; Start time
(define-basic-viewpoint onset ((events md:melodic-sequence))
  (md:onset (last-element events)))

;; Pitch as a midi note number
(define-basic-viewpoint cpitch ((events md:melodic-sequence))
  (md:chromatic-pitch (last-element events)))

;; Duration
(define-basic-viewpoint dur ((events md:melodic-sequence))
  (md:duration (last-element events)))

;;  Integer indicating the position of the key signature on a line of
;;  fifths with C major = 0. Corresponds to a positive count of the
;;  number of sharps or a negative count of the number of flats (as
;;  long as the range is limited to +/-7, avoiding double
;;  accidentals).
(define-basic-viewpoint keysig ((events md:melodic-sequence))
  (md:key-signature (last-element events)))

;; This reflects the prevailing mode using a general representation
;; that is intended to cover the church modes as well as
;; major/minor. 0 = major; 9 = minor reflecting the fact that the
;; minor mode corresponds to rotation of the pitch class set
;; corresponding to its relative major scale by 9 semitones (see
;; Balzano, 1982). This allows a crude representation of the simpler
;; common modes: Dorian is 2, Lydian 5, etc
(define-basic-viewpoint mode ((events md:melodic-sequence))
  (md:mode (last-element events)))

;; Current tempo in bpm
(define-basic-viewpoint tempo ((events md:melodic-sequence))
  (md:tempo (last-element events)))

;; Beats/pulses/tactus units in a bar 
(define-basic-viewpoint pulses ((events md:melodic-sequence))
  (md:pulses (last-element events)))

;; Basic time units (ticks) in a bar, based on the timebase
(define-basic-viewpoint barlength ((events md:melodic-sequence))
  (md:barlength (last-element events)))

;; Gap between last note and its predecessor (returns 0 for first note).  
(define-basic-viewpoint deltast ((events md:melodic-sequence))
  (if (slot-exists-p (car (last events)) 'music-data:deltast) 
      ;; if there is a deltast slot, then return the value
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
;; (returns interval from 0 to onset for first note). BIOI = Basic IOI
;; (IOI as a basic feature)
(define-basic-viewpoint bioi ((events md:melodic-sequence))
  (if (slot-exists-p (car (last events)) 'music-data::bioi) 
      ;; if BIOI slot exists return the value
      (md:bioi (car (last events)))
      ;; else calculate
      (let* ((last-element (last-element events))
	     (penultimate-element (penultimate-element events)))
	(cond ((and last-element penultimate-element)
	       (- (md:onset last-element)
		  (md:onset penultimate-element)))
	      ((and last-element (null penultimate-element))
	       0)
              (t nil)))))

;; 1 if event begins a phrase, -1 if it ends a phrase. Otherwise 0.
(define-basic-viewpoint phrase ((events md:melodic-sequence)) 
  (md:phrase (last-element events)))

;; David Meredith's morphetic pitch: count of name-notes (white notes)
;; up or down from middle C = 35 
(define-basic-viewpoint mpitch ((events md:melodic-sequence)) 
  (md:morphetic-pitch (last-element events)))

;; Inflection of note name, so 0 for a natural, 1 for a single sharp,
;; 2 for a double sharp, -1 for a flat and so on.
(define-basic-viewpoint accidental ((events md:melodic-sequence))
  (md:accidental (last-element events)))

;; Dynamics: ppppp = -11; pppp = -9; ppp = -7; pp = -5; p = -3; mp =
;; -1; mf = 1; f = 3; ff = 5; fff = 7; ffff = 9; fffff = 11
(define-basic-viewpoint dyn ((events md:melodic-sequence))
  (md:dynamics (last-element events)))

;; Note ornaments (0 = no ornament; 1 = accacciatura; 2 = mordent; 3 =
;; trill
(define-basic-viewpoint ornament ((events md:melodic-sequence))
  (md:ornament (last-element events)))

;; Voice number in a score
(define-basic-viewpoint voice ((events md:melodic-sequence))
  (md:voice (last-element events)))

;; 0 = no comma; 1 = comma (breath mark)
(define-basic-viewpoint comma ((events md:melodic-sequence))
  (md:comma (last-element events)))

;; 0 = no articulation mark; 1 = staccato; 2 = staccatissimo; 3 =
;; sforzando; 4 = marcato
(define-basic-viewpoint articulation ((events md:melodic-sequence))
  (md:articulation (last-element events)))
