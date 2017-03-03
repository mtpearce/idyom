;;;; ======================================================================
;;;; File:       pitch.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-03-03 10:13:20 peter>                              
;;;; Time-stamp: <2017-03-03 15:49:56 peter>                           
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; This code defines various derived harmony viewpoints.
;;;; 

(cl:in-package #:viewpoints)

;;;======================
;;;* Derived viewpoints *
;;;======================

(define-viewpoint (h-cpitch-class derived (h-cpitch))
    ;; Pitches present in harmonic slice, mod 12, including duplicates
    ((events md:harmonic-sequence) element)
  :function (mapcar #'(lambda (x) (mod x 12)) (h-cpitch events)))

(define-viewpoint (h-cpitch-class-set derived (h-cpitch))
    ;; Pitches present in harmonic slice, mod 12, not including duplicates
    ((events md:harmonic-sequence) element)
  :function (sort (remove-duplicates (h-cpitch-class events) :test #'=) #'<))

(define-viewpoint (h-csd derived (h-cpitch))
    ;; Set of chromatic scale degrees present in the harmonic slice,
    ;; relative to local tonic.
    ((events md:harmonic-sequence) element)
  :function (let ((local-tonic (local-tonic-method=3-context=medium events)))
	      (if (undefined-p local-tonic)
		  +undefined+ 
	      (mapcar #'(lambda (x) (mod (- x local-tonic) 12))
		      (h-cpitch-class-set events)))))
		  
     
