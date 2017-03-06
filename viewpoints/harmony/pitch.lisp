;;;; ======================================================================
;;;; File:       pitch.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-03-03 10:13:20 peter>                              
;;;; Time-stamp: <2017-03-06 11:27:06 peter>                           
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

(define-viewpoint (h-gct-horiz-root-interval derived (h-cpitch))
    ;; Chromatic interval between the root of the current chord
    ;; and the root of the previous chord. Returns +undefined+
    ;; if the root of the previous chord is undefined.
    ((events md:harmonic-sequence) element)
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((root1 (h-gct-root (list e1)))
                        (root2 (h-gct-root (list e2))))
                    (if (undefined-p root1 root2) +undefined+
                        (mod (- root2 root1) 12))))))
  
     
