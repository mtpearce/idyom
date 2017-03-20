;;;; ======================================================================
;;;; File:       pitch.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-03-03 10:13:20 peter>                              
;;;; Time-stamp: <2017-03-14 11:47:20 peter>                           
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

;;;; *** Properties of individual chords ***
;;;;  ** Properties of the whole chord **
;;;;   * Pitch content *

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
    ;; relative to local tonic
    ((events md:harmonic-sequence) element)
  :function (let ((local-tonic (local-tonic-method=3-context=long events)))
	      (if (undefined-p local-tonic)
		  +undefined+
		  (sort (mapcar #'(lambda (x) (mod (- x local-tonic) 12))
				(h-cpitch-class-set events))
			#'<))))

(define-viewpoint (h-cpc-int-from-bass derived (h-cpitch))
    ;; Pitch classes in harmonic slice expressed relative to the bass note
    ((events md:harmonic-sequence) element)
  :function (let* ((bass (h-bass-cpitch events))
		   (pitches (h-cpitch events))
		   (rel-pcs (mapcar #'(lambda (pitch)
					(mod (- pitch bass) 12))
				    pitches))
		   (non-bass (remove-if #'zerop rel-pcs))
		   (no-duplicates (remove-duplicates non-bass))
		   (sorted (sort no-duplicates #'<)))
	      sorted))

(define-viewpoint (h-cpc-int-from-gct-root derived (h-cpitch))
    ;; Pitch classes in harmonic slice expressed relative to the chord root
    ((events md:harmonic-sequence) element)
  :function (let* ((root (h-gct-root events))
		   (pitches (h-cpitch events))
		   (rel-pcs (mapcar #'(lambda (pitch)
					(mod (- pitch root) 12))
				    pitches))
		   (non-root (remove-if #'zerop rel-pcs))
		   (no-duplicates (remove-duplicates non-root))
		   (sorted (sort no-duplicates #'<)))
	      sorted))

;; We also have h-gct-base and g-gct-ext from general-chord-type.lisp

;;;;  * Properties of the bass note *

(define-viewpoint (h-bass-cpitch derived (h-cpitch))
    ;; Lowest chromatic pitch present in harmonic slice
    ((events md:harmonic-sequence) element)
  :function (apply #'min (h-cpitch events)))

(define-viewpoint (h-bass-cpc derived (h-cpitch))
    ;; Pitch class of the bass note
    ((events md:harmonic-sequence) element)
  :function (mod (h-bass-cpitch events) 12))

(define-viewpoint (h-bass-csd derived (h-cpitch))
    ;; Chromatic scale degree of the bass note
    ((events md:harmonic-sequence) element)
  :function (let ((local-tonic (local-tonic-method=3-context=long events)))
	      (if (undefined-p local-tonic)
		  +undefined+
		  (mod (- (h-bass-cpitch events)
			  local-tonic)
		       12))))

(define-viewpoint (h-bass-int-from-root derived (h-cpitch))
    ;; Pitch-class interval from the root to the bass
    ((events md:harmonic-sequence) element)
  :function (let ((bass (h-bass-cpitch events))
		  (root (h-gct-root events)))
	      (mod (- bass root) 12)))

;;;;  * Properties of the chord root *
    
;; We have h-gct-root from general-chord-type.lisp

(define-viewpoint (h-gct-root-csd derived (h-cpitch))
    ;; Chromatic scale degree of the chord root
    ((events md:harmonic-sequence) element)
  :function (let ((local-tonic (local-tonic-method=3-context=long events))
		  (root (h-gct-root events)))
	      (if (undefined-p local-tonic)
		  +undefined+
		  (mod (- root local-tonic) 12))))

;;;;  * Aspects of chord quality *

(define-viewpoint (h-hedges-chord-type derived (h-cpitch))
    ;; Chord type, after Hedges & Wiggins (2016, JNMR)
    ((events md:harmonic-sequence) element)
  :function (let ((pcset (h-cpc-int-from-gct-root events)))
	      (flet ((test-pc (pc) (member pc pcset
					   :test #'equalp)))
		(if (test-pc 4)
		    (if (test-pc 10)
			(if (test-pc 8) 'alt 7)
			(if (test-pc 9)
			    6
			    (if (test-pc 8) 'aug 'maj)))
		    (if (test-pc 3)
			(if (test-pc 10)
			    (if (test-pc 6)
				'half-dim
				'min7)
			    (if (test-pc 6)
				'dim
				(if (test-pc 8)
				    'min-sharp5
				    'min)))
			(if (test-pc 7)
			    'sus
			    'special))))))

;;;; *** Relationships between successive chords ***
;;;;  ** Distance metrics **
;;;;  ** Non-metrics **
;;;;   * Between bass notes *
;;;;   * Between chord roots *

(define-viewpoint (h-gct-root-cpcint derived (h-cpitch))
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
  

;;;========================
;;;* Supporting functions *
;;;========================

;;(defun csd (cpitch events)
;;  "Represents <cpitch> (which can be a pitch or a pitch class
;;   as a chromatic scale degree, given the local key at the
;;   end of <events>."
;;  (let 
  
