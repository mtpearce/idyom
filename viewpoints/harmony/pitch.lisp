;;;; ======================================================================
;;;; File:       pitch.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-03-03 10:13:20 peter>                              
;;;; Time-stamp: <2017-07-14 10:25:01 peter>                           
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

;; We also have h-gct-base and g-gct-ext from general-chord-type.lisp
;; We have h-gct-root-cpc, h-gct-root-csd,
;; and h-gct-root-cpcint from general-chord-type.lisp

(define-viewpoint (h-cpitch-identity derived (h-cpitch))
    ;; Returns t if h-cpitch is the same as the previous chord,
    ;; nil otherwise.
    ((events md:harmonic-sequence) element)
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
	      (if (or (null e1) (null e2)) +undefined+
		  (let ((p-set-1 (h-cpitch (list e1)))
			(p-set-2 (h-cpitch (list e2))))
		    (if (equalp p-set-1 p-set-2) t nil)))))

(define-viewpoint (h-cpitch-class derived (h-cpitch))
    ;; Pitches present in harmonic slice, mod 12, including duplicates
    ((events md:harmonic-sequence) element)
  :function (mapcar #'(lambda (x) (mod x 12)) (h-cpitch events)))

(define-viewpoint (h-cpitch-class-set derived (h-cpitch))
    ;; Pitches present in harmonic slice, mod 12, not including duplicates
    ((events md:harmonic-sequence) element)
  :function (sort (remove-duplicates (h-cpitch-class events) :test #'=) #'<))

(define-viewpoint (h-cpc-identity derived (h-cpitch))
    ;; Returns t if h-cpitch-class-set is the same as the previous chord,
    ;; nil otherwise.
    ((events md:harmonic-sequence) element)
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
	      (if (or (null e1) (null e2)) +undefined+
		  (let ((pc-set-1 (h-cpitch-class-set (list e1)))
			(pc-set-2 (h-cpitch-class-set (list e2))))
		    (if (equalp pc-set-1 pc-set-2) t nil)))))

(define-viewpoint (h-csd derived (h-cpitch))
    ;; Set of chromatic scale degrees present in the harmonic slice,
    ;; relative to local tonic
    ((events md:harmonic-sequence) element)
  :function (let ((local-tonic (local-tonic events)))
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
  :function (let* ((root (h-gct-root-cpc events))
		   (pitches (h-cpitch events))
		   (rel-pcs (mapcar #'(lambda (pitch)
					(mod (- pitch root) 12))
				    pitches))
		   (non-root (remove-if #'zerop rel-pcs))
		   (no-duplicates (remove-duplicates non-root))
		   (sorted (sort no-duplicates #'<)))
	      sorted))

(define-viewpoint (h-bass-cpitch derived (h-cpitch))
    ;; Lowest chromatic pitch present in harmonic slice
    ((events md:harmonic-sequence) element)
  :function (apply #'min (h-cpitch events)))

(define-viewpoint (h-bass-cpint derived (h-cpitch))
    ;; Chromatic interval between the bass pitch of the current chord
    ;; and the bass pitch of the previous chord. Returns +undefined+
    ;; if the bass pitch of the previous chord is undefined.
    ((events md:harmonic-sequence) element)
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((bass1 (h-bass-cpitch (list e1)))
                        (bass2 (h-bass-cpitch (list e2))))
                    (if (undefined-p bass1 bass2) +undefined+
                        (- bass2 bass1))))))

(define-viewpoint (h-bass-cpc derived (h-cpitch))
    ;; Pitch class of the bass note
    ((events md:harmonic-sequence) element)
  :function (mod (h-bass-cpitch events) 12))

(define-viewpoint (h-bass-cpcint derived (h-cpitch))
    ;; Chromatic interval between the bass pitch class of the current chord
    ;; and the bass pitch class of the previous chord, modulo 12.
    ;; Returns +undefined+ if the bass pitch class of the previous
    ;; chord is undefined.
    ((events md:harmonic-sequence) element)
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((bass1 (h-bass-cpc (list e1)))
                        (bass2 (h-bass-cpc (list e2))))
                    (if (undefined-p bass1 bass2) +undefined+
                        (mod (- bass2 bass1) 12))))))

(define-viewpoint (h-bass-csd derived (h-cpitch))
    ;; Chromatic scale degree of the bass note
    ((events md:harmonic-sequence) element)
  :function (let ((local-tonic (local-tonic events)))
	      (if (undefined-p local-tonic)
		  +undefined+
		  (mod (- (h-bass-cpitch events)
			  local-tonic)
		       12))))

(define-viewpoint (h-bass-int-from-gct-root derived (h-cpitch))
    ;; Pitch-class interval from the GCT root to the bass
    ((events md:harmonic-sequence) element)
  :function (let ((bass (h-bass-cpc events))
		  (root (h-gct-root-cpc events)))
	      (mod (- bass root) 12)))

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

(define-viewpoint (h-gct-3rd-type derived (h-cpitch))
    ;; Third type, after Hedges & Wiggins (2016, JNMR)
    ((events md:harmonic-sequence) element)
  :function (let ((pcset (h-cpc-int-from-gct-root events)))
	      (flet ((test-pc (pc) (member pc pcset
					   :test #'equalp)))
		(let ((3-present (test-pc 3))
		      (4-present (test-pc 4)))
		  (cond
		    ((and 4-present (not 3-present)) 'maj)
		    ((and 3-present (not 4-present)) 'min)
		    (t 'unclear))))))

(define-viewpoint (h-gct-7th-type derived (h-cpitch))
    ;; Seventh type, after Hedges & Wiggins (2016, JNMR)
    ((events md:harmonic-sequence) element)
  :function (let ((pcset (h-cpc-int-from-gct-root events)))
	      (flet ((test-pc (pc) (member pc pcset
					   :test #'equalp)))
		(if (test-pc 10)
		    'min-7-present 'min-7-absent))))

(define-viewpoint (h-gct-root-5ths-dist derived (h-cpitch))
    ;; Distance between successive chord roots along
    ;; the circle of fifths, after Hedges & Wiggins
    ;; (2016, JNMR)
    ((events md:harmonic-sequence) element)
  :function
  (let ((int (h-gct-root-cpcint events)))
    (if (undefined-p int)
	+undefined+
	(progn
	  (assert (integerp int))
	  (assert (>= int 0))
	  (assert (<= int 12))
	  (case int
	    (0 0)
	    (7 1) (5 1)
	    (2 2) (10 2)
	    (9 3) (3 3)
	    (4 4) (8 4)
	    (11 5) (1 5)
	    (6 6)
	    (otherwise
	     (error
	      (format
	       nil
	       "Invalid h-gct-root-cpcint value: ~A" int))))))))

(define-viewpoint (h-gct-meeus-int derived (h-cpitch))
    ;; Categories of root progressions,
    ;; after Hedges & Wiggins (2016, JNMR)
    ((events md:harmonic-sequence) element)
  :function
  (let ((int (h-gct-root-cpcint events)))
    (if (undefined-p int)
	+undefined+
	(progn
	  (assert (integerp int))
	  (assert (>= int 0))
	  (assert (<= int 12))
	  (cond
	    ((member int '(1 2 5 8 9)) 'dominant)
	    ((member int '(3 4 7 10 11)) 'subdominant)
	    ((equal int 0) 'static)
	    ((equal int 6) 'tritone)
	    (t (error
		(format
		 nil
		 "Invalid h-gct-root-cpcint value: ~A"
		 int))))))))

(define-viewpoint (h-hash-12 derived (h-cpitch))
  ;; Hashes h-cpitch into 12 random bins (used as a benchmark
  ;; to show performance for an uninformed representation
  ;; with an alphabet size of 12
    ((events md:harmonic-sequence) element)
  :function (mod (sxhash (h-cpitch events)) 12))
  
