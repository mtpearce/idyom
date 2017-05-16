;;;; ======================================================================
;;;; File:       key-finding.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-03-01 14:58:07 peter>                             
;;;; Time-stamp: <2017-05-16 23:57:53 peter>                           
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; This code defines derived viewpoints corresponding to various
;;;; key-finding algorithms. 
;;;;
;;;; Method 1: Albrecht and Shanahan's (2013) Euclidean distance algorithm
;;;; Method 2: Aarden's (2003) algorithm
;;;; Method 3: Albrecht and Shanahan's (2013) meta-algorithm
;;;;
;;;; Aarden, B. J. (2003). Dynamic melodic expectancy (Unpublished
;;;; doctoral dissertation). Ohio State University, Columbus, OH.
;;;;
;;;; Albrecht, J. & Shanahan, D. (2013). The Use of Large Corpora to
;;;; Train a New Type of Key-Finding Algorithm: An Improved Treatment
;;;; of the Minor Mode. Music Perception, 31(1), 59–67.
;;;;
;;;; Note: names are prefixed with 'kf-' to avoid name clashes.
;;;;
;;;; ======================================================================
;;;; Todo =================================================================
;;;; ======================================================================
;;;;
;;;; 1. Reference window sizes to global time base.
;;;; ======================================================================

(cl:in-package #:viewpoints)

;;;======================
;;;* User variables *
;;;======================

;; Parameters that determine the window lengths used for key finding
;; in terms of basic time units. Must be positive.
(defparameter *key-finding-short-window-size* (* 4 24))   ; 4 crotchet beats
(defparameter *key-finding-medium-window-size* (* 8 24))  ; 8 crotchet beats
(defparameter *key-finding-long-window-size* (* 16 24))   ; 16 crotchet beats

;;;===============================================
;;;* Derived viewpoints (generic implementation) *
;;;===============================================

;; Generic implementation, used by most other derived viewpoints

(define-viewpoint (local-key derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (if *use-cached-local-key*
		(let ((cached-local-tonic
		       (md:get-attribute (car (last events))
					 'cached-local-tonic))
		      (cached-local-mode
		       (md:get-attribute (car (last events))
					 'cached-local-mode)))
		  (if (or (null cached-local-tonic)
			  (null cached-local-mode))
		      (error "Couldn't find cached key information.")
		      (if (or (undefined-p cached-local-tonic)
			      (undefined-p cached-local-mode))
			  +undefined+
			  (list (cons :mode cached-local-mode)
				(cons :tonic cached-local-tonic)))))
		(let* ((key (kf-events->local-key-method-1
			     events *key-finding-long-window-size*))
		       (tonic  (if (undefined-p key)
				   +undefined+
				   (cdr (assoc :tonic key))))
		       (mode (if (undefined-p key)
				 +undefined+
				 (cdr (assoc :mode key)))))
		  (list (cons :mode mode)
			(cons :tonic tonic)))))

(define-viewpoint (local-tonic derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((key (local-key events)))
	      (if (undefined-p key)
		  +undefined+
		  (cdr (assoc :tonic key)))))

(define-viewpoint (local-mode derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((key (local-key events)))
	      (if (undefined-p key)
		  +undefined+
		  (cdr (assoc :mode key)))))

;;;===============================================
;;;* Caching derived viewpoints *
;;;===============================================

;; The local tonic key can be expensive to compute. The following
;; function caches the local key in an event sequence to avoid
;; repeated computation of the local key. Beware of situations
;; where this cache might be invalidated.

(defgeneric add-local-key-cache (sequence)
  (:documentation "Adds key information to <sequence> using the 
<local-key> viewpoint."))

(defmethod add-local-key-cache ((seq md:harmonic-sequence))
  (let ((*use-cached-local-key* nil)
	(key-list (viewpoint-sequence (get-viewpoint 'local-key) seq
				      :keep-undefined t)))
    (iterate:iterate (iterate:for event in-sequence seq)
		     (iterate:for key in key-list)
		     (let ((tonic (if (undefined-p key)
				      +undefined+
				      (cdr (assoc :tonic key))))
			   (mode (if (undefined-p key)
				     +undefined+
				     (cdr (assoc :mode key)))))
		       (md:set-attribute event 'cached-local-tonic tonic)
		       (md:set-attribute event 'cached-local-mode mode)))
    seq))



;;;===============================================
;;;* Derived viewpoints (specific algorithms) *
;;;===============================================

;; These viewpoints correspond to specific implementations of particular
;; key-finding algorithms with particular context lengths.

;;;  Key
;;   Method 1 (Albrecht and Shanahan's 2013 Euclidean distance algorithm)

(define-viewpoint (local-key-method=1-context=short derived (h-cpitch))
    ;; Estimates the local key based using method 1 and a short context.
    ((events md:harmonic-sequence) element)
  :function (kf-events->local-key-method-1
	     events *key-finding-short-window-size*))

(define-viewpoint (local-key-method=1-context=medium derived (h-cpitch))
    ;; Estimates the local key based using method 1 and a medium context.
    ((events md:harmonic-sequence) element)
  :function (kf-events->local-key-method-1
	     events *key-finding-medium-window-size*))

(define-viewpoint (local-key-method=1-context=long derived (h-cpitch))
    ;; Estimates the local key based using method 1 and a long context.
    ((events md:harmonic-sequence) element)
  :function (kf-events->local-key-method-1
	     events *key-finding-long-window-size*))

;; Method 2 (Aarden's 2003 algorithm)

(define-viewpoint (local-key-method=2-context=short derived (h-cpitch))
    ;; Estimates the local key based using method 2 and a short context.
    ((events md:harmonic-sequence) element)
  :function (kf-events->local-key-method-2
	     events *key-finding-short-window-size*))

(define-viewpoint (local-key-method=2-context=medium derived (h-cpitch))
    ;; Estimates the local key based using method 2 and a medium context.
    ((events md:harmonic-sequence) element)
  :function (kf-events->local-key-method-2
	     events *key-finding-medium-window-size*))

(define-viewpoint (local-key-method=2-context=long derived (h-cpitch))
    ;; Estimates the local key based using method 2 and a long context.
    ((events md:harmonic-sequence) element)
  :function (kf-events->local-key-method-2
	     events *key-finding-long-window-size*))

;; Method 3 (hybrid algorithm)

(define-viewpoint (local-key-method=3-context=short derived (h-cpitch))
    ;; Estimates the local key based using method 1 and a short context.
    ((events md:harmonic-sequence) element)
  :function (kf-events->local-key-method-3
	     events *key-finding-short-window-size*))

(define-viewpoint (local-key-method=3-context=medium derived (h-cpitch))
    ;; Estimates the local key based using method 1 and a medium context.
    ((events md:harmonic-sequence) element)
  :function (kf-events->local-key-method-3
	     events *key-finding-medium-window-size*))

(define-viewpoint (local-key-method=3-context=long derived (h-cpitch))
    ;; Estimates the local key based using method 1 and a long context.
    ((events md:harmonic-sequence) element)
  :function (kf-events->local-key-method-3
	     events *key-finding-long-window-size*))

;;; Tonic

;; Method 3 (hybrid algorithm)
(define-viewpoint (local-tonic-method=3-context=short derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((key (local-key-method=3-context=short
			events)))
	      (if (undefined-p key)
		  +undefined+
		  (cdr (assoc :tonic key)))))

(define-viewpoint (local-tonic-method=3-context=medium derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((key (local-key-method=3-context=medium
			events)))
	      (if (undefined-p key)
		  +undefined+
		  (cdr (assoc :tonic key)))))

(define-viewpoint (local-tonic-method=3-context=long derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((key (local-key-method=3-context=long
			events)))
	      (if (undefined-p key)
		  +undefined+
		  (cdr (assoc :tonic key)))))

;;; Mode

;; Method 3 (hybrid algorithm)
(define-viewpoint (local-mode-method=3-context=short derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((key (local-key-method=3-context=short
			events)))
	      (if (undefined-p key)
		  +undefined+
		  (cdr (assoc :mode key)))))

(define-viewpoint (local-mode-method=3-context=medium derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((key (local-key-method=3-context=medium
			events)))
	      (if (undefined-p key)
		  +undefined+
		  (cdr (assoc :mode key)))))

(define-viewpoint (local-mode-method=3-context=long derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((key (local-key-method=3-context=long
			events)))
	      (if (undefined-p key)
		  +undefined+
		  (cdr (assoc :mode key)))))


;;;================
;;;* Key profiles *
;;;================

;; Key profiles from Albrecht & Shanahan (2013).

;; Albrecht, J. & Shanahan, D. (2013). The Use of Large Corpora to
;; Train a New Type of Key-Finding Algorithm: An Improved Treatment
;; of the Minor Mode. Music Perception, 31(1), 59–67.

;; Note that the minor weightings don't quite sum to 1, for some reason.

(defparameter *kf-albrecht-scale-degree-profile-major*
  (vector 0.238 0.006 0.111 0.006 0.137 0.094
	  0.016 0.214 0.009 0.080 0.008 0.081))

(defparameter *kf-albrecht-scale-degree-profile-minor*
  (vector 0.220 0.006 0.104 0.123 0.019 0.103
	  0.012 0.214 0.062 0.022 0.061 0.052))

;; Key profiles from Aarden (2003), as given in Sapp (2011, p. 95).

;; Aarden, B. J. (2003). Dynamic melodic expectancy (Unpublished
;; doctoral dissertation). Ohio State University, Columbus, OH.

;; Sapp, C. S. (2011). Computational methods for the analysis of
;; musical structure (Unpublished doctoral dissertation).
;; Stanford University, Stanford, CA.

(defparameter *kf-aarden-scale-degree-profile-major*
  (vector 17.77 0.15 14.93 0.16 19.80 11.36
	  0.29 22.06 0.15 8.15 0.23 4.95))

(defparameter *kf-aarden-scale-degree-profile-minor*
  (vector 18.26 0.74 14.05 16.86 0.70 14.44
	  0.70 18.62 4.57 1.93 7.38 1.76))

;;;============================
;;;* Parameter pre-processing *
;;;============================

(defun kf-expand-key-profile (profile)
  "Takes a vector of scale degree weightings <profile>
   and expands it to a vector of vectors of pitch class weights,
   the ith element of which contains the pitch class weights
   for the key with pitch-class i as the tonic."
  (let ((pc-profiles (make-array 12 :initial-element nil)))
    (dotimes (tonic 12 pc-profiles)
      (setf (svref pc-profiles tonic)
	    (kf-scale-degree-profile->pc-profile profile
						 tonic)))))

(defun kf-scale-degree-profile->pc-profile (scale-degree-profile tonic)
  "Takes a vector of scale degree weightings <scale-degree-profile>
   and converts it to a vector of pitch-class weightings
   where <tonic> is the pitch-class of the tonic."
  (let ((pc-profile (make-array 12 :initial-element nil)))
    (dotimes (scale-degree 12 pc-profile)
      (let ((weight (svref scale-degree-profile scale-degree))
	    (pitch-class (mod (+ tonic scale-degree)
			      12)))
	(setf (svref pc-profile pitch-class)
	      weight)))))

;;;========================
;;;* Global parameters *
;;;========================

(defparameter *kf-albrecht-pc-profiles-major*
  (kf-expand-key-profile *kf-albrecht-scale-degree-profile-major*))

(defparameter *kf-albrecht-pc-profiles-minor*
  (kf-expand-key-profile *kf-albrecht-scale-degree-profile-minor*))

(defparameter *kf-aarden-pc-profiles-major*
  (kf-expand-key-profile *kf-aarden-scale-degree-profile-major*))

(defparameter *kf-aarden-pc-profiles-minor*
  (kf-expand-key-profile *kf-aarden-scale-degree-profile-minor*))

;;;========================
;;;* Supporting functions *
;;;========================

(defun kf-events->local-key-method-1 (events window-size
				      &optional context-pc-profile)
  "Estimates the local key using estimation method 1,
   looking at recent context in <events> determined
   by <window-size>. If <context-pc-profile> is provided,
   then <events> and <window-size> are ignored, and 
   local key is calculated directly from the 
   pitch-class profile provided in <context-pc-profile."
  (let* ((context-pc-profile (if context-pc-profile
				 context-pc-profile
				 (kf-events->context-pc-profile
				  events window-size)))
	 (key (if context-pc-profile
		  (kf-find-closest-pc-profile
		   context-pc-profile
		   *kf-albrecht-pc-profiles-major*
		   *kf-albrecht-pc-profiles-minor*
		   'euclidean)
		  +undefined+)))
    key))

(defun kf-events->local-key-method-2 (events window-size
				      &optional context-pc-profile)
  "Estimates the local key using estimation method 2,
   looking at recent context in <events> determined
   by <window-size>. If <context-pc-profile> is provided,
   then <events> and <window-size> are ignored, and 
   local key is calculated directly from the 
   pitch-class profile provided in <context-pc-profile."
  (let* ((context-pc-profile (if context-pc-profile
				 context-pc-profile
				 (kf-events->context-pc-profile
				  events window-size)))
	 (key (if context-pc-profile
		  (kf-find-closest-pc-profile
		   context-pc-profile
		   *kf-aarden-pc-profiles-major*
		   *kf-aarden-pc-profiles-minor*
		   'pearson)
		  +undefined+)))
    key))

(defun kf-events->local-key-method-3 (events window-size)
  "Estimates the local key using estimation method 3,
   looking at recent context in <events> determined
   by <window-size>. Assumes that cpitch of 0 corresponds
   to a pitch-class of 0, as usual."
  (let ((context-pc-profile (kf-events->context-pc-profile
			     events window-size)))
    (if context-pc-profile
	(let* ;; ((last-event (car (last (coerce events 'list))))
	    ((last-event (car (last events)))
	     (last-bass-note (apply #'min (mapcar #'md:chromatic-pitch
						  (coerce last-event 'list))))
	     (last-bass-pc (mod last-bass-note 12))
	     (albrecht-key (kf-events->local-key-method-1
			    nil nil context-pc-profile))
	     (aarden-key (kf-events->local-key-method-2
			  nil nil context-pc-profile))
	     (albrecht-tonic (cdr (assoc :tonic albrecht-key)))
	     (aarden-tonic (cdr (assoc :tonic aarden-key)))
	     (albrecht-mode (cdr (assoc :mode albrecht-key)))
	     (aarden-mode (cdr (assoc :mode aarden-key)))
	     (albrecht-confidence (cdr (assoc :confidence albrecht-key)))
	     (aarden-confidence (cdr (assoc :confidence aarden-key)))
	     (albrecht-output (list (cons :tonic albrecht-tonic)
	     			    (cons :mode albrecht-mode)))
	     (aarden-output (list (cons :tonic aarden-tonic)
	     			  (cons :mode aarden-mode))))
	  (cond ((and (eql albrecht-tonic aarden-tonic)
		      (eql albrecht-mode aarden-mode))
		 albrecht-output)
		((>= albrecht-confidence 0.25) albrecht-output)
		((>= aarden-confidence 0.25) aarden-output)
		((eql albrecht-tonic last-bass-pc) albrecht-output)
		((eql aarden-tonic last-bass-pc) aarden-output)
		((> albrecht-confidence aarden-confidence) albrecht-output)
		(t aarden-output)))
	+undefined+)))

(defun kf-find-closest-pc-profile (test-profile
				   major-reference-profiles
				   minor-reference-profiles
				   method)
  "Finds the closest matching profile in <reference-profiles>
   to <test-profile> using congruence method <method>.
   <test-profile> should be a vector of pitch class weights,
   with the ith element corresponding to the weight for pitch
   class i.
   <reference-profiles> should be a vector of reference pitch
   class profiles, the ith element of which contains a vector of 
   pitch class weights for the key with pitch-class i as the tonic.
   <method> can be either 'euclidean, for euclidean distance
   metric, or 'pearson, for Pearson correlation coefficient."
  (let* ((major-match-scores (kf-compute-match-scores
			      test-profile
			      major-reference-profiles
			      method))
	 (minor-match-scores (kf-compute-match-scores
			      test-profile
			      minor-reference-profiles
			      method))
	 (best-match nil)
	 (second-best-match nil))
    (dolist (i (list (cons 'major major-match-scores)
		     (cons 'minor minor-match-scores)))
      (let ((label (car i))
	    (match-scores (cdr i)))
	(dotimes (tonic-pc 12)
	  (let* ((score (svref match-scores tonic-pc))
		 (token (list score label tonic-pc)))
	    (cond
	      ((null best-match) (setf best-match token))
	      ((kf-compare-match-scores score (car best-match) method)
	       (progn
		 (setf second-best-match best-match)
		 (setf best-match token)))
	      ((null second-best-match) (setf second-best-match token))
	      ((kf-compare-match-scores score (car second-best-match) method)
	       (setf second-best-match token))
	      (t nil))))))
    (let* ((best-score (first best-match))
	   (second-best-score (first second-best-match))
	   (mode (second best-match))
	   (tonic (third best-match))
	   (confidence (kf-compute-confidence best-score
					      second-best-score
					      method)))
      (list (cons :mode mode)
	    (cons :tonic tonic)
	    (cons :confidence confidence)))))

(defun kf-compare-match-scores (x y method)
  "Compares two congruence scores <x> and <y> under
         method <method> and returns T if <x> denotes better
         fit than <y>, nil otherwise."
  (cond
    ((eql method 'euclidean) (< x y)) ; smaller distances indicate better fit
    ((eql method 'pearson) (> x y))   ; larger correlations indicate better fit
    (t (error "Unknown <method> specified."))))

(defun kf-compute-match-scores
    (test-profile reference-profiles method)
  "Returns a vector of match scores from comparing <test-profile>
              with <reference-profiles>."
  (let ((match-scores (make-array 12 :initial-element nil)))
    (dotimes (tonic-pc 12 match-scores)
      (setf (svref match-scores tonic-pc)
	    (kf-compute-congruence test-profile
				   (svref reference-profiles tonic-pc)
				   method)))))

(defun kf-compute-confidence (best-score second-best-score method)
  "Gets a confidence assessment for the key assignment.
              <best-match> and <second-best-match> should take 
              the form of lists where the first element is 
              the score delivered by the congruence measure."
  (cond ((eql method 'euclidean)
	 (- 1 (/ best-score second-best-score)))
	((eql method 'pearson)
	 (* 3 (- best-score second-best-score)))
	(t (error "Unknown <method> specified."))))


(defun kf-events->context-pc-profile (events window-size)
  "Calculates the sum duration that each pitch class sounds for 
   in the list of events <events> within the window of 
   <window-size> basic time units from the onset of the 
   last event in <events>, with duration normalised so that
   the durations sum to 1. Returns nil if no pitch classes
   were found.  Non-integer pitch classes are rounded
   to the nearest pitch class. Assumes that a cpitch of 0 
   corresponds to a pitch class of 0 (as is conventional 
   if cpitch corresponds to MIDI note number)."
  (let* ((window-close (onset events))
	 (window-open (- window-close window-size))
	 (context (remove-if #'(lambda (x)
				 (< (md:onset (md:end-time x)) window-open))
			     events))
	 (pc-durs (make-array 12 :initial-element 0)))
    ;; (dolist (e (coerce context 'list))
    (dolist (e context)
      ;;  (dolist (e (md::%list-slot-sequence-data context) pc-durs)
      (let* ((note-on (max window-open (md:onset e)))
	     (note-off (min window-close (md:onset (md:end-time e))))
	     (dur (- note-off note-on))
	     (cpitches (md:get-attribute e 'md:h-cpitch))
	     (pitch-classes (mapcar #'(lambda (x) (mod (round x) 12))
				    cpitches)))
	(assert (>= dur 0))
	(dolist (pc pitch-classes) 
	  (incf (svref pc-durs pc) dur))))
    (if (every #'zerop pc-durs)
	nil
	(normalise-to-unit-sum pc-durs))))

(defun kf-compute-congruence (x y method)
  "Computes the congurence between two sequences
   <x> and <y> using <method>, which can either be 
   'euclidean for Euclidean distance or 'pearson
   for Pearson correlation."
  (cond 
    ((eql method 'euclidean) (euclidean-distance x y))
    ((eql method 'pearson) (pearson-correlation x y))
    (t (error "Unknown <method> specified."))))

(defun normalise-to-unit-sum (vec)
  "Linearly scales <vec> so that the elements sum to 1."
  (let ((sum (reduce #'+ vec)))
    (map 'vector #'(lambda (x) (/ x sum)) vec)))

(defun euclidean-distance (x y)
  "Computes the Euclidean distance between two numeric sequences."
  (assert (eql (length x) (length y)))
  (sqrt (reduce #'+ (map 'vector
			 #'(lambda (a b) (expt (- a b) 2.0))
			 x y))))

(defun pearson-correlation (x y)
  "Computes the Pearson correlation coefficient for two numeric sequences."
  (assert (eql (length x) (length y)))
  (let* ((n (length x))
	 (s-xy (reduce #'+ (map 'vector #'(lambda (a b) (* a b)) x y)))
	 (x-bar (/ (reduce #'+ x) n))
	 (y-bar (/ (reduce #'+ y) n))
	 (s-x2 (reduce #'+ (map 'vector #'(lambda (x) (expt x 2)) x)))
	 (s-y2 (reduce #'+ (map 'vector #'(lambda (y) (expt y 2)) y)))
	 (numerator (- s-xy (* n x-bar y-bar)))
	 (denominator (* (sqrt (- s-x2 (* n (expt x-bar 2))))
			 (sqrt (- s-y2 (* n (expt y-bar 2)))))))
    (/ numerator denominator)))



