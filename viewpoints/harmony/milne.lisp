;;;; ======================================================================
;;;; File:       milne.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-04-12 11:41:38 peter>                      
;;;; Time-stamp: <2017-05-11 19:31:11 peter>                          
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; This code defines a derived viewpoint corresponding to the
;;;; spectral pitch-class similarity model of Milne et al. (2011, 2015, 2016).
;;;;
;;;; Currently assumes integer pitch classes, and will throw an assertion failure
;;;; if this assumption is violated.
;;;;
;;;; References:
;;;;
;;;; Milne, A. J., Sethares, W. A., Laney, R., & Sharp, D. B. (2011).
;;;; Modelling the similarity of pitch collections with expectation tensors.
;;;; Journal of Mathematics and Music, 5(1), 1–20.
;;;; https://doi.org/10.1080/17459737.2011.573678
;;;;
;;;; Milne, A., Laney, R., & Sharp, D. (2015). A spectral pitch class model
;;;; of the probe tone data and scalic tonality. Music Perception, 32(4),
;;;; 364–393. https://doi.org/10.1525/MP.2015.32.4.364
;;;;
;;;; Milne, A. J., Laney, R., & Sharp, D. B. (2016). Testing a spectral
;;;; model of tonal affinity with microtonal melodies and inharmonic spectra.
;;;; Musicae Scientiae, 20(4), 465–494. https://doi.org/10.1177/1029864915622682


(cl:in-package #:viewpoints)

;;;======================
;;;* Global parameters *
;;;======================

(defparameter *milne-array-dim* 1200)   ; dimension of the expectation arrays
(defparameter *milne-sigma* 6.83)       ; SD of the smoothing Gaussian
(defparameter *milne-rho* 0.75)         ; roll-off for harmonic salience
(defparameter *milne-num-harmonics* 11) ; number of harmonics in each complex tone
                                        ; (excluding the fundamental)

;; Note that sigma is expressed in units of <array-dim>.

;;;======================
;;;* Derived viewpoints *
;;;======================

(define-viewpoint (h-cpc-milne-sd-cont=min derived (h-cpitch))
    ;; Spectral pitch-class distance between the current harmonic
    ;; slice and the previous harmonic slice, computed according
    ;; to Milne et al. (2011, 2015, 2016).
    ((events md:harmonic-sequence) element)
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
	      (if (or (null e1) (null e2)) +undefined+
		  (let ((pc-set-1 (h-cpitch-class-set (list e1)))
			(pc-set-2 (h-cpitch-class-set (list e2))))
		    (pc-set-spectral-distance pc-set-1 pc-set-2))))
  :continuous t)
	

;;;========================
;;;* Supporting functions *
;;;========================

(defun gaussian-pdf (x mean sd)
  (/ (exp (* -1 (/ (expt (- x mean) 2) (* 2 (expt sd 2)))))
     (sqrt (* 2 pi (expt sd 2)))))

(defun make-gaussian-spectral-template
    (array-dim sigma &key (truncation-point 12))
  "Makes a Gaussian spectral template with unit
mass, centred on 0, with standard deviation <sigma>.
The template will be truncated to zero for points
<truncation-point> standard deviations or further
away from the mean, after Milne's implementation."
  (assert (integerp array-dim))
  (assert (>= array-dim 3))
  (assert (numberp sigma))
  (assert (integerp truncation-point))
  (assert (>= sigma 0))
  (assert (> truncation-point 0))
  (let ((limit (floor (* 12 sigma)))
	(template (make-array array-dim :initial-element 0)))
    (assert (> limit 0))
    (assert (< limit (floor (/ array-dim 2))))
    (setf (svref template 0)
	  (gaussian-pdf 0 0 sigma))
    (loop
       for i from 1 to limit
       for j downfrom (1- array-dim) downto (- array-dim limit)
       do (let ((salience (gaussian-pdf i 0 sigma)))
	    (setf (svref template i) salience
		  (svref template j) salience)))
    template))

(defun make-gaussian-spectrum
    (array-dim mean mass sigma)
  "Makes a Gaussian spectrum with standard deviation <sigma>,
mean <mean>, and mass <mass>. See make-gaussian-spectral-template
for definition of <truncation-point>."
  (assert (integerp array-dim))
  (assert (> array-dim 0))
  (assert (>= mean 0))
  (assert (<= mean array-dim))
  (let* ((origin (round mean))
	 (template (make-gaussian-spectral-template
		    array-dim sigma))
	 (scaled (map 'vector
		      #'(lambda (x) (* x mass))
		      template))
	 (output (make-array array-dim :initial-element nil)))
    (loop for i from 0 to (1- array-dim)
       do (setf (svref output i)
		(svref scaled (mod (- i origin) array-dim))))
    output))

(defun make-complex-tone
    (fundamental-pc &key array-dim num-harmonics rho sigma)
  "Makes a pitch-class spectrum describing the
salience of each pitch class bin when played a complex tone 
with pitch class <fundamental-pc> (where <fundamental-pc>
is a number between 0 and 11), expanded to a harmonic
series containing <num-harmonics> harmonics with 
salience roll-off parameter <rho>, with each frequency
component being smoothed by a wrapped Gaussian with standard
deviation <sigma>. The output is a simple vector of length
<array-dim> corresponding to <array-dim> samples along the
pitch-class scale from C to C. Sample i corresponds
to the salience of the pitch class bin situated at
i / <array-dim> octaves along the pitch-class
continuum."
  (assert (numberp fundamental-pc))
  (assert (<= 0 fundamental-pc))
  (assert (< fundamental-pc 12))
  (assert (integerp array-dim))
  (assert (> array-dim 0))
  (assert (numberp num-harmonics))
  (assert (>= num-harmonics 0))
  (assert (numberp rho))
  (assert (numberp sigma))
  (assert (>= sigma 0))
  (let* ((pcs (loop for i from 1 to (1+ num-harmonics)
		 collect (mod (+ (* (/ array-dim 12) fundamental-pc)
				 (* array-dim (log i 2)))
			      array-dim)))
	 (saliences (loop for i from 1 to (1+ num-harmonics)
		       collect (/ 1 (expt i rho))))
	 (spectra (mapcar #'(lambda (pc salience)
			      (make-gaussian-spectrum
			       array-dim pc salience
			       sigma))
			  pcs saliences))
	 (spectrum (reduce #'(lambda (s1 s2)
			       (map 'vector #'+ s1 s2))
			   spectra)))
    spectrum))

(let ((pc-spectral-templates
       (loop for i from 0 to 11
	  collect (make-complex-tone
		   i
		   :array-dim *milne-array-dim*
		   :num-harmonics *milne-num-harmonics*
		   :rho *milne-rho*
		   :sigma *milne-sigma*))))
  (defun get-complex-tone (fundamental-pc)
    "A wrapper for <make-complex-tone> that caches the
pitch-class spectra to avoid repeated computation."
    (assert (numberp fundamental-pc))
    (let ((pc (round fundamental-pc)))
      (assert (= fundamental-pc pc))
      (assert (<= 0 pc))
      (assert (< pc 12))
      (nth pc pc-spectral-templates))))

(defun pc-set->pc-spectrum (pc-set)
  "Takes a list of pitch classes and outputs
a pitch-class spectrum obtained by treating each
of the pitch classes as complex tones."
  (assert (listp pc-set))
  (assert (every #'numberp pc-set))
  (assert (every #'(lambda (x) (= x (round x)))
			 pc-set))
  (let* ((pc-set (mapcar #'round pc-set))
	 (pc-set (remove-duplicates pc-set))
	 (pc-spectra (mapcar #'get-complex-tone pc-set))
	 (pc-spectrum (reduce #'(lambda (s1 s2)
				  (map 'vector #'+ s1 s2))
			      pc-spectra)))
    pc-spectrum))

(defun cosine-similarity (x y)
  "Calculates the cosine similarity between two
vectors <x> and <y>."
  (assert (vectorp x))
  (assert (vectorp y))
  (assert (eql (length x) (length y)))
  (let ((numerator (loop for i across x
		      for j across y
		      summing (* i j)))
	(denominator (* (sqrt (loop for i across x summing (* i i)))
			(sqrt (loop for j across y summing (* j j))))))
    (/ numerator denominator)))

(declaim (notinline pc-set-spectral-distance))
(fmemo:define-memo-function pc-set-spectral-distance
    (pc-set-1 pc-set-2)
  "Calculates the spectral distance between two pitch
class sets using Milne's spectral distance model."
  (assert (listp pc-set-1))
  (assert (listp pc-set-2))
  (assert (every #'numberp pc-set-1))
  (assert (every #'numberp pc-set-2))
  (assert (every #'(lambda (x) (= x (round x)))
			 pc-set-1))
  (assert (every #'(lambda (x) (= x (round x)))
			 pc-set-2))
  (- 1 (cosine-similarity (pc-set->pc-spectrum pc-set-1)
			  (pc-set->pc-spectrum pc-set-2))))
