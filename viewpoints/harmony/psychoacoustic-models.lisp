;;;; ======================================================================
;;;; File:       psychoacoustic-models.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-04-10 16:20:11 peter>                       
;;;; Time-stamp: <2017-04-11 14:36:30 peter>                           
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; This code defines derived viewpoints corresponding to implementations
;;;; of various psychoacoustic models.
;;;;
;;;; Unless otherwise specified, the code should allow both integer
;;;; and non-integer pitch values as inputs.
;;;;
;;;; References:
;;;;
;;;; Hutchinson, W., & Knopoff, L. (1979). The significance of the acoustic
;;;; component of consonance in Western triads. Journal of Musicological
;;;; Research, 3(1–2), 5–22. https://doi.org/10.1080/01411897908574504
;;;;
;;;; Hutchinson, W., & Knopoff, L. (1978). The acoustic component of
;;;; western consonance. Journal of New Music Research, 7(1), 1–29.
;;;; https://doi.org/10.1080/09298217808570246
;;;;
;;;; Mashinter, K. (2006). Calculating sensory dissonance: Some
;;;; discrepancies arising from the models of Kameoka & Kuriyagawa, and
;;;; Hutchinson & Knopoff. Empirical Musicology Review, 1(2), 65–84.

(cl:in-package #:viewpoints)

;;;======================
;;;* Derived viewpoints *
;;;======================

(define-viewpoint (h-hutch-rough derived (h-cpitch))
    ;; Roughness as computed by the models of Hutchinson & Knopoff
    ;; (1978, 1979) as implemented by Mashinter (2006).
    ;; Assumes equal amplitude for each pitch in the chord.
    ((events md:harmonic-sequence) element)
  :function (let* ((cpitch-list (h-cpitch events))
		   (freq-list (mapcar #'midi->freq cpitch-list)))
	      (assert (apply #'< freq-list))
	      (assert (every #'numberp freq-list))
	      (assert (every #'(lambda (x) (> x 0)) freq-list))
	      (hutch-d (freq-list->harmonics
			freq-list :num-harmonics 10)
		       :cbw-cut-off nil)))



;;;========================
;;;* Supporting functions *
;;;========================

(defun midi->freq (midi)
  "Converts a MIDI note number to a frequency."
  (* 440
     (expt 2
	   (/ (- midi 69)
	      12))))

(defun hutch-cbw (f1 f2)
  "Calculates the critical bandwidth given two frequencies
<f1> and <f2>, according to Hutchinson and Knopoff's model."
  (assert (numberp f1))
  (assert (numberp f2))
  (assert (> f1 0))
  (assert (> f2 0))
  (let ((mean-freq (/ (+ f1 f2) 2)))
    (* 1.72 (expt mean-freq 0.65))))

(defun hutch-y (f1 f2)
  "Calculates the distance between frequencies
<f1> and <f2> in units of critical bandwidths."
  (assert (numberp f1))
  (assert (numberp f2))
  (assert (> f1 0))
  (assert (> f2 0))
  (let ((abs-freq-diff (abs (- f1 f2)))
	(cbw (hutch-cbw f1 f2)))
    (/ abs-freq-diff cbw)))

(defun hutch-g (y &optional cbw-cut-off)
  "Calculates the dissonance factor given a frequency 
distance in units of critical bandwitchs <y>, after 
Mashinter (2006). If <cbw-cut-off> is not nil,
then it should be a number corresponding to the
variable CBWcutoff in Mashinter's own implementation.
If y >= <cbw-cut-off>, then the dissonance factor will
be approximated as 0. Setting <cbw-cut-off> to 1.2
is necessary for replicating Mashinter's results."
  (assert (numberp y))
  (assert (>= y 0))
  (if (or (null cbw-cut-off) (< y cbw-cut-off))
      (let ((a 0.25) (b 2))
	(expt (* (/ y a) (exp (- 1 (/ y a))))
	      b))
      0.0))

(defun hutch-d (s &key cbw-cut-off)
  "Calculates the overall dissonance in a spectrum <s>,
after Hutchinson and Knopoff. <s> should be a list of lists where
the inner lists have two elements, the first element of which is the frequency
of the corresponding pure tone, and the second element is the amplitude of this
pure tone."
  (assert (listp s))
  (let ((n (length s)))
    (if (< n 2)
	0
	(let* ((freqs (coerce (mapcar #'car s) 'simple-vector))
	       (amps (coerce (mapcar #'cadr s) 'simple-vector))
	       (denom (loop for i across amps summing (expt i 2)))
	       (num (loop for i from 1 to n summing
			 (loop for j from (1+ i) to n summing
			      (* (svref amps (- i 1))
				 (svref amps (- j 1))
				 (hutch-g (hutch-y (svref freqs (- i 1))
						   (svref freqs (- j 1)))
					  cbw-cut-off))))))
	  (if (equalp denom 0)
	      0
	      (/ num denom))))))

(defun freq-list->harmonics (freq-list &key (num-harmonics 10)
				       (roll-off #'(lambda (n) (/ 1 (1+ n))))
				       coherent)
  "Takes a list of frequencies <freq-list> and outputs a
list of lists produced by adding the harmonics implied by each
chromatic pitch to produce a set of pure tones. Each list in the
resulting list corresponds to one pure tone. The first item in such
a list corresponds to the frequency of the pure tone (Hz). The second
item corresponds to its amplitude. The amplitude produced by the 
fundamental frequency component of a given chromatic pitch is 
set arbitrarily to 1.0. The number of harmonics is specified by
<num-harmonics>, which must be an integer. The amplitude of 
each harmonic is determined as a function of the harmonic's 
number, which should be specified via the anonymous function
<roll-off>. When more than one pure tone results at the same
frequency, amplitudes are combined assuming coherent or 
incoherent phases, according to the Boolean argument <coherent>.
The output list will be sorted in ascending order by frequency."
  (assert (integerp num-harmonics))
  (assert (functionp roll-off))
  (combine-pure-tones
   (mapcan #'(lambda (fundamental)
	       (freq->harmonics fundamental
				 :num-harmonics num-harmonics
				 :roll-off roll-off))
	   freq-list)
   :coherent coherent))


(defun freq->harmonics (fundamental &key (num-harmonics 10)
			  (roll-off #'(lambda (n) (/ 1 (1+ n)))))
  "Takes a <fundamental> frequency of a complex tone as its 
input and returns a set of pure tones intended to represent
that complex tone, by adding the harmonics to the fundamental
frequency. The number of harmonics is specified by
<num-harmonics>, which must be an integer. The amplitude of 
each harmonic is determined as a function of the harmonic's 
number, which should be specified via the anonymous function
<roll-off>. The amplitude of the fundamental will be 1.0."
  (assert (numberp fundamental))
  (assert (> fundamental 0))
  (assert (integerp num-harmonics))
  (assert (>= num-harmonics 0))
  (assert (functionp roll-off))
  (let ((output (list (list fundamental
			    1))))
    (loop
       for n from 1 to num-harmonics
       do (let ((frequency (* fundamental (1+ n)))
		(amplitude (funcall roll-off n)))
	    (push (list frequency amplitude)
		  output)))
    (reverse output)))

(defun combine-pure-tones (tones &key coherent)
  "Takes an unsorted list of pure tones, where each pure tone
corresponds to a list, the first element of which is the tone's
frequency, the second element of which is its amplitude. The function
outputs a sorted list of pure tones, represented in the same way,
where any tones of the same frequency have been combined assuming
either incoherent or coherent phases, depending on the argument
<coherent>, which is a Boolean."
  (assert (listp tones))
  (if (eql (length tones) 1)
      tones
      (let* ((sorted-tones (sort (copy-list tones)
				 #'(lambda (x y) (> (car x) (car y)))))
	     (output nil))
	(dolist (tone sorted-tones output)
	  (assert tone)
	  (assert (car tone))
	  (if (equalp (car tone) (caar output))
	      (setf (second (car output))
		    (sum-amplitudes
		     (second tone)
		     (second (car output))
		     :coherent coherent))
	      (push (copy-list tone) output))))))

(defun sum-amplitudes (amp-1 amp-2 &key coherent)
  "Takes two amplitudes, <amp-1> and <amp-2>, which should
be numbers of some kind, and combines them assuming either
coherent or incoherent phases, according to the argument <coherent>."
  (if coherent
      (float (+ amp-1 amp-2))
      (float (sqrt (+ (* amp-1 amp-1) (* amp-2 amp-2))))))

;; (defun cpitch->harmonics (cpitch &key num-harmonics
;; 					     (roll-off #'(lambda (n) (/ 1 n))))


;; (define-viewpoint (h-cpitch-class derived (h-cpitch))
;;     ;; Pitches present in harmonic slice, mod 12, including duplicates
;;     ((events md:harmonic-sequence) element)
;;   :function (let* ((pc (mapcar #'(lambda (x) (mod x 12)) (h-cpitch events)))
;; 		   (sort-pc (sort pc #'<)))
;; 	      sort-pc))

