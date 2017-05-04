;;;; ======================================================================
;;;; File:       basic-viewpoints.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2014-09-25 19:09:17 marcusp>                           
;;;; Time-stamp: <2017-05-04 10:23:51 peter>                           
;;;; ======================================================================

(cl:in-package #:viewpoints)

;; This parameter determines whether all cpitch values
;; are rounded to the nearest integer before incorporation
;; in h-cpitch.
(defparameter *h-cpitch-round-to-int* t)

(defvar *common-practice-consonance-vector* '(1 0 0 1 1 1 0 1 1 1 0 0))

;;;================================
;;;* Basic viewpoints from melody *
;;;================================

;; * onset
;; * duration
;; * keysig
;; * mode
;; * pulses
;; * barlength
;; * tempo

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (let ((slice (make-instance 'md:music-slice))
;;         (types '(onset dur keysig mode pulses barlength tempo)))
;;     (mapcar #'(lambda (x) (viewpoints:register-basic-type x slice)) types)))

;;;==================================
;;;* Derived viewpoints from melody *
;;;==================================

;; * referent
;; * beatunit
;; * dur-ratio
;; * ioi, ioi-contour, posinbar, fib, crotchet, tactus,
;;   metaccent, met-interval, met-contour

;;;==============================
;;;* New viewpoints for harmony *
;;;==============================

(define-basic-viewpoint h-cpitch ((events md:harmonic-sequence))
  ;; Pitches present in harmonic slice
  (let* ((last-event (car (last events)))
	 (pitches (md:get-attribute last-event 'md:h-cpitch))
	 (pitches (sort pitches #'<))
	 (pitches (if *h-cpitch-round-to-int*
		      (mapcar #'round pitches)
		      pitches)))
    pitches))

;;;==============================
;;;* Testing functions *
;;;==============================

;; Note: this function is replicated in music-data
(defun harm-seq
    (cpitch &key onset dur (ref-pitch 0))
  "Intended for testing purposes. Makes an object of class md:harmonic-sequence
with attributes specified by <cpitch>, <onset>, <dur>, and <ref-pitch>.
<cpitch> should be a list of lists, with the ith element of the jth list
corresponding to the ith chromatic pitch in the jth chord, after transposition
by <ref-pitch>. By default, each chord has duration 100 and onset 100 basic
time units after the onset of the previous chord. It is possible to
specify <onset> manually, in which case <onset> should be a list with the ith
element being the onset of the ith chord. Chords should be listed in order
of increasing onset. In this case the duration of each
chord will default to the inter-onset interval between each chord,
unless <dur> is specified manually, in which case <dur> should 
be a list the ith element of which is the duration of the ith chord.
"
  (if (or (null cpitch) (not (listp cpitch)))
      (error "<cpitch> must be a non-empty list."))
  (let* ((num-chords (length cpitch))
	 (onset (cond ((null onset)
		       (loop for x from 0 to (1- num-chords)
			  collect (* 100 x)))
		      ((eql (length onset) num-chords)
		       onset)
		      (t (error "<onset> must be NIL or a list of same length as <cpitch>."))))
	 (dur (cond ((null dur)
		     (if (eql (length onset) 1)
			 '(100)
			 (append (loop
				    for a in (butlast onset)
				    for b in (cdr onset)
				    collect (- b a))
				 '(100))))
		    ((eql (length dur) num-chords) dur)
		    (t (error "<dur> must be NIL or a list of same length as <cpitch>.")))))
    (if (not (every #'(lambda (x) (> x 0)) dur))
	(error "Every element of <dur> must be greater than 0."))
    (let* ((seq (make-instance 'md:harmonic-sequence
			       :onset 0
			       :duration (+ (car (last onset)) (car (last dur)))
			       :midc 60))
	   (slices nil))
      (loop
	 for chord in cpitch
	 for o in onset
	 for d in dur
	 do (progn
	      (let ((events (mapcar #'(lambda (pitch)
					(let ((event (make-instance
						      'md::music-event
						      :cpitch (+ ref-pitch pitch))))
					  (md:set-attribute event 'onset o)
					  (md:set-attribute event 'dur d)
					  event))
				    chord))
		    (slice (make-instance 'md:music-slice
					  :onset o :duration d)))
		(sequence:adjust-sequence slice (length events)
					  :initial-contents events)
		(push slice slices))))
      (sequence:adjust-sequence seq (length slices)
				:initial-contents (reverse slices))
      seq)))
