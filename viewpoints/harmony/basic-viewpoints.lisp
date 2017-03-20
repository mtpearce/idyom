;;;; ======================================================================
;;;; File:       basic-viewpoints.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2014-09-25 19:09:17 marcusp>                           
;;;; Time-stamp: <2017-03-14 10:14:18 peter>                           
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
  (let* ((pitches (mapcar #'md:chromatic-pitch
			  (coerce (car (last events)) 'list)))
	 (pitches (sort pitches #'<))
	 (pitches (if *h-cpitch-round-to-int*
		      (mapcar #'round pitches)
		      pitches)))
    pitches))
