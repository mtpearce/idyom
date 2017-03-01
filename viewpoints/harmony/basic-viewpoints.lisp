;;;; ======================================================================
;;;; File:       basic-viewpoints.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2014-09-25 19:09:17 marcusp>                           
;;;; Time-stamp: <2017-03-01 15:11:28 peter>                           
;;;; ======================================================================

(cl:in-package #:viewpoints)

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
  (mapcar #'md:chromatic-pitch (coerce (car (last events)) 'list)))
