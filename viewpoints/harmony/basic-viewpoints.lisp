;;;; ======================================================================
;;;; File:       basic-viewpoints.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2014-09-25 19:09:17 marcusp>                           
;;;; Time-stamp: <2022-06-14 08:50:16 marcusp>                           
;;;; ======================================================================

(cl:in-package #:viewpoints)

;;;; Basic viewpoints

;; From melody, we also have:
;; 
;; * onset
;; * duration
;; * keysig
;; * mode
;; * pulses
;; * barlength
;; * tempo
;; 
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (let ((slice (make-instance 'md:music-slice))
;;         (types '(onset dur keysig mode pulses barlength tempo)))
;;     (mapcar #'(lambda (x) (viewpoints:register-basic-type x slice)) types)))

(define-basic-viewpoint h-cpitch ((events md:harmonic-sequence))
  (mapcar #'md:chromatic-pitch (sort (coerce (car (last events)) 'list) #'<)))


