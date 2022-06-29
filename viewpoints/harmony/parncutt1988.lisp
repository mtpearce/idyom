;;;; ======================================================================
;;;; File:       parncutt1988.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2022-06-29 13:09:17 marcusp>                           
;;;; Time-stamp: <2022-06-29 19:25:37 marcusp>                           
;;;; ======================================================================

;;; An implementation of the key-finding algorithm of Parncutt (1988).
;;;
;;;   Parncutt, R. (1988). Revision of Terhardt's Psychoacoustical
;;;   Model of the Root (s) of a Musical Chord. Music Perception,
;;;   65-93.
;;;
;;;   Parncutt, R. (2006). “Commentary on Cook & Fujisawa’s "The
;;;   Psychophysics of Harmony Perception: Harmony is a Three-Tone
;;;   Phenomenon".” Empirical Musicology Review 1 (4): 204–9.
;;;
;;; Follows the implementation in R by Peter Harrison:
;;; 
;;;   https://doi.org/10.5281/zenodo.1491909
;;;   https://github.com/pmcharrison/parn88
;;;
;;; Summary
;;; 
;;;   1. each pitch class is assigned a weight -> the root is the pc with the greatest weight
;;;   2. the weights are used to determine the root ambiguity
;;;   3. root ambiguity is used to estimate salience for each pitch class

(cl:in-package #:viewpoints)

;; original weights from Parncutt (1988)
(defvar *root-support-weights-parn88* '(1 0 1/5 1/10 1/3 0 0 1/2 0 0 1/4 0))

;; revised weights from Parncutt (2006)
(defvar *root-support-weights-parn06* '(10 0 1 0 3 0 0 5 0 0 2 0))

(defun parn88 (chord &key (root-support *root-support-weights-parn06*) (exponent 0.5))
  (parn88-pc-set (sort (remove-duplicates (mapcar #'(lambda (x) (mod x 12)) chord) :test #'=) #'<)
                 :root-support root-support :exponent exponent))

(defun parn88-pc-set (pc-set &key (root-support *root-support-weights-parn06*) (exponent 0.5))
  (let* ((pc-set (encode-pc-set pc-set))
         (weights (mapcar #'(lambda (x) (pc-weight x pc-set root-support)) (utils:generate-integers 0 11)))
         (max (apply #'max weights))
         (root (position max weights :test #'=))
         (ambiguity (get-root-ambiguity weights exponent)))
    (list root ambiguity weights)))
                   
(defun pc-weight (pc pc-set root-support)
  (let ((pc-set (utils:rotate pc-set pc)))
    (reduce #'+ (mapcar #'* pc-set root-support))))

(defun get-root-ambiguity (weights exponent)
  (let ((max (apply #'max weights)))
    (expt (reduce #'+ (mapcar #'(lambda (x) (/ x max)) weights)) exponent)))

(defun encode-pc-set (pc-set)
  (let ((y (make-list 12 :initial-element 0)))
    (dolist (x pc-set y)
      (setf (elt y x) 1))))

      
  
