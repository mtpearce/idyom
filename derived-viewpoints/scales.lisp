;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       scales.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@eecs.qmul.ac.uk>
;;;; Created:    <2013-01-24 15:00:00 jeremy>
;;;; Time-stamp: <2013-01-30 15:48:58 jeremy>
;;;; ======================================================================

(cl:in-package #:viewpoints)

;; Western scales
  
(define-viewpoint (inscale derived (cpitch))
    (events element) 
  :function (let ((cpitch-class (cpitch-class events))
                  (referent (referent events))
                  (mode (mode events)))
              (cond ((undefined-p cpitch-class referent mode) +undefined+)
                    ((member cpitch-class (diatonic-set referent mode) 
                             :test #'=)
                     1)
                    (t 0)))
  :function* (let* ((referent (referent events)) 
                    (mode (mode events))
                    (ds (diatonic-set referent mode)))
               (remove-if #'(lambda (e) (case element 
                                          (0 (member (mod e 12) ds))
                                          (1 (not (member (mod e 12) ds)))))
                          (viewpoint-alphabet (get-viewpoint 'cpitch)))))

(defun diatonic-set (referent mode)
  (let* ((diatonic-set '(0 2 4 5 7 9 11))
         (start (position mode diatonic-set)))
    (mapcar #'(lambda (x) (mod (+ (- x mode) referent) 12))
            (append (subseq diatonic-set start)
                    (subseq diatonic-set 0 start)))))









