;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       implication-realisation.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@eecs.qmul.ac.uk>
;;;; Created:    <2013-01-24 15:00:00 jeremy>
;;;; Time-stamp: <2013-01-30 14:51:37 jeremy>
;;;; ======================================================================

(cl:in-package #:viewpoints)


;; IR Principles

(defun large-interval (interval) (> (abs interval) 6))
(defun small-interval (interval) (< (abs interval) 6))
(defun same-direction (int1 int2) (= (signum int1) (signum int2)))
(defun different-direction (int1 int2) (not (same-direction int1 int2)))

(define-viewpoint (registral-direction derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2 e3)
                (values-list (last events 3))
              (if (or (null e1) (null e2) (null e3)) +undefined+
                  (let ((implicative (cpint (list e1 e2)))
                        (realised (cpint (list e2 e3))))
                    (cond ((undefined-p implicative realised) +undefined+)
                          ((large-interval implicative)
                           (if (same-direction implicative realised) 0 1))
                          ((small-interval implicative)
                           (if (same-direction implicative realised) 1 0))
                          (t +undefined+)))))
  ;; TODO: function*
  )

(define-viewpoint (intervallic-difference derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2 e3)
                (values-list (last events 3))
              (if (or (null e1) (null e2) (null e3)) +undefined+
                  (let ((implicative (cpint (list e1 e2)))
                        (realised (cpint (list e2 e3))))
                    (if (undefined-p implicative realised) +undefined+
                        (let ((margin (if (same-direction implicative realised) 3 2)))
                          (cond ((large-interval implicative)
                                 (if (>= (abs realised) (- (abs implicative) margin))
                                     0 1))
                                ((small-interval implicative)
                                 (if (and (>= (abs realised)
                                              (- (abs implicative) margin))
                                          (<= (abs realised)
                                              (+ (abs implicative) margin)))
                                     1 0))
                                (t +undefined+)))))))
  ;; TODO: function*
  )

(define-viewpoint (registral-return derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2 e3)
                (values-list (last events 3))
              (if (or (null e1) (null e2) (null e3)) +undefined+
                  (let ((cpitch1 (cpitch (list e1)))
                        (cpitch3 (cpitch (list e3)))
                        (implicative (cpint (list e1 e2)))
                        (realised (cpint (list e2 e3))))
                    (cond ((undefined-p implicative realised) +undefined+)
                          ((different-direction realised implicative)
                           (cond ((or (zerop implicative) (zerop realised)) 0)
                                 ((and (<= cpitch3 (+ cpitch1 2))
                                       (>= cpitch3 (- cpitch1 2)))
                                  (- 3 (abs (- cpitch1 cpitch3))))
                                 (t 0)))
                          (t 0)))))
  ;; TODO: function*
  )

                        
(define-viewpoint (proximity derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((realised (cpint (list e1 e2))))
                    (if (undefined-p realised) +undefined+
                        (let ((proximity (- 6 (abs realised))))
                          (if (< proximity 0) 0 proximity))))))
  ;; TODO: function*
  )
  
(define-viewpoint (closure derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2 e3)
                (values-list (last events 3))
              (if (or (null e1) (null e2) (null e3)) +undefined+
                  (let ((implicative (cpint (list e1 e2)))
                        (realised (cpint (list e2 e3))))
                    (if (undefined-p implicative realised) +undefined+
                        (let ((condition1 (different-direction 
                                           implicative realised))
                              (condition2 (< (abs realised) 
                                             (- (abs implicative) 2)))
                              (score 0))
                          (when condition1 (incf score))
                          (when condition2 (incf score))
                          score)))))
  ;; TODO: function*
  )
