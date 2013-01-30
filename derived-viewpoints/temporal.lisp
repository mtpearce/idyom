;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       temporal.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@eecs.qmul.ac.uk>
;;;; Created:    <2013-01-24 15:00:00 jeremy>
;;;; Time-stamp: <2013-01-30 14:53:15 jeremy>
;;;; ======================================================================

(cl:in-package #:viewpoints)

;; Onset 

(define-viewpoint (ioi derived (onset))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((onset1 (onset (list e1)))
                        (onset2 (onset (list e2))))
                    (cond ((undefined-p onset1 onset2) +undefined+)
                          (t (- onset2 onset1))))))
  :function* (list (+ element (onset (list (penultimate-element events))))))

(define-viewpoint (posinbar derived (onset))
    (events element) 
  :function (let ((onset (onset events))
                  (barlength (barlength events)))
              (cond ((undefined-p onset barlength) +undefined+)
                    ((zerop barlength) +undefined+)
                    ((zerop onset) 0)
                    ((> onset 0) (mod onset barlength))
                    (t +undefined+)))
  ;; TODO: function*
  )

(define-viewpoint (fib test (onset))
    (events element) 
  :function (let ((posinbar (posinbar events)))
              (cond ((undefined-p posinbar) +undefined+)
                    ((= posinbar 0) 1)
                    (t 0)))
  ;; TODO: function* 
  )

(define-viewpoint (crotchet test (onset))
    (events element) 
  :function (let ((e1 (car events))
                  (e2 (last-element events)))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((onset1 (onset (list e1)))
                        (onset2 (onset (list e2))))
                    (if (undefined-p onset1 onset2) +undefined+
                        ;;this only works if crotchet == 24 
                        (if (zerop (mod (- onset2 onset1) 24)) 1 0)))))
  ;; TODO: function* 
  )

(define-viewpoint (tactus test (onset))
    (events element) 
  :function (let ((event (last events)))
              (if (null event) +undefined+
                  (let ((barlength (barlength event))
                        (pulses (pulses event))
                        (onset (onset event)))
                    (declare (type fixnum barlength pulses))
                    (if (or (undefined-p barlength pulses onset)
                            (zerop barlength)
                            (zerop pulses))
                        +undefined+
                        (if (zerop (mod onset (/ barlength pulses))) 1 0)))))
  ;; TODO: function* 
  )

(define-viewpoint (ioi-ratio derived (onset))
    (events element) 
  :function (multiple-value-bind (e1 e2 e3)
                (values-list (last events 3))
              (if (or (null e1) (null e2) (null e3)) +undefined+
                  (let ((ioi1 (ioi (list e1 e2)))
                        (ioi2 (ioi (list e2 e3))))
                    (declare (type fixnum ioi1 ioi2))
                    (if (undefined-p ioi1 ioi2) +undefined+
                        (/ ioi2 ioi1)))))
  :function* (let ((penultimate-element (list (penultimate-element events))))
               (list (+ (onset penultimate-element) 
                        (* element (ioi penultimate-element))))))

;; bioi

(define-viewpoint (bioi-ratio derived (bioi))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((ioi1 (bioi (list e1)))
                        (ioi2 (bioi (list e2))))
                    (if (or (zerop ioi1) (undefined-p ioi1 ioi2)) +undefined+
                        (/ ioi2 ioi1)))))
  :function* (let ((penultimate-element (list (penultimate-element events))))
               (list (* element (bioi penultimate-element)))))

(define-viewpoint (bioi-contour derived (bioi))
    (events element) 
  :function (let ((bioi-ratio (bioi-ratio events)))
              (if (undefined-p bioi-ratio) +undefined+
                  (signum (- bioi-ratio 1))))
  :function* (let ((bioi (bioi (list (penultimate-element events)))))
               (remove-if #'(lambda (a) (case element
                                          (-1 (>= a bioi))
                                          (0  (not (= a bioi)))
                                          (1  (<= a bioi))))
                          (viewpoint-alphabet (get-viewpoint 'bioi)))))

;; dur

(define-viewpoint (dur-ratio derived (dur))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((dur1 (dur (list e1)))
                        (dur2 (dur (list e2))))
                    (declare (type fixnum dur1 dur2))
                    (if (undefined-p dur1 dur2) +undefined+
                        (/ dur2 dur1)))))
  :function* (list (* element (dur (list (penultimate-element events))))))



