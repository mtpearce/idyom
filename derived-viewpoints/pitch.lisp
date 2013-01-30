;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       pitch.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@eecs.qmul.ac.uk>
;;;; Created:    <2013-01-24 15:00:00 jeremy>
;;;; Time-stamp: <2013-01-30 14:55:00 jeremy>
;;;; ======================================================================

(cl:in-package #:viewpoints)


;; Morphetic pitch

(define-viewpoint (mpitch-class derived (mpitch))
    (events element) 
  :function (let ((mpitch (mpitch events)))
              (cond ((undefined-p mpitch) +undefined+)
                    (t (mod mpitch 7))))
  :function* (remove-if-not #'(lambda (e) (= (mod e 12) element)) 
                            (viewpoint-alphabet (get-viewpoint 'mpitch))))


;; Chromatic Pitch 

(define-viewpoint (cpint derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((cpitch1 (cpitch (list e1)))
                        (cpitch2 (cpitch (list e2))))
                    (if (undefined-p cpitch1 cpitch2) +undefined+
                        (- cpitch2 cpitch1)))))
  :function* (list (+ element (cpitch (list (penultimate-element events))))))

(define-viewpoint (cpint-size derived (cpitch))
    (events element) 
  :function (let ((cpint (cpint events)))
              (cond ((undefined-p cpint) +undefined+)
                    (t (abs cpint))))
  :function* (let ((pitch (cpitch (list (penultimate-element events)))))
               (remove-if-not #'(lambda (a) 
                                  (or (= (+ pitch element) a)
                                      (= (- pitch element) a)))
                              (viewpoint-alphabet (get-viewpoint 'cpitch)))))

(define-viewpoint (contour derived (cpitch))
    (events element) 
  :function (let ((cpint (cpint events)))
              (cond ((undefined-p cpint) +undefined+)
                    (t (signum cpint))))
  :function* (let ((pitch (cpitch (list (penultimate-element events)))))
               (remove-if #'(lambda (a) (case element
                                          (-1 (>= a pitch))
                                          (0  (not (= a pitch)))
                                          (1  (<= a pitch))))
                          (viewpoint-alphabet (get-viewpoint 'cpitch)))))

(define-viewpoint (newcontour derived (cpitch))
    (events element) 
  :function (let ((contour2 (contour events))
                  (contour1 (contour (reverse (cdr (reverse events))))))
              (cond ((undefined-p contour2 contour1)
                     +undefined+)
                    ((= contour1 contour2) 1)
                    (t 0)))
  ;; TODO: function* 
  )



(define-viewpoint (cpitch-class derived (cpitch))
    (events element) 
  :function (let ((cpitch (cpitch events)))
              (cond ((undefined-p cpitch) +undefined+)
                    (t (mod cpitch 12))))
  :function* (remove-if-not #'(lambda (e) (= (mod e 12) element)) 
                            (viewpoint-alphabet (get-viewpoint 'cpitch))))

(define-viewpoint (cpcint derived (cpitch))
    (events element) 
  :function (let* ((cpint (cpint events)))
              (if (or (null cpint) (undefined-p cpint)) +undefined+ 
                  (if (minusp cpint) 
                      (- (mod (abs cpint) 12))
                      (mod cpint 12))))
  :function* (let ((pitch (cpitch (list (penultimate-element events)))))
               (remove-if-not #'(lambda (e) 
                                  (let* ((cpint (- e pitch))
                                         (cpint (if (minusp cpint) 
                                                    (- (mod (abs cpint) 12))
                                                    (mod cpint 12))))
                                    (= element cpint)))
                              (viewpoint-alphabet (get-viewpoint 'cpitch)))))

(define-viewpoint (cpcint-size derived (cpitch))
    (events element) 
  :function (let ((cpcint (cpcint events)))
              (cond ((undefined-p cpcint) +undefined+)
                    (t (abs cpcint))))
  ;; TODO: function*
  )

(define-viewpoint (cpcint-2 derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((cpcint (cpcint-size (list e1 e2))))
                    (if (undefined-p cpcint) +undefined+
                        (mod cpcint 2)))))
  ;; TODO: function* 
  )

(define-viewpoint (cpcint-3 derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((cpcint (cpcint-size (list e1 e2))))
                    (if (undefined-p cpcint) +undefined+
                        (mod cpcint 3)))))
  ;; TODO: function* 
  )

(define-viewpoint (cpcint-4 derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((cpcint (cpcint-size (list e1 e2))))
                    (if (undefined-p cpcint) +undefined+
                        (mod cpcint 4)))))
  ;; TODO: function* 
  )
  
(define-viewpoint (cpcint-5 derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((cpcint (cpcint-size (list e1 e2))))
                    (if (undefined-p cpcint) +undefined+
                        (mod cpcint 5)))))
  ;; TODO: function* 
  )
  
(define-viewpoint (cpcint-6 derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((cpcint (cpcint-size (list e1 e2))))
                    (if (undefined-p cpcint) +undefined+
                        (mod cpcint 6)))))
  ;; TODO: function* 
  )



(define-viewpoint (cpintfref derived (cpitch))
    (events element) 
  :function (let ((cpitch (cpitch events))
                  (referent (referent events)))
              (cond ((undefined-p cpitch referent) +undefined+)
                    (t (mod (- cpitch referent) 12))))
  :function* (let* ((referent (referent events))
                    (pitch (mod (+ referent element) 12)))
               (remove-if-not #'(lambda (e) (= (mod e 12) pitch))
                              (viewpoint-alphabet (get-viewpoint 'cpitch)))))

(define-viewpoint (cpintfip derived (cpitch))
    (events element) 
  :function (if (< (length events) 2) +undefined+ 
                ;; (if (= (length events) 1) 0
                (let ((cpitch1 (cpitch (list (car events))))
                      (cpitch2 (cpitch events)))
                  (if (undefined-p cpitch2 cpitch1) +undefined+
                      (- cpitch2 cpitch1))))
  :function* (list (+ element (cpitch (list (car events))))))


(define-viewpoint (cpintfiph derived (cpitch))
    (events element) 
  :function (if (= (fiph events) 1) +undefined+ 
                (let ((e1 (strip-until-true (get-viewpoint 'fiph) events)))
                  (if (null e1) +undefined+
                      (let ((cpitch1 (cpitch e1))
                            (cpitch2 (cpitch events)))
                        (if (undefined-p cpitch2 cpitch1) +undefined+
                            (- cpitch2 cpitch1))))))
  :function* (let ((e (strip-until-true (get-viewpoint 'fiph) events)))
               (list (+ element (cpitch e)))))

(define-viewpoint (cpintfib derived (cpitch))
    (events element) 
  :function (if (= (fib events) 1) +undefined+ ;; 0
                (let ((e1 (strip-until-true (get-viewpoint 'fib) events)))
                  (if (null e1) +undefined+
                      (let ((cpitch1 (cpitch e1))
                            (cpitch2 (cpitch events)))
                        (if (undefined-p cpitch2 cpitch1) +undefined+
                            (- cpitch2 cpitch1))))))
  :function* (let ((e (strip-until-true (get-viewpoint 'fib) events)))
               (list (+ element (cpitch e)))))



(define-viewpoint (octave derived (cpitch))
    (events element) 
  :function (let ((cpitch (cpitch events)))
              (cond ((undefined-p cpitch) +undefined+)
                    (t (floor cpitch 12))))
  ;; TODO: function* 
  )



(define-viewpoint (tessitura derived (cpitch))
    (events element) 
  :function (let ((cpitch (cpitch events)))    
              (cond ((undefined-p cpitch) +undefined+)
                    ((< cpitch 66) 0) ; from chorales 
                    ((> cpitch 74) 2) ; from chorales 
                    (t 1)))
  :function* (remove-if #'(lambda (e) 
                            (case element
                              (0 (>= e 66))
                              (1 (not (<= 66 e 74)))
                              (2 (<= e 74))))
                        (viewpoint-alphabet (get-viewpoint 'cpitch))))

