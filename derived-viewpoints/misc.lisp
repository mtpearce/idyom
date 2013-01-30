;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       misc.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@eecs.qmul.ac.uk>
;;;; Created:    <2013-01-24 15:00:00 jeremy>
;;;; Time-stamp: <2013-01-30 15:34:07 jeremy>
;;;; ======================================================================

(cl:in-package #:viewpoints)

;; Keysig

(define-viewpoint (referent derived (keysig))
    (events element) 
  :function (let ((keysig (keysig events))
                  (mode (mode events)))
              ;(declare (type (integer -7 7) keysig) (type (integer 0 11) mode))
              (if (undefined-p keysig mode) +undefined+
                  (cond ((and (numberp keysig) (> keysig 0))
                         (mod (+ (* keysig 7) mode) 12))
                        ((and (numberp keysig) (< keysig 0))
                         (mod (+ (* (- keysig) 5) mode) 12))
                        ((numberp mode) mode)
			(t +undefined+)))) 
  :function* (viewpoint-alphabet (get-viewpoint 'keysig)))


;; Phrase

(define-viewpoint (fiph test (phrase))
    (events element) 
  :function (let ((phrase (phrase events)))
              (cond ((undefined-p phrase) +undefined+)
                    ((= phrase 1) 1)
                    (t 0)))
  :function* (if (= element 1) 1 (list -1 0)))

(define-viewpoint (liph test (phrase))
    (events element) 
  :function (let ((phrase (phrase events)))
              (cond ((undefined-p phrase) +undefined+)
                    ((= phrase -1) 1)
                    (t 0)))
  :function* (if (= element 1) -1 (list 1 0)))

(define-viewpoint (lphrase derived (phrase))
    (events element) 
  :function (let ((e2 (last-element events)))
              (if (null e2) +undefined+
                  (let* ((phrase (phrase (list e2)))
                         (e1 (case phrase
                               (0 +undefined+)
                               (1 (last-element 
                                   (strip-until-true (get-viewpoint 'fiph)
                                                     (butlast events))))
                               (-1 (last-element 
                                    (strip-until-true (get-viewpoint 'liph)
                                                      (butlast events)))))))
                    (cond ((undefined-p e1) +undefined+)
                          ((null e1) 0)
                          (t (ioi (list e1 e2)))))))
  ;; TODO: function* 
  )




(define-viewpoint (thrbar threaded (cpitch onset))
    (events element) 
  :function (let ((e1 (last-element (strip-until-true (get-viewpoint 'fib)
                                                    (butlast events))))
                  (e2 (last-element events)))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((fib1 (fib (list e1)))
                        (fib2 (fib (list e2))))
                    (if (or (zerop fib1) (zerop fib2))
                        +undefined+
                        (cpint (list e1 e2))))))
  :function* (let ((e (strip-until-true (get-viewpoint 'fib) 
                                        (butlast events))))
               (list (+ element (cpitch e)))))

(define-viewpoint (thrfiph threaded (cpitch onset))
    (events element) 
  :function (let ((e1 (last-element (strip-until-true (get-viewpoint 'fiph)
                                                    (butlast events))))
                  (e2 (last-element events)))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((fiph1 (fiph (list e1)))
                        (fiph2 (fiph (list e2))))
                    (if (or (zerop fiph1) (zerop fiph2))
                        +undefined+
                        (cpint (list e1 e2))))))
  :function* (let ((e (strip-until-true (get-viewpoint 'fiph) 
                                        (butlast events))))
               (list (+ element (cpitch e)))))
  
(define-viewpoint (thrliph threaded (cpitch onset))
    (events element) 
  :function (let ((e1 (last-element (strip-until-true (get-viewpoint 'liph)
                                                    (butlast events))))
                  (e2 (last-element events)))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((liph1 (liph (list e1)))
                        (liph2 (liph (list e2))))
                    (if (or (zerop liph1) (zerop liph2))
                        +undefined+
                        (cpint (list e1 e2))))))
  :function* (let ((e (strip-until-true (get-viewpoint 'liph) 
                                        (butlast events))))
               (list (+ element (cpitch e)))))

(define-viewpoint (thrintfrefliph threaded (cpitch onset))
    (events element) 
  :function (let ((e (last-element events)))
              (if (null e) +undefined+
                  (let ((liph (liph (list e))))
                    (if (zerop liph)
                        +undefined+
                        (cpintfref (list e))))))
  ;; TODO: function* 
  ) 

(define-viewpoint (thrqu threaded (cpitch onset))
    (events element) 
  :function (let* ((events-1 (strip-until-true (get-viewpoint 'crotchet)
                                               (butlast events)))
                   (e1 (last-element events-1))
                   (e2 (last-element events)))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((crotchet1 (crotchet events-1))
                        (crotchet2 (crotchet events)))
                    (if (or (zerop crotchet1) (zerop crotchet2))
                        +undefined+
                        (cpint (list e1 e2))))))
  ;; TODO: function*
  )

(define-viewpoint (thrtactus threaded (cpitch onset))
    (events element) 
  :function (let* ((events-1 (strip-until-true (get-viewpoint 'tactus)
                                               (butlast events)))
                   (e1 (last-element events-1))
                   (e2 (last-element events)))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((tactus1 (tactus events-1))
                        (tactus2 (tactus events)))
                    (if (or (zerop tactus1) (zerop tactus2))
                        +undefined+
                        (cpint (list e1 e2))))))
  :function* (let ((e (strip-until-true (get-viewpoint 'tactus) 
                                        (butlast events))))
               (list (+ element (cpitch e)))))


