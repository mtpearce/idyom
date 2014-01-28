;;;; ======================================================================
;;;; File:       implication-realisation.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@eecs.qmul.ac.uk>
;;;; Created:    <2013-01-24 15:00:00 jeremy>
;;;; Time-stamp: <2014-01-28 09:57:32 marcusp>
;;;; ======================================================================

(cl:in-package #:viewpoints)

;; IR Principles

(defun large-interval (interval) (> (abs interval) 600))
(defun small-interval (interval) (< (abs interval) 600))
(defun same-direction (int1 int2) (= (signum int1) (signum int2)))
(defun different-direction (int1 int2) (not (same-direction int1 int2)))

;; Is a large (>= perfect fifth) jump followed by a direction change? Is
;; a small (<= perfect fourth) jump followed by a move in the same
;; direction? If the answer to either is yes, return 1, otherwise
;; 0. (N.B. a tritone move is undefined, I think because it isn't really
;; relevant to the theory - the movement after a tritone is almost always
;; to resolve a harmonic issue. Also, since this is based on chromatic
;; intervals, despite my use of diatonic ones in the description, a
;; diminished fifth and an augmented fourth would look the same).
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

;; Is a large jump followed by a smaller (3 semitones smaller if in the
;; same direction or 2 semitones if reversing the direction) jump? Is a
;; small jump followed by a similar interval? I think this returns 1 in
;; these cases, and 0 otherwise. I could be wrong.
(define-viewpoint (intervallic-difference derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2 e3)
                (values-list (last events 3))
              (if (or (null e1) (null e2) (null e3)) +undefined+
                  (let ((implicative (cpint (list e1 e2)))
                        (realised (cpint (list e2 e3))))
                    (if (undefined-p implicative realised) +undefined+
                        (let ((margin (if (same-direction implicative realised) 300 200)))
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

;; If the last three notes form a move away from and then back to a pitch
;; returns 3. If the returned to pitch is only a semitone away, returns
;; 2, if a tone returns 1. Otherwise returns 0. The pitch contour must be
;; an arch (\/ or /\), with a rising or falling contour or with any pitch
;; repeated, this returns 0.
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
                                 ((and (<= cpitch3 (+ cpitch1 200))
                                       (>= cpitch3 (- cpitch1 200)))
                                  (/ (- 300 (abs (- cpitch1 cpitch3))) 100))
                                 (t 0)))
                          (t 0)))))
  ;; TODO: function*
  )

;; 6 minus the number of semitones in the last interval: 0 if the last
;; interval is a tritone or greater; 1 for a perfect fourth; 4 for a tone;
;; 6 for a unison.
(define-viewpoint (proximity derived (cpitch))
    (events element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((realised (cpint (list e1 e2))))
                    (if (undefined-p realised) +undefined+
                        (let ((proximity (- 6 (/ (abs realised) 100))))
                          (if (< proximity 0) 0 proximity))))))
  ;; TODO: function*
  )

;; Also based on the shape defined by the last 3 notes. Scores 1 point
;; for a change of direction, 1 point for an interval that is more than a
;; tone smaller than the preceding one (so score can be 0, 1, 2)
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
