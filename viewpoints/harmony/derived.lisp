;;;; ======================================================================
;;;; File:       derived.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2014-09-25 19:09:17 marcusp>                           
;;;; Time-stamp: <2022-06-14 08:49:16 marcusp>                           
;;;; ======================================================================

(cl:in-package #:viewpoints)

;;;; Derived viewpoints

;; From melody we also have: 
;; 
;; * referent
;; * beatunit
;; * dur-ratio
;; 
;; * ioi, ioi-contour, posinbar, fib, crotchet, tactus, metaccent, met-interval, met-contour

(define-viewpoint (h-cpitch-class derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (mapcar #'(lambda (x) (mod x 12)) (h-cpitch events)))

(define-viewpoint (h-cpitch-class-set derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (sort (remove-duplicates (h-cpitch-class events) :test #'=) #'<))

(define-viewpoint (h-root derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (root (h-gct events)))

(define-viewpoint (h-base derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (base (h-gct events)))

(define-viewpoint (h-extension derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (extension (h-gct events)))

(define-viewpoint (h-gct derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let* ((pitch-class-set (h-cpitch-class-set events))
                   (mode (mode events))
                   (tonic (referent events))
                   (pitch-scale-hierarchy 
                    (list tonic (if (= mode 9)
                                    ;; (0 2 3 5 7 8 10)   ;; natural minor (2 1 2 2 1 2 2)
                                    '(0 2 3 5 7 8 11)     ;; harmonic minor (2 1 2 2 1 3 1)
                                    '(0 2 4 5 7 9 11)))))  ;; major
              (general-chord-type pitch-class-set pitch-scale-hierarchy *common-practice-consonance-vector*)))
