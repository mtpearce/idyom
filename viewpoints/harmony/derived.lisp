;;;; ======================================================================
;;;; File:       derived.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2014-09-25 19:09:17 marcusp>                           
;;;; Time-stamp: <2022-06-24 14:32:49 marcusp>                           
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

;;; Harrison (2019, Ch. 4) +++ = not tested empirically

;; ++ pitch chord (pi_chord) **chromatic pitch in ascending order** 
;; ++ pitch chord type (pi_chord_type) **relative to bass note**
;; 
;; -- pitch-class set (pc_set) **pitch-class sans duplicates** 
;; -- pitch-class set type (pc_set_type, pc_set_rel_bass) **relative to bass note**
;;
;; -- pitch-class chord (pc_chord) **bass note (inversion) specified*
;; ++ pitch-class chord type (pc_chord_type) **relative to bass note**
;; 
;; -- bass pitch class (bass_pc)
;; -- bass interval (bass_int)
;; -- bass pitch-class relative to root (bass_pc_rel_root)
;; -- root pitch class (root_pc)
;; -- root interval (root_int)
;; -- pitch-class set relative to root (pc_set_rel_root)
;; -- pitch-class set relative to previous bass (pc_set_rel_prev_bass)
;; -- pitch-class chord relative to previous bass (pc_chord_rel_prev_bass)


;;; Sears et al. (2018, 2019)

;; vertical interval class combination (vintcc): interval class from lowest note
;; doublings excluded, permutations allowed
(define-viewpoint (vintcc derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let* ((chord (h-cpitch events))
                   (bass (car chord)))
              (sort (remove-duplicates (mapcar #'(lambda (x) (mod (- x bass) 12)) chord) :test #'=) #'<)))

;; chromatic scale degree combination (csdc): scale degrees
;; doublings excluded, permutations allowed in upper parts
(define-viewpoint (csdc derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let* ((chord (h-cpitch events))
                   (referent (referent events))
                   (sds (remove-duplicates (mapcar #'(lambda (x) (mod (- x referent) 12)) chord) :test #'=)))
              (cons (car sds) (sort (cdr sds) #'<))))
              

;;; General Chord Type (GCT) representations 

(define-viewpoint (h-root-function derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((root (h-root events))
                  (tonic (referent events)))
              (if (null tonic)
                  +undefined+
                  (utils:subtract-mod-n root tonic 12))))

(define-viewpoint (h-root derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (root (h-gct events)))

(define-viewpoint (h-chord-type derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (chord-type (h-gct events)))

(define-viewpoint (h-gct derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (general-chord-type (h-cpitch events) *tonal-consonance-vector*))

;; (define-viewpoint (h-gct-2014 derived (h-cpitch))
;;     ((events md:harmonic-sequence) element)
;;   :function (let* ((pitch-class-set (h-cpitch-class-set events))
;;                    (mode (mode events))
;;                    (tonic (referent events))
;;                    (pitch-scale-hierarchy 
;;                     (list tonic (if (= mode 9)
;;                                     ;; (0 2 3 5 7 8 10)   ;; natural minor (2 1 2 2 1 2 2)
;;                                     '(0 2 3 5 7 8 11)     ;; harmonic minor (2 1 2 2 1 3 1)
;;                                     '(0 2 4 5 7 9 11)))))  ;; major
;;               (general-chord-type-2014 pitch-class-set pitch-scale-hierarchy *common-practice-consonance-vector*)))

