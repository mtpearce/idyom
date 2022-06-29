;;;; ======================================================================
;;;; File:       derived.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2014-09-25 19:09:17 marcusp>                           
;;;; Time-stamp: <2022-06-29 18:30:50 marcusp>                           
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


;;; ============================================================================
;;; Harrison (2019, Ch. 2, 4)
;;;
;;; 1. ‘Pitch-class’ representations express octave invariance;
;;; 2. ‘Chord’ representations identify which pitch class corresponds to the lowest pitch in the chord (the bass note);
;;; 3. ‘Type’ representations express transposition invariance.

;; -- pitch chord (pi_chord): chromatic pitch in ascending order
;; -- pitch chord type (pi_chord_type): pi_chord relative to bass note, providing transpositional equivalence

(define-viewpoint (pi-chord derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (h-cpitch events))

(define-viewpoint (pi-chord-type derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let* ((pi-chord (pi-chord events))
                   (bass (car pi-chord)))
              (mapcar #'(lambda (x) (- x bass)) pi-chord)))

;; -- pitch-class chord (pc_chord): pc_set with bass note (i.e., inversion) specified (24,576 elements)
;; -- pitch-class chord type (pc_chord_type): pc_chord relative to bass note (2,048 elements)
;; -- pitch-class chord relative to previous bass (pc_chord_rel_prev_bass): pc_chord relative to previous bass (24,576)

(define-viewpoint (pc-chord derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((pitch-classes (remove-duplicates (h-cpitch-class events) :test #'= :from-end t)))
              (list (car pitch-classes)
                    (sort (cdr pitch-classes) #'<))))

(define-viewpoint (pc-chord-type derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let* ((pc-chord (pc-chord events))
                   (bass (car pc-chord))
                   (pc-chord (cons bass (cadr pc-chord))))
              (sort (mapcar #'(lambda (x) (mod (- x bass) 12)) pc-chord) #'<)))

(define-viewpoint (pc-chord-rel-prev-bass derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((prev-events (butlast events)))
              (if (null prev-events) +undefined+
                  (let* ((pc-chord (pc-chord events))
                         (bass (car pc-chord))
                         (pc-chord (cons bass (cadr pc-chord)))
                         (prev-bass (bass-pc prev-events))
                         (pc-chord (mapcar #'(lambda (x) (mod (- x prev-bass) 12)) pc-chord)))
                    (list (car pc-chord) (sort (cdr pc-chord) #'<))))))

;; -- pitch-class set (pc_set): pitch-class sans duplicates, ascending order (4,095 elements)
;; -- pitch-class set type (pc_set_type): transposition invariant pc_set (351 elements) 
;; -- pitch-class set relative to bass (pc_set_rel_bass) == pc_chord_type (2,048 elements) 
;; -- pitch-class set relative to previous bass (pc_set_rel_prev_bass): pc_set relative to previous bass (4,095 elements)
;; -- pitch-class set relative to root (pc_set_rel_root): pc_set relative to chord root (457 elements) 

(define-viewpoint (pc-set derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (sort (remove-duplicates (h-cpitch-class events) :test #'=) #'<))

(define-viewpoint (pc-set-type derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let* ((pc-set (pc-set events))
                   (nf (normal-form pc-set))
                   (bass (car nf)))
              (mapcar #'(lambda (x) (mod (- x bass) 12)) nf)))

(defun normal-form (pc-set)
  "Find the normal form of a pc-set provided in ascending order (Rahn, 1980; Forte, 1973)."
  (labels ((get-score (pc-set)
             (mod (- (car (last pc-set)) (car pc-set)) 12))
           (choose-ties (candidates)
             (let ((result nil)
                   (l (length (car candidates))))
               (do* ((i 0 (1+ i))
                     (c candidates (mapcar #'(lambda (x) (subseq x 0 i)) c)))
                    ((or result (= i (- l 2))) result)
                 (let* ((scores (mapcar #'get-score c))
                        (min (apply #'min scores))
                        (min-count (count min scores :test #'=)))
                   (when (= min-count 1)
                     (setf result (elt pc-set (position min pc-set :test #'=))))))
               (if (null result)
                   (car (sort pc-set #'< :key #'car))
                   result))))
    (let ((candidate pc-set)
          (top-score nil)
          (winners nil))
      (dotimes (i (length pc-set))
        (let ((score (get-score candidate)))
          (cond ((or (null winners) (< score top-score))
                 (setf top-score score
                       winners (list candidate)))
                ((= score top-score)
                 (push candidate winners))))
        (setf candidate (utils:rotate candidate 1)))
      (if (= (length winners) 1)
          (car winners)
          (choose-ties winners)))))
  
(define-viewpoint (pc-set-rel-bass derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (pc-chord-type events))

(define-viewpoint (pc-set-rel-prev-bass derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((prev-events (butlast events)))
              (if (null prev-events) +undefined+
                  (let ((pc-set (pc-set events))
                        (prev-bass (bass-pc prev-events)))
                    (sort (mapcar #'(lambda (x) (mod (- x prev-bass) 12)) pc-set) #'<)))))

(define-viewpoint (pc-set-rel-root derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((pc-set (pc-set events))
                  (root (root-pc events)))
              (sort (mapcar #'(lambda (x) (mod (- x root) 12)) pc-set) #'<)))

;; -- bass pitch class (bass_pc): bass pitch class (12 elements)
;; -- bass interval (bass_int): bass pitch class interval to previous bass (12 elements) 
;; -- bass pitch-class relative to root (bass_pc_rel_root): bass pitch class relative to root (12 elements) 

(define-viewpoint (bass-pc derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (car (pc-chord events)))

(define-viewpoint (bass-int derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((prev-events (butlast events)))
              (if (null prev-events) +undefined+
                  (let ((bass (bass-pc events))
                        (prev-bass (bass-pc prev-events)))
                    (mod (- bass prev-bass) 12)))))

(define-viewpoint (bass-pc-rel-root derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((bass (bass-pc events))
                  (root (root-pc events)))
              (mod (- bass root) 12)))

;; -- root pitch class (root_pc): root pitch class (12 elements) 
;; -- root interval (root_int): root pitch class interval to previous root (12 elements)

(define-viewpoint (root-pc derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (parn88-root events))

(define-viewpoint (root-int derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((prev-events (butlast events)))
              (if (null prev-events) +undefined+
                  (let ((root (root-pc events))
                        (prev-root (root-pc prev-events)))
                    (mod (- root prev-root) 12)))))


;;; ============================================================================
;;; Scale degrees

(define-viewpoint (sd-set derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((pc-set (pc-set events))
                  (tonic (referent events)))
              (if (undefined-p tonic) +undefined+
                  (sort (mapcar #'(lambda (x) (mod (- x tonic) 12)) pc-set) #'<))))
  
(define-viewpoint (bass-sd derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((bass (bass-pc events))
                  (tonic (referent events)))
              (if (undefined-p tonic)
                  +undefined+
                  (mod (- bass tonic) 12))))
  
(define-viewpoint (root-sd derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (let ((root (root-pc events))
                  (tonic (referent events)))
              (if (undefined-p tonic)
                  +undefined+
                  (mod (- root tonic) 12))))


;;; ============================================================================
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
  :function (let* ((pi-chord (pi-chord events))
                   (tonic (referent events))
                   (sds (remove-duplicates (mapcar #'(lambda (x) (mod (- x tonic) 12)) pi-chord) :test #'=)))
              (cons (car sds) (sort (cdr sds) #'<))))


;;; ============================================================================
;;; Parncutt (1988) root finding model

(define-viewpoint (parn88-root derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (car (parn88 (pi-chord events))))


;;; ============================================================================
;;; General Chord Type (GCT) representations (Giannis & Cambouropoulos, 2021)

(define-viewpoint (gct-root derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (root (gct events)))

(define-viewpoint (gct-chord-type derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (chord-type (gct events)))

(define-viewpoint (gct derived (h-cpitch))
    ((events md:harmonic-sequence) element)
  :function (general-chord-type (h-cpitch events) *tonal-consonance-vector*))

;; (define-viewpoint (gct-2014 derived (h-cpitch))
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

