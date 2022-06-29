;;;; ======================================================================
;;;; File:       utils-tests.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2022-06-23 13:17:12 peter>                         
;;;; Time-stamp: <2022-06-29 23:35:02 marcusp>                           
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; Tests for the viewpoints package.

(cl:in-package #:viewpoints)

(5am:def-suite viewpoints :in testing::idyom-tests)
(5am:in-suite viewpoints)


;;; Harmony: GCT algorithm (from Giannos & Cambouropoulos, 2021, Table 1). 

(5am:def-suite harmony :in viewpoints)
(5am:in-suite harmony)

;; triads
(5am:test gct-major      (5am:is (equalp (general-chord-type '(60 64 67) *tonal-consonance-vector*) '(0 (0 4 7)))))
(5am:test gct-minor      (5am:is (equalp (general-chord-type '(60 63 67) *tonal-consonance-vector*) '(0 (0 3 7)))))
(5am:test gct-diminished (5am:is (equalp (general-chord-type '(60 63 66) *tonal-consonance-vector*) '(0 (0 3 6)))))
(5am:test gct-augmented  (5am:is (equalp (general-chord-type '(60 64 68) *tonal-consonance-vector*) '(0 (0 4 8)))))
(5am:test gct-sus2       (5am:is (equalp (general-chord-type '(60 62 67) *tonal-consonance-vector*) '(0 (0 7 14)))))
(5am:test gct-sus4       (5am:is (equalp (general-chord-type '(60 65 67) *tonal-consonance-vector*) '(0 (0 7 17)))))
;; tetrads 
(5am:test gct-major7th          (5am:is (equalp (general-chord-type '(60 64 67 71) *tonal-consonance-vector*) '(0 (0 4 7 11)))))
(5am:test gct-minor7th          (5am:is (equalp (general-chord-type '(60 63 67 70) *tonal-consonance-vector*) '(0 (0 3 7 10)))))
(5am:test gct-dominant7th       (5am:is (equalp (general-chord-type '(60 64 67 70) *tonal-consonance-vector*) '(0 (0 4 7 10)))))
(5am:test gct-dominant7thsus4   (5am:is (equalp (general-chord-type '(60 65 67 70) *tonal-consonance-vector*) '(0 (0 7 10 17)))))
(5am:test gct-diminished7th     (5am:is (equalp (general-chord-type '(60 63 66 69) *tonal-consonance-vector*) '(0 (0 3 6 9)))))
(5am:test gct-halfdiminished7th (5am:is (equalp (general-chord-type '(60 63 66 70) *tonal-consonance-vector*) '(3 (0 3 7 9)))))  ;;
(5am:test gct-minormajor7th     (5am:is (equalp (general-chord-type '(60 63 67 71) *tonal-consonance-vector*) '(0 (0 3 7 11)))))
(5am:test gct-augmented7th      (5am:is (equalp (general-chord-type '(60 64 68 70) *tonal-consonance-vector*) '(0 (0 4 8 10)))))
(5am:test gct-major6th          (5am:is (equalp (general-chord-type '(60 64 67 69) *tonal-consonance-vector*) '(9 (0 3 7 10))))) ;; 
(5am:test gct-minor6th          (5am:is (equalp (general-chord-type '(60 63 67 69) *tonal-consonance-vector*) '(0 (0 3 7 9)))))
;; pentads 
(5am:test gct-dominant9th (5am:is (equalp (general-chord-type '(60 64 67 70 74) *tonal-consonance-vector*) '(0 (0 4 7 10 14)))))
(5am:test gct-major9th    (5am:is (equalp (general-chord-type '(60 64 67 71 74) *tonal-consonance-vector*) '(4 (0 3 7 10 20))))) ;; 
(5am:test gct-minor9th    (5am:is (equalp (general-chord-type '(60 63 67 70 74) *tonal-consonance-vector*) '(0 (0 3 7 10 14)))))


;;; Harmony: Parncutt (1988)

(5am:test parn88-encode-pc-set-1 (5am:is (equalp (encode-pc-set '(0 4 7)) '(1 0 0 0 1 0 0 1 0 0 0 0))))
(5am:test parn88-encode-pc-set-2 (5am:is (equalp (encode-pc-set '(0)) '(1 0 0 0 0 0 0 0 0 0 0 0))))
(5am:test parn88-encode-pc-set-3 (5am:is (equalp (encode-pc-set nil) '(0 0 0 0 0 0 0 0 0 0 0 0))))
(5am:test parn88-pc-weight-1 (5am:is (equalp (pc-weight 0 (encode-pc-set '(0 4 7)) *root-support-weights-parn06*) (+ 10 3 5))))
(5am:test parn88-pc-weight-2 (5am:is (equalp (pc-weight 1 (encode-pc-set '(0 4 7)) *root-support-weights-parn06*) 0)))
(5am:test parn88-pc-weight-3 (5am:is (equalp (pc-weight 2 (encode-pc-set '(0 4 7)) *root-support-weights-parn06*) (+ 2 1))))
(5am:test parn88-pc-weight-4 (5am:is (equalp (pc-weight 4 (encode-pc-set '(0 4 7)) *root-support-weights-parn06*) 10)))
(5am:test parn88-root-1 (5am:is (equalp (car (parn88 '(0 4 7))) 0)))
(5am:test parn88-root-2 (5am:is (equalp (car (parn88 '(1 4 9))) 9)))
(5am:test parn88-root-3 (5am:is (equalp (car (parn88 '(4 7 0))) 0)))
(5am:test parn88-root-4 (5am:is (equalp (car (parn88 '(7 2 5 11))) 7)))
(5am:test parn88-root-5 (5am:is (equalp (car (parn88 '(0 5 9))) 5)))
(5am:test parn88-root-6 (5am:is (equalp (car (parn88 '(50 60 64 69))) 2)))
(5am:test parn88-ambiguity (5am:is (> (second (parn88 '(0 3 6))) (second (parn88 '(0 4 7))))))

;; triads
(5am:test parn88-major      (5am:is (equalp (car (parn88 '(60 64 67))) 0)))
(5am:test parn88-minor      (5am:is (equalp (car (parn88 '(60 63 67))) 0)))
(5am:test parn88-diminished (5am:is (equalp (car (parn88 '(60 63 66))) 0)))
(5am:test parn88-augmented  (5am:is (equalp (car (parn88 '(60 64 68))) 0)))
(5am:test parn88-sus2       (5am:is (equalp (car (parn88 '(60 62 67))) 0)))
(5am:test parn88-sus4       (5am:is (equalp (car (parn88 '(60 65 67))) 5)))           ;;
;; tetrads 
(5am:test parn88-major7th          (5am:is (equalp (car (parn88 '(60 64 67 71))) 0)))
(5am:test parn88-minor7th          (5am:is (equalp (car (parn88 '(60 63 67 70))) 3))) ;; 
(5am:test parn88-dominant7th       (5am:is (equalp (car (parn88 '(60 64 67 70))) 0)))
(5am:test parn88-dominant7thsus4   (5am:is (equalp (car (parn88 '(60 65 67 70))) 0)))
(5am:test parn88-diminished7th     (5am:is (equalp (car (parn88 '(60 63 66 69))) 0)))
(5am:test parn88-halfdiminished7th (5am:is (equalp (car (parn88 '(60 63 66 70))) 3))) ;; 
(5am:test parn88-minormajor7th     (5am:is (equalp (car (parn88 '(60 63 67 71))) 0)))
(5am:test parn88-augmented7th      (5am:is (equalp (car (parn88 '(60 64 68 70))) 0)))
(5am:test parn88-major6th          (5am:is (equalp (car (parn88 '(60 64 67 69))) 0)))
(5am:test parn88-minor6th          (5am:is (equalp (car (parn88 '(60 63 67 69))) 0)))
;; pentads 
(5am:test parn88-dominant9th (5am:is (equalp (car (parn88 '(60 64 67 70 74))) 0)))
(5am:test parn88-major9th    (5am:is (equalp (car (parn88 '(60 64 67 71 74))) 0)))
(5am:test parn88-minor9th    (5am:is (equalp (car (parn88 '(60 63 67 70 74))) 0)))


;;; Harmony: viewpoints

(defun make-event (pitch)
  (make-instance 'md:music-event :cpitch pitch))

(defun make-slice (pitches)
  (make-instance 'md:music-slice :events (mapcar #'make-event pitches)))

(defun make-harmonic-sequence (pitches)
  (make-instance 'md:harmonic-sequence
                 :events (mapcar #'make-slice pitches)))


;; Harrison (2019, pp. 32-35)
(5am:test pi-chord-a        (5am:is (equalp (pi-chord      (make-harmonic-sequence '((52 60 67)))) '(52 60 67))))
(5am:test pc-set-a          (5am:is (equalp (pc-set        (make-harmonic-sequence '((52 60 67)))) '(0 4 7))))
(5am:test pc-chord-a        (5am:is (equalp (pc-chord      (make-harmonic-sequence '((52 60 67)))) '(4 (0 7)))))
(5am:test pi-chord-type-a   (5am:is (equalp (pi-chord-type (make-harmonic-sequence '((52 60 67)))) '(0 8 15))))
(5am:test pc-chord-type-a   (5am:is (equalp (pc-chord-type (make-harmonic-sequence '((52 60 67)))) '(0 3 8))))
(5am:test pc-set-type-a     (5am:is (equalp (pc-set-type   (make-harmonic-sequence '((52 60 67)))) '(0 4 7))))

(5am:test pi-chord-b        (5am:is (equalp (pi-chord      (make-harmonic-sequence '((43 52 60)))) '(43 52 60))))
(5am:test pc-set-b          (5am:is (equalp (pc-set        (make-harmonic-sequence '((43 52 60)))) '(0 4 7))))
(5am:test pc-chord-b        (5am:is (equalp (pc-chord      (make-harmonic-sequence '((43 52 60)))) '(7 (0 4)))))
(5am:test pi-chord-type-b   (5am:is (equalp (pi-chord-type (make-harmonic-sequence '((43 52 60)))) '(0 9 17))))
(5am:test pc-chord-type-b   (5am:is (equalp (pc-chord-type (make-harmonic-sequence '((43 52 60)))) '(0 5 9))))
(5am:test pc-set-type-b     (5am:is (equalp (pc-set-type   (make-harmonic-sequence '((43 52 60)))) '(0 4 7))))

(5am:test pi-chord-c        (5am:is (equalp (pi-chord      (make-harmonic-sequence '((50 59 64 68)))) '(50 59 64 68))))
(5am:test pc-set-c          (5am:is (equalp (pc-set        (make-harmonic-sequence '((50 59 64 68)))) '(2 4 8 11))))
(5am:test pc-chord-c        (5am:is (equalp (pc-chord      (make-harmonic-sequence '((50 59 64 68)))) '(2 (4 8 11)))))
(5am:test pi-chord-type-c   (5am:is (equalp (pi-chord-type (make-harmonic-sequence '((50 59 64 68)))) '(0 9 14 18))))
(5am:test pc-chord-type-c   (5am:is (equalp (pc-chord-type (make-harmonic-sequence '((50 59 64 68)))) '(0 2 6 9))))
(5am:test pc-set-type-c     (5am:is (equalp (pc-set-type   (make-harmonic-sequence '((50 59 64 68)))) '(0 3 6 8))))

(5am:test pc-set-d          (5am:is (equalp (pc-set        (make-harmonic-sequence '((54 62 69 74)))) '(2 6 9))))
;; NB Harrison (2019, p. 33, has (6, {2, 6, 9}) but hrep says Pitch-class chord: [6] 2 9
(5am:test pc-chord-d        (5am:is (equalp (pc-chord      (make-harmonic-sequence '((54 62 69 74)))) '(6 (2 9)))))
(5am:test pi-chord-type-d   (5am:is (equalp (pi-chord-type (make-harmonic-sequence '((54 62 69))))    '(0 8 15))))

(5am:test harmony-nf-1 (5am:is (equalp (normal-form '(0 7 9)) '(7 9 0))))
(5am:test harmony-nf-2 (5am:is (equalp (normal-form '(2 7 9)) '(7 9 2))))
(5am:test harmony-nf-3 (5am:is (equalp (normal-form '(2 6 7 11)) '(6 7 11 2))))


;; Harrison (2019, pp. 126-127)

(defun super-freak ()
  "Eight chord sequence from the song Super Freak by Rick James (1981)." 
  (let ((c1 '(57 60 64)) ;; A minor (A C E) 
        (c2 '(55 62 71)) ;; G major (G D B) 
        (c3 '(50 60 64 69))) ;; D C E A - A minor add 4 (A C E D) or D7 sus2 (D E A C) 
    (make-harmonic-sequence (list c1 c2 c1 c3 c1 c2 c1 c3))))

(defun super-freak-vs (attribute)
  "A viewpoint sequence for Super Freak corresponding to the supplied attribute." 
  (viewpoints:viewpoint-sequence
   (viewpoints:get-viewpoint attribute)
   (super-freak)))
 
(5am:test sf-bass-pc  (5am:is (equalp (super-freak-vs 'bass-pc) '(9 7 9 2 9 7 9 2))))
(5am:test sf-bass-int (5am:is (equalp (super-freak-vs 'bass-int) '(10 2 5 7 10 2 5))))
(5am:test sf-bass-pc-rel-root  (5am:is (equalp (super-freak-vs 'bass-pc-rel-root) '(0 0 0 0 0 0 0 0))))

(5am:test sf-root-pc  (5am:is (equalp (super-freak-vs 'root-pc) '(9 7 9 2 9 7 9 2))))
(5am:test sf-root-int (5am:is (equalp (super-freak-vs 'root-int) '(10 2 5 7 10 2 5))))

(5am:test sf-pc-set
  (5am:is
   (equalp (super-freak-vs 'pc-set)
           '((0 4 9) (2 7 11) (0 4 9) (0 2 4 9) (0 4 9) (2 7 11) (0 4 9) (0 2 4 9)))))

(5am:test sf-pc-set-rel-bass
  (5am:is
   (equalp (super-freak-vs 'pc-set-rel-bass)
           '((0 3 7) (0 4 7) (0 3 7) (0 2 7 10) (0 3 7) (0 4 7) (0 3 7) (0 2 7 10)))))

(5am:test sf-pc-set-rel-root
  (5am:is
   (equalp (super-freak-vs 'pc-set-rel-root)
           '((0 3 7) (0 4 7) (0 3 7) (0 2 7 10) (0 3 7) (0 4 7) (0 3 7) (0 2 7 10)))))

(5am:test sf-pc-set-rel-prev-bass
  (5am:is
   (equalp (super-freak-vs 'pc-set-rel-prev-bass)
           '((2 5 10) (2 5 9) (0 3 5 7) (2 7 10) (2 5 10) (2 5 9) (0 3 5 7)))))


(5am:test sf-pc-chord
  (5am:is
   (equalp (super-freak-vs 'pc-chord)
           '((9 (0 4)) (7 (2 11)) (9 (0 4)) (2 (0 4 9)) (9 (0 4)) (7 (2 11)) (9 (0 4)) (2 (0 4 9))))))

(5am:test sf-pc-chord-rel-prev-bass
  (5am:is
   (equalp (super-freak-vs 'pc-chord-rel-prev-bass)
           '((10 (2 5)) (2 (5 9)) (5 (0 3 7)) (7 (2 10)) (10 (2 5)) (2 (5 9)) (5 (0 3 7))))))






    




