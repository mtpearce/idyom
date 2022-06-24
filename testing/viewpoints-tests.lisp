;;;; ======================================================================
;;;; File:       utils-tests.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2022-06-23 13:17:12 peter>                         
;;;; Time-stamp: <2022-06-23 15:08:51 marcusp>                           
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; This code defines tests for the viewpoints package.

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
(5am:test gct-halfdiminished7th (5am:is (equalp (general-chord-type '(60 63 66 70) *tonal-consonance-vector*) '(3 (0 3 7 9)))))
(5am:test gct-minormajor7th     (5am:is (equalp (general-chord-type '(60 63 67 71) *tonal-consonance-vector*) '(0 (0 3 7 11)))))
(5am:test gct-augmented7th      (5am:is (equalp (general-chord-type '(60 64 68 70) *tonal-consonance-vector*) '(0 (0 4 8 10)))))
(5am:test gct-major6th          (5am:is (equalp (general-chord-type '(60 64 67 69) *tonal-consonance-vector*) '(9 (0 3 7 10)))))
(5am:test gct-minor6th          (5am:is (equalp (general-chord-type '(60 63 67 69) *tonal-consonance-vector*) '(0 (0 3 7 9)))))
;; pentads 
(5am:test gct-dominant9th (5am:is (equalp (general-chord-type '(60 64 67 70 74) *tonal-consonance-vector*) '(0 (0 4 7 10 14)))))
(5am:test gct-major9th    (5am:is (equalp (general-chord-type '(60 64 67 71 74) *tonal-consonance-vector*) '(4 (0 3 7 10 20)))))
(5am:test gct-minor9th    (5am:is (equalp (general-chord-type '(60 63 67 70 74) *tonal-consonance-vector*) '(0 (0 3 7 10 14)))))







