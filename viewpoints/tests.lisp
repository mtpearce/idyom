;;;; ======================================================================
;;;; File:       tests.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-03-28 21:04:43 peter>                             
;;;; Time-stamp: <2017-03-28 21:09:31 peter>                           
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; This code defines tests for the viewpoints package.

(cl:in-package #:viewpoints)

(5am:def-suite viewpoints :description "Viewpoints")

;;;; ======================================================================
;;;; Harmony  =============================================================
;;;; ======================================================================

;;;======================
;;;* General chord type *
;;;======================

(5am:def-suite gct :description "General Chord Type (GCT)")

;;;; h-cpitch-class
(5am:def-suite h-cpitch-class
    :in gct :description "h-cpitch-class")
(5am:in-suite h-cpitch-class)
(5am:test h-cpitch-class-1
  (5am:is (equal (h-cpitch-class (make-harmonic-seq '((0 4 10))))
		 '(0 4 11))))
(5am:test h-cpitch-class-2
  (5am:is (equal (h-cpitch-class (make-harmonic-seq '((48 50 52))))
		 '(0 2 4))))
(5am:test h-cpitch-class-3
  (5am:is (equal (h-cpitch-class (make-harmonic-seq '((48 50 52) (17 19 21))))
		 '(5 7 9))))
(5am:test h-cpitch-class-4
  (5am:is (equal (h-cpitch-class (make-harmonic-seq '((48 60 62))))
		 '(0 0 1))))

;;;; h-cpitch-class-set
(5am:def-suite h-cpitch-class-set
    :in gct :description "h-cpitch-class-set")
(5am:in-suite h-cpitch-class-set)
(5am:test h-cpitch-class-set-1
  (5am:is (equal (h-cpitch-class (make-harmonic-seq '((0 4 10))))
		 '(0 4 11))))
(utils::add-test-dependency 'h-cpitch-class-set 'h-cpitch-class)
