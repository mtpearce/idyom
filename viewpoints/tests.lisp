;;;; ======================================================================
;;;; File:       tests.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-03-28 21:04:43 peter>                             
;;;; Time-stamp: <2017-03-30 15:29:34 peter>                           
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; This code defines tests for the viewpoints package.

(cl:in-package #:viewpoints)

(5am:def-suite viewpoints :description "Viewpoints")

;;;; ======================================================================
;;;; Macros ===============================================================
;;;; ======================================================================

(defun make-viewpoint-tests (viewpoint test-specs
			     &key (test #'equal) (parent-suite 'viewpoints)
			       depends-on)
  "Defines a set of tests for <viewpoint>. Each test takes the form 
   of a given input, a musical sequence which the viewpoint will be
   computed on, and a desired output, which will be checked for equality
   with the result of applying <viewpoint> to the input using equality
   predicate <test>. <viewpoint> should be a symbol identifying the viewpoint
   to be tested. <test-specs> should be a list identifying the set 
   of tests to be run. Each element of <test-specs> should itself be 
   a list corresponding to exactly one test. The first element of this
   element should be the test input; the second element should be the 
   desired output; the third element (optional) should be a string
   providing a written description of the input. Tests will be defined
   using the 5am package, and will have labels corresponding to the 
   name of the viewpoint followed by 'ex' followed by a unique identifying 
   number. All created tests will be put in a suite named after the viewpoint;
   by default this suite will be a child of the viewpoints test suite,
   but by specifying <parent-suite> it is possible to nest the tests
   within a different suite. If not null, <depends-on> should be either a
   symbol or a list of symbols identifying viewpoints which the current 
   viewpoint depends on, and hence the current viewpoint's tests should 
   depend on this viewpoint. Note: tests must have already been defined
   for the viewpoint which is depended on (e.g. higher up the code page).
   Example usage:
   (make-viewpoint-tests 
     'local-tonic
     '(((harm-seq '((0 5 9) (2 5 7 11) (0 4 7) (0))) 0 \"a IV-V-I cadence\")
       ((harm-seq '((0 5 8) (2 5 7 11) (0 3 7) (0))) 0 \"a iv-V-i cadence\")))"
  (eval `(5am:def-suite ,viewpoint :in ,parent-suite))
  (eval `(5am:in-suite ,viewpoint))
  (loop
     for test-spec in test-specs
     for n from 1
     do (eval 
	 (let* ((test-name (intern (format nil "~A-EX-~A" (symbol-name viewpoint) n)))
		(input (first test-spec))
		(desired-output (second test-spec))
		(description (third test-spec))
		(fail-msg (format nil "Failed to compute ~A for ~A."
				  viewpoint
				  (if description description input))))
	   `(5am:test ,test-name
	      (5am:is (funcall ,test (,viewpoint ,input) ,desired-output)
		      ,fail-msg)))))
  (let ((depends-on-list
	 (cond ((listp depends-on) depends-on)
	       ((symbolp depends-on) (list depends-on))
	       (t (error "<depends-on> must either be a symbol or a list.")))))
    (dolist (dependency depends-on-list)
      (if (null (eval `(5am:get-test ,dependency)))
	  (error "Tried to add dependency to empty test suite."))
      (eval `(utils:add-test-suite-dependency ',viewpoint ',dependency)))))
      
	      
;;;; ======================================================================
;;;; Harmony  =============================================================
;;;; ======================================================================

;;;======================
;;;* Key-finding *
;;;======================

(5am:def-suite key-finding :in viewpoints)

;;;; local-tonic
(5am:def-suite local-tonic :in key-finding)
(5am:in-suite local-tonic)

;; Note that the local tonic computations exclude the final event
;; in the harmonic sequence.

;; IV-V-I cadence
(5am:test local-tonic-1
  (5am:is (equal (local-tonic
		  (make-harmonic-seq '((0 5 9) (2 5 7 11) (0 4 7) (0))))
		 0) "Failed to compute the correct tonic for a IV-V-I cadence"))
;; iv-V-i cadence
(5am:test local-tonic-2
  (5am:is (equal (local-tonic
		  (make-harmonic-seq '((0 5 8) (2 5 7 11) (0 3 7) (0))))
		 0) "Failed to compute the correct tonic for a iv-V-i cadence"))
;; tranposed IV-V-I cadence
(5am:test local-tonic-3
  (5am:is (equal (local-tonic
		  (make-harmonic-seq '((46 62 77) (24 31 40 46) (29 45 60) (0))))
		 5) "Failed to compute the correct tonic for a transposed IV-V-I cadence"))
;; transposed iv-V-i cadence
(5am:test local-tonic-4
  (5am:is (equal (local-tonic
		  (make-harmonic-seq '((46 61 77) (24 31 40 46) (29 44 60) (0))))
		 5) "Failed to compute the correct tonic for a transposed iv-V-i cadence"))

;;;; local-mode
(5am:def-suite local-mode :in key-finding)
(5am:in-suite local-mode)

;; Note that the local mode computations exclude the final event
;; in the harmonic sequence.

;; IV-V-I cadence
(5am:test local-mode-1
  (5am:is (equal (local-mode
		  (make-harmonic-seq '((0 5 9) (2 5 7 11) (0 4 7) (0))))
		 'major) "Failed to compute the correct tonic for a IV-V-I cadence"))
;; iv-V-i cadence
(5am:test local-mode-2
  (5am:is (equal (local-mode
		  (make-harmonic-seq '((0 5 8) (2 5 7 11) (0 3 7) (0))))
		 'minor) "Failed to compute the correct mode for a iv-V-i cadence"))
;; transposed IV-V-I cadence
(5am:test local-mode-3
  (5am:is (equal (local-mode
		  (make-harmonic-seq '((46 62 77) (24 31 40 46) (29 45 60) (0))))
		 'major) "Failed to compute the correct tonic for a transposed IV-V-I cadence"))
;; transposed iv-V-i cadence
(5am:test local-mode-4
  (5am:is (equal (local-mode
		  (make-harmonic-seq '((46 61 77) (24 31 40 46) (29 44 60) (0))))
		 'minor) "Failed to compute the correct mode for a transposed iv-V-i cadence"))

;;;======================
;;;* General chord type *
;;;======================

(5am:def-suite gct :in viewpoints :description "General Chord Type (GCT)")

;; Note: stage 2 of the GCT algorithm (hypothesis selection) is not yet very well tested

;;;; h-cpitch-class
(5am:def-suite h-cpitch-class :in gct)
(5am:in-suite h-cpitch-class)
(5am:test h-cpitch-class-1
  (5am:is (equal (h-cpitch-class (make-harmonic-seq '((0 4 10))))
		 '(0 4 10))
	  "Failed to compute the h-cpitch-class of '(0 4 10) correctly"))
(5am:test h-cpitch-class-2
  (5am:is (equal (h-cpitch-class (make-harmonic-seq '((48 50 52))))
		 '(0 2 4))
	  "Failed to compute the h-cpitch-class of '(0 2 4) correctly"))
(5am:test h-cpitch-class-3
  (5am:is (equal (h-cpitch-class (make-harmonic-seq '((48 50 52) (17 19 21))))
		 '(5 7 9))
	  "Failed to compute the h-cpitch-class of '((48 50 52) (17 19 21)) correctly"))
(5am:test h-cpitch-class-4
  (5am:is (equal (h-cpitch-class (make-harmonic-seq '((48 60 62))))
		 '(0 0 2))
	  "Failed to compute the h-cpitch-class of '(48 60 62) correctly"))

;;;; h-cpitch-class-set
(5am:def-suite h-cpitch-class-set :in gct)
(5am:in-suite h-cpitch-class-set)
(5am:test h-cpitch-class-set-1
  (5am:is (equal (h-cpitch-class-set (make-harmonic-seq '((0 4 10))))
		 '(0 4 10))
	  "Failed to compute the h-cpitch-class-set of '(0 4 10) correctly"))
(5am:test h-cpitch-class-set-2
  (5am:is (equal (h-cpitch-class-set (make-harmonic-seq '((12 26 53))))
		 '(0 2 5))
	  "Failed to compute the h-cpitch-class-set of '(0 2 5) correctly"))
(5am:test h-cpitch-class-set-3
  (5am:is (equal (h-cpitch-class-set (make-harmonic-seq '((53 41 31))))
		 '(5 7))
	  "Failed to compute the h-cpitch-class-set of '(53 41 31) correctly"))
(5am:test h-cpitch-class-set-4
  (5am:is (equal (h-cpitch-class-set (make-harmonic-seq '((0 4 10) (12 26 53) (53 41 31))))
		 '(5 7))
	  "Failed to compute the h-cpitch-class-set of '((0 4 10) (12 26 53) (53 41 31)) correctly"))
(utils:add-test-suite-dependency 'h-cpitch-class-set 'h-cpitch-class)

;;;; general-chord-type
(5am:def-suite general-chord-type :in gct)
(5am:in-suite general-chord-type)

(5am:test general-chord-type-1
  (5am:is (equal (general-chord-type '(0 5 9)                       ; pitch-class set
				     5                              ; bass-pc
				     (list 0 *major-scale-degrees*) ; pitch-scale-hierarchy
				     *common-practice-consonance-vector*)
		 (list (cons :root-csd 5)
		       (cons :base '(4 7))
		       (cons :ext nil)
		       (cons :tonic 0)))
	  "Failed to compute the GCT representation of an F major chord"))
(5am:test general-chord-type-2
  (5am:is (equal (general-chord-type '(0 2 6 9)                     ; pitch-class set
				     0                              ; bass-pc
				     (list 7 *major-scale-degrees*) ; pitch-scale-hierarchy
				     *common-practice-consonance-vector*)
		 (list (cons :root-csd 7)
		       (cons :base '(4 7))
		       (cons :ext '(10))
		       (cons :tonic 7)))
	  "Failed to compute the GCT representation of a dominant seventh chord"))
(5am:test general-chord-type-3
  (5am:is (equal (general-chord-type '(0 2 6 9)                    ; pitch-class set
				     0                             ; bass-pc
				     nil                           ; pitch-scale-hierarchy
				     *common-practice-consonance-vector*)
		 (list (cons :root-csd 2)
		       (cons :base '(4 7))
		       (cons :ext '(10))
		       (cons :tonic nil)))
	  "Failed to compute the GCT representation of a seventh chord without pitch-scale-hierarchy"))


;;;; h-gct
(5am:def-suite h-gct :in gct)
(5am:in-suite h-gct)

(5am:test h-gct-1
  (5am:is (equal (h-gct (make-harmonic-seq '((60 64 67))))
		 (list (cons :root-csd 0)
		       (cons :base '(4 7))
		       (cons :ext nil)
		       (cons :tonic nil)))
	  "Failed to compute the GCT representation for a C major chord in root position"))
(5am:test h-gct-2
  (5am:is (equal (h-gct (make-harmonic-seq '((0 5 9) (2 5 7 11) (0 4 7) (35 55 65 74))))
		 (list (cons :root-csd 7)
		       (cons :base '(4 7))
		       (cons :ext '(10))
		       (cons :tonic 0)))
	  "Failed to compute the GCT representation for an inverted dominant seventh chord in C major"))
(5am:test h-gct-3
  (5am:for-all ((cpitch (5am:gen-list :length (5am:gen-integer :min 1 :max 10)
				      :elements (5am:gen-integer :min 0 :max 127))))
    (let* ((context (list '(0 5 9) '(2 5 7 11) '(0 4 7)))
	   (chords (append context (list cpitch)))
	   (chords-copy (mapcar #'copy-list chords))
	   (events (make-harmonic-seq chords))
	   (events-copy (make-harmonic-seq chords-copy)))
      (5am:is (equal (h-gct events) (h-gct events-copy))))))

(utils:add-test-suite-dependency 'h-gct 'general-chord-type)
(utils:add-test-suite-dependency 'h-gct 'key-finding)

;;;; h-gct-root-cpc
(5am:def-suite h-gct-root-cpc :in gct)
(5am:in-suite h-gct-root-cpc)

(5am:test h-gct-root-cpc-1
  (5am:is (equal (h-gct-root-cpc (make-harmonic-seq '((0 4 7)))) 0)
	  "Failed to compute the GCT root pitch class of a C major chord in root position"))
(5am:test h-gct-root-cpc-2
  (5am:is (equal (h-gct-root-cpc (make-harmonic-seq '((16 55 60)))) 0)
	  "Failed to compute the GCT root pitch class of a C major chord in first inversion"))
(5am:test h-gct-root-cpc-3
  (5am:is (equal (h-gct-root-cpc (make-harmonic-seq '((16 45 60)))) 9)
	  "Failed to compute the GCT root pitch class of a A minor chord in second inversion"))
(5am:test h-gct-root-cpc-4
  (5am:is (equal (h-gct-root-cpc (make-harmonic-seq '((14 17 20 23)))) 2)
	  "Failed to compute the GCT root pitch class of a diminished chord with D in the bass"))
(5am:test h-gct-root-cpc-5
  (5am:is (equal (h-gct-root-cpc (make-harmonic-seq '((17 20 23 26)))) 5)
	  "Failed to compute the GCT root pitch class of a diminished chord with F in the bass"))

(utils:add-test-suite-dependency 'h-gct-root-cpc 'h-gct)

;;;; h-gct-root-cpcint
(5am:def-suite h-gct-root-cpcint :in gct)
(5am:in-suite h-gct-root-cpcint)

(5am:test h-gct-root-cpcint-1
  (5am:is (equal (h-gct-root-cpcint
		  (make-harmonic-seq '((2 5 7 11) (0 4 7))))
		 5) "Failed to compute the correct root interval progression for a V-I cadence"))
(5am:test h-gct-root-cpcint-2
  (5am:is (equal (h-gct-root-cpcint
		  (make-harmonic-seq '((17 44 60) (18 33 38))))
		 9) "Failed to compute the correct root interval progression for a iii-I cadence"))
(5am:test h-gct-root-cpcint-3
  (5am:is (equal (h-gct-root-cpcint
		  (make-harmonic-seq '((50 53 57) (21 24 28))))
		 7) "Failed to compute the correct root interval progression for a iv-i cadence"))

(utils:add-test-suite-dependency 'h-gct-root-cpcint 'h-gct)

;;;; h-gct-root-csd


;;;; h-gct-base
(5am:def-suite h-gct-base :in gct)
(5am:in-suite h-gct-base)

(5am:test h-gct-base-1
  (5am:is (equal (h-gct-base (make-harmonic-seq '((0 4 7)))) '(4 7))
	  "Failed to compute the GCT base of a C major chord in root position"))
(5am:test h-gct-base-2
  (5am:is (equal (h-gct-base (make-harmonic-seq '((16 55 60)))) '(4 7))
	  "Failed to compute the GCT base of a C major chord in first inversion"))
(5am:test h-gct-base-3
  (5am:is (equal (h-gct-base (make-harmonic-seq '((16 45 60)))) '(3 7))
	  "Failed to compute the GCT base of a A minor chord in second inversion"))
(5am:test h-gct-base-4
  (5am:is (equal (h-gct-base (make-harmonic-seq '((14 17 20 23)))) '(3))
	  "Failed to compute the GCT base of a diminished chord with D in the bass"))
(5am:test h-gct-base-5
  (5am:is (equal (h-gct-base (make-harmonic-seq '((17 20 23 26)))) '(3))
	  "Failed to compute the GCT base of a diminished chord with F in the bass"))

(utils:add-test-suite-dependency 'h-gct-base 'h-gct)

;;;; h-gct-ext
(5am:def-suite h-gct-ext :in gct)
(5am:in-suite h-gct-ext)
(5am:test h-gct-ext-1
  (5am:is (equal (h-gct-ext (make-harmonic-seq '((0 4 7 10)))) '(10))
	  "Failed to compute the GCT extension of a C major seventh chord in root position"))
(5am:test h-gct-ext-2
  (5am:is (equal (h-gct-ext (make-harmonic-seq '((14 17 20 23)))) '(6 9))
	  "Failed to compute the GCT extension of a diminished chord with D in the bass"))
(5am:test h-gct-ext-3
  (5am:is (equal (h-gct-ext (make-harmonic-seq '((17 20 23 26)))) '(6 9))
	  "Failed to compute the GCT extension of a diminished chord with F in the bass"))

(utils:add-test-suite-dependency 'h-gct-ext 'h-gct)


