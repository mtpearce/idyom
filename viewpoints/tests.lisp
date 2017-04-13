;;;; ======================================================================
;;;; File:       tests.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-03-28 21:04:43 peter>                             
;;;; Time-stamp: <2017-04-13 18:07:50 peter>                           
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
   within a different suite, which should be defined in advance. If not
   null, <depends-on> should be either a symbol or a list of
   symbols identifying viewpoints which the current viewpoint 
   depends on, and hence the current viewpoint's tests should 
   depend on this viewpoint.
   Note: tests must have already been defined
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
	      (5am:is (funcall ,test
			       (,viewpoint ,input)
			       ,desired-output)
		      ,fail-msg)))))
  (eval `(utils::set-test-suite-dependencies ',viewpoint ',depends-on)))
      
	      
;;;; ======================================================================
;;;; Harmony  =============================================================
;;;; ======================================================================

(5am:def-suite harmony :description "Harmony viewpoints" :in viewpoints)

;;;=============
;;;* General *
;;;=============

;; h-cpitch
(make-viewpoint-tests 
 'h-cpitch
 '(((harm-seq '((0 4 10))) '(0 4 10))
   ((harm-seq '((4 10 0))) '(0 4 10))
   ((harm-seq '((40 30 20))) '(20 30 40)))
 :parent-suite 'harmony)

;; h-cpitch-class
(make-viewpoint-tests 
 'h-cpitch-class
 '(((harm-seq '((0 4 10))) '(0 4 10))
   ((harm-seq '((48 50 52))) '(0 2 4))
   ((harm-seq '((48 50 52))) '(0 2 4))
   ((harm-seq '((48 60 62))) '(0 0 2)))
 :parent-suite 'harmony)

;; h-cpitch-class-set
(make-viewpoint-tests 
 'h-cpitch-class-set
 '(((harm-seq '((0 4 10))) '(0 4 10))
   ((harm-seq '((12 26 53))) '(0 2 5))
   ((harm-seq '((53 41 31))) '(5 7))
   ((harm-seq '((0 4 10) (12 26 53) (53 41 31))) '(5 7)))
 :parent-suite 'harmony
 :depends-on 'h-cpitch-class)

;; h-bass-cpitch
(make-viewpoint-tests 
 'h-bass-cpitch
 '(((harm-seq '((0 4 10))) 0)
   ((harm-seq '((12 26 53))) 12)
   ((harm-seq '((53 41 31))) 31)
   ((harm-seq '((0 4 10) (12 26 53) (53 41 31))) 31)
   ((harm-seq '((0 4 10) (12 26 53) (27 30 50))) 27))
 :parent-suite 'harmony
 :depends-on 'h-cpitch)

;; h-bass-cpint
(make-viewpoint-tests 
 'h-bass-cpint
 '(((harm-seq '((0 4 10))) +undefined+)
   ((harm-seq '((0 4 10) (12 20 30))) 12)
   ((harm-seq '((20 30) (22 25) (18 25))) -4)
   ((harm-seq '((0 4) (6 0))) 0))
 :parent-suite 'harmony
 :depends-on 'h-bass-cpitch)

;; h-bass-cpc
(make-viewpoint-tests 
 'h-bass-cpc
 '(((harm-seq '((0 4 10))) 0)
   ((harm-seq '((12 26 53))) 0)
   ((harm-seq '((53 41 31))) 7)
   ((harm-seq '((0 4 10) (12 26 53) (53 41 31))) 7)
   ((harm-seq '((0 4 10) (12 26 53) (27 30 50))) 3))
 :parent-suite 'harmony
 :depends-on 'h-bass-cpitch)

;; h-bass-cpcint
(make-viewpoint-tests 
 'h-bass-cpcint
 '(((harm-seq '((0 4 10))) +undefined+)
   ((harm-seq '((0 4 10) (5 20 30))) 5)
   ((harm-seq '((0 4 10) (12 20 30))) 0)
   ((harm-seq '((20 30) (22 25) (18 25))) 8)
   ((harm-seq '((0 4) (6 0))) 0)
   ((harm-seq '((0 4) (25 30))) 1))
 :parent-suite 'harmony
 :depends-on 'h-bass-cpc)

;; h-cpc-int-from-bass
(make-viewpoint-tests
 'h-cpc-int-from-bass
 '(((harm-seq '((0 4 7 10))) '(4 7 10)
    "a C7 chord in root position")
   ((harm-seq '((14 17 20 23))) '(3 6 9)
    "a diminished chord, bass D")
   ((harm-seq '((17 20 23 26))) '(3 6 9)
    "a diminished chord, bass F")
   ((harm-seq '((64 67 70 72))) '(3 6 8)
    "a C7 chord in first inversion")
   ((harm-seq '((64 70 72 79))) '(3 6 8)
    "a C7 chord in first inversion"))
 :parent-suite 'harmony) ;; add depends on bass

;;;======================
;;;* Key-finding *
;;;======================

(5am:def-suite key-finding :in viewpoints)

;; Note that the local key computations exclude the final event
;; in the harmonic sequence.

;; local-key
(5am:def-suite local-key :in key-finding)
(5am:in-suite local-key)
(5am:test local-key-ex-1
  (5am:is (equal (let* ((key (local-key (harm-seq '((0 5 9) (2 5 7 11)
						   (0 4 7) (0)))))
		       (tonic (assoc :tonic key))
		       (mode (assoc :mode key)))
		   (list tonic mode))
		 (list (cons :tonic 0) (cons :mode 'major)))))
(5am:test local-key-ex-2
  (5am:is (equal (let* ((key (local-key (harm-seq '((0 5 8) (2 5 7 11)
						   (0 3 7) (0)))))
		       (tonic (assoc :tonic key))
		       (mode (assoc :mode key)))
		   (list tonic mode))
		 (list (cons :tonic 0) (cons :mode 'minor)))))
(5am:test local-key-ex-3
  (5am:is (equal (let* ((key (local-key (harm-seq '((2 7 10) (4 7 9 13)
						   (2 5 9) (2)))))
		       (tonic (assoc :tonic key))
		       (mode (assoc :mode key)))
		   (list tonic mode))
		 (list (cons :tonic 2) (cons :mode 'minor)))))

;; local-tonic
(make-viewpoint-tests 
 'local-tonic
 '(((harm-seq '((0 5 9) (2 5 7 11) (0 4 7) (0))) 0 "a IV-V-I cadence")
   ((harm-seq '((0 5 8) (2 5 7 11) (0 3 7) (0))) 0 "a iv-V-i cadence")
   ((harm-seq '((46 62 77) (24 31 40 46) (29 45 60) (0))) 5 "a IV-V-I cadence")
   ((harm-seq '((46 61 77) (24 31 40 46) (29 44 60) (0))) 5 "a iv-V-i cadence"))
 :parent-suite 'key-finding
 :depends-on 'local-key)

;; local-mode
(make-viewpoint-tests 
 'local-mode
 '(((harm-seq '((0 5 9) (2 5 7 11) (0 4 7) (0))) 'major "a IV-V-I cadence")
   ((harm-seq '((0 5 8) (2 5 7 11) (0 3 7) (0))) 'minor "a iv-V-i cadence")
   ((harm-seq '((46 62 77) (24 31 40 46) (29 45 60) (0))) 'major "a IV-V-I cadence")
   ((harm-seq '((46 61 77) (24 31 40 46) (29 44 60) (0))) 'minor "a iv-V-i cadence"))
 :parent-suite 'key-finding
 :depends-on 'local-key)

;;;======================
;;;* Scale degrees *
;;;======================

;; h-csd
(make-viewpoint-tests 
 'h-csd
 '(((harm-seq '((0 5 9) (2 5 7 11) (0 4 7) (0 4 7))) '(0 4 7))
   ((harm-seq '((0 5 8) (2 5 7 11) (0 3 7) (0 4 7))) '(0 4 7))
   ((harm-seq '((46 62 77) (24 31 40 46) (5 9 12) (0 3 7))) '(2 7 10) "chord v of F major")
   ((harm-seq '((46 62 77) (24 31 40 46) (5 9 12) (34 41 49))) '(0 5 8) "chord iv of F major")
   ((harm-seq '((46 62 77) (24 31 40 46) (5 9 12) (34 41 49 61))) '(0 5 8) "chord iv of F major (third doubled)")
   ((harm-seq '((46 62 77) (24 31 40 46) (5 9 12) (34 41 49 49))) '(0 5 8) "chord iv of F major (third doubled)"))
 :parent-suite 'key-finding
 :depends-on 'local-key)

;; h-bass-csd
(make-viewpoint-tests 
 'h-bass-csd
 '(((harm-seq '((0 5 9) (2 5 7 11) (0 4 7) (0 4 7))) 0)
   ((harm-seq '((0 5 8) (2 5 7 11) (0 3 7) (0 4 7))) 0)
   ((harm-seq '((46 62 77) (24 31 40 46) (5 9 12) (0 3 7))) 7 "chord v of F major")
   ((harm-seq '((46 62 77) (24 31 40 46) (5 9 12) (34 41 49))) 5 "chord iv of F major")
   ((harm-seq '((46 62 77) (24 31 40 46) (5 9 12) (34 41 49 61))) 5 "chord iv of F major (third doubled)")
   ((harm-seq '((46 62 77) (24 31 40 46) (5 9 12) (34 41 49 49))) 5 "chord iv of F major (third doubled)"))
 :parent-suite 'key-finding
 :depends-on '(local-key h-bass-cpitch))

;;;======================
;;;* General chord type *
;;;======================

(5am:def-suite gct :in harmony :description "General Chord Type (GCT)")

;; Note: stage 2 of the GCT algorithm (hypothesis selection) is not yet very well tested

;; general-chord-type
(5am:def-suite general-chord-type :in gct)
(5am:in-suite general-chord-type)
(5am:test general-chord-type-ex-1
  (5am:is
   (equal(general-chord-type
	  '(0 5 9)                       ; pitch-class set
	  5                              ; bass-pc
	  (list 0 *major-scale-degrees*) ; pitch-scale-hierarchy
	  *common-practice-consonance-vector*)
	 (list (cons :root-csd 5)
	       (cons :base '(4 7))
	       (cons :ext nil)
	       (cons :tonic 0)))
   "Failed to compute the GCT representation of an F major chord"))
(5am:test general-chord-type-ex-2
  (5am:is
   (equal (general-chord-type
	   '(0 2 6 9)                     ; pitch-class set
	   0                              ; bass-pc
	   (list 7 *major-scale-degrees*) ; pitch-scale-hierarchy
	   *common-practice-consonance-vector*)
	  (list (cons :root-csd 7)
		(cons :base '(4 7))
		(cons :ext '(10))
		(cons :tonic 7)))
   "Failed to compute the GCT representation of a dominant seventh chord"))
(5am:test general-chord-type-ex-3
  (5am:is
   (equal (general-chord-type
	   '(0 2 6 9)                    ; pitch-class set
	   0                             ; bass-pc
	   nil                           ; pitch-scale-hierarchy
	   *common-practice-consonance-vector*)
	  (list (cons :root-csd 2)
		(cons :base '(4 7))
		       (cons :ext '(10))
		       (cons :tonic nil)))
	  "Failed to compute the GCT representation of a seventh chord without pitch-scale-hierarchy"))

;; h-gct
(make-viewpoint-tests 
 'h-gct
 '(((harm-seq '((60 64 67)))
    (list (cons :root-csd 0)
     (cons :base '(4 7))
     (cons :ext nil)
     (cons :tonic nil))
    "a C major chord in root position")
   ((harm-seq '((0 5 9) (2 5 7 11) (0 4 7) (35 55 65 74)))
    (list (cons :root-csd 7)
     (cons :base '(4 7))
     (cons :ext '(10))
     (cons :tonic 0))
    "an inverted dominant seventh chord in C major"))
 :parent-suite 'gct
 :depends-on '(general-chord-type key-finding))

(5am:in-suite gct)
(5am:test h-gct-ex-3
  (5am:for-all ((cpitch (5am:gen-list
			 :length (5am:gen-integer :min 1 :max 10)
			 :elements (5am:gen-integer :min 0 :max 127))))
    (let* ((context (list '(0 5 9) '(2 5 7 11) '(0 4 7)))
	   (chords (append context (list cpitch)))
	   (chords-copy (mapcar #'copy-list chords))
	   (events (harm-seq chords))
	   (events-copy (harm-seq chords-copy)))
      (5am:is (equal (h-gct events) (h-gct events-copy))))))
(utils:set-test-suite-dependencies
 'h-gct '(general-chord-type key-finding))

;; h-gct-root-cpc
(make-viewpoint-tests 
 'h-gct-root-cpc
 '(((harm-seq '((0 4 7))) 0 "a C major chord in root position")
   ((harm-seq '((16 55 60))) 0 "a C major chord in first inversion")
   ((harm-seq '((16 45 60))) 9 "an A minor chord in second inversion")
   ((harm-seq '((14 17 20 23))) 2 "a diminished chord with D in the bass")
   ((harm-seq '((17 20 23 26))) 5 "a diminished chord with F in the bass"))
 :parent-suite 'gct
 :depends-on 'h-gct)

;;;; h-gct-root-cpcint
(make-viewpoint-tests 
 'h-gct-root-cpcint
 '(((harm-seq '((2 5 7 11) (0 4 7))) 5 "a V-I cadence")
   ((harm-seq '((17 44 60) (18 33 38))) 9 "a iii-I cadence")
   ((harm-seq '((50 53 57) (21 24 28))) 7 "a iv-i cadence"))
 :parent-suite 'gct
 :depends-on 'h-gct)

;;;; h-gct-root-5ths-dist
(make-viewpoint-tests 
 'h-gct-root-5ths-dist
 '(((harm-seq '((0 4 7))) +undefined+)
   ((harm-seq '((2 5 7 11) (0 4 7)) :ref-pitch 10)
    1 "a V-I cadence")
   ((harm-seq '((0 4 7) (5 10 14)) :ref-pitch 20)
    2 "a I ->  bVII progression")
   ((harm-seq '((4 7 12) (9 13 16)) :ref-pitch 60)
    3 "a I ->  VI progression")
   ((harm-seq '((0 4 7) (8 11 16)) :ref-pitch 27)
    4 "a I ->  III progression")
   ((harm-seq '((0 4 7) (1 5 8)) :ref-pitch 15)
    5 "a I ->  bII progression"))
 :parent-suite 'gct
 :depends-on 'h-gct-root-cpcint)

;;;; h-gct-meeus-int
(make-viewpoint-tests 
 'h-gct-meeus-int
 '(((harm-seq '((0 4 7))) +undefined+)
   ((harm-seq '((0 4 7) (12 16 19)) :ref-pitch 70)
    'static "a I-I cadence")
   ((harm-seq '((2 5 7 11) (0 4 7)) :ref-pitch 10)
    'dominant "a V-I cadence")
   ((harm-seq '((5 9 12) (0 4 7)) :ref-pitch 15)
    'subdominant "a IV-I cadence")
   ((harm-seq '((0 4 7) (5 10 14)) :ref-pitch 20)
    'subdominant "a I ->  bVII progression")
   ((harm-seq '((4 7 12) (9 13 16)) :ref-pitch 60)
    'dominant "a I ->  VI progression")
   ((harm-seq '((0 4 7) (8 11 16)) :ref-pitch 27)
    'subdominant "a I ->  III progression")
   ((harm-seq '((0 4 7) (1 5 8)) :ref-pitch 15)
    'dominant "a I ->  bII progression")
    ((harm-seq '((0 4 7) (6 10 13)) :ref-pitch 16)
    'tritone "a I -> #IV progression"))
 :parent-suite 'gct
 :depends-on 'h-gct-root-cpcint)

;;;; h-gct-root-csd
(make-viewpoint-tests 
 'h-gct-root-csd
 '(((harm-seq '((0 5 9) (2 5 7 11) (0 4 7) (0 4 7) (0 5 9))) 5
    "a I-IV progression")
   ((harm-seq '((0 5 9) (2 5 7 11) (0 4 7) (0 5 9) (0 4 7))) 7
    "a IV-I progression")
   ((harm-seq '((0 5 9) (2 5 7 11) (0 4 7) (60 64 67) (62 65 69))) 2
    "a I-ii progression")
   ((harm-seq '((2 7 11) (4 7 9 13) (2 6 9) (62 66 69) (64 68 71))) 2
		"a I-II progression"))
 :parent-suite 'gct
 :depends-on 'h-gct)

;;;; h-gct-base
(make-viewpoint-tests 
 'h-gct-base
 '(((harm-seq '((0 4 7))) '(4 7) "a C major chord in root position")
   ((harm-seq '((16 55 60))) '(4 7) "a C major chord in first inversion")
   ((harm-seq '((16 45 60))) '(3 7) "an A minor chord in second inversion")
   ((harm-seq '((14 17 20 23))) '(3) "a diminished chord, bass D")
   ((harm-seq '((17 20 23 26))) '(3) "a diminished chord, bass F"))
 :parent-suite 'gct
 :depends-on 'h-gct)

;;;; h-gct-ext
(make-viewpoint-tests 
 'h-gct-ext
 '(((harm-seq '((0 4 7 10))) '(10) "a C7 chord in root position")
   ((harm-seq '((14 17 20 23))) '(6 9) "a diminished chord, bass D")
   ((harm-seq '((17 20 23 26))) '(6 9) "a diminished chord, bass F"))
 :parent-suite 'gct
 :depends-on 'h-gct)

;; h-bass-int-from-gct-root
(make-viewpoint-tests 
 'h-bass-int-from-gct-root
 '(((harm-seq '((0 4 7 10))) 0
    "a C7 chord in root position")
   ((harm-seq '((4 7 10 12))) 4
    "a C7 chord in first inversion")
   ((harm-seq '((7 10 12 16))) 7
    "a C7 chord in second inversion")
   ((harm-seq '((10 12 16 19))) 10
    "a C7 chord in third inversion")
   ((harm-seq '((14 18 21 24))) 0
    "a D7 chord in root position")
   ((harm-seq '((20 26 26 28))) 4
    "a E7 chord in first inversion")
   ((harm-seq '((24 27 29 33))) 7
    "a F7 chord in second inversion")
   ((harm-seq '((29 31 35 38))) 10
    "a G7 chord in third inversion"))
   :parent-suite 'gct
   :depends-on '(h-bass-cpc h-gct-root-cpc))

;; h-cpc-int-from-gct-root
(make-viewpoint-tests 
 'h-cpc-int-from-gct-root
 '(((harm-seq '((0 4 7))) '(4 7)
    "a C major chord in root position")
   ((harm-seq '((16 55 60))) '(4 7)
    "a C major chord in first inversion")
   ((harm-seq '((16 45 60))) '(3 7)
    "an A minor chord in second inversion")
   ((harm-seq '((14 17 20 23))) '(3 6 9)
    "a diminished chord, bass D")
   ((harm-seq '((17 20 23 26))) '(3 6 9)
    "a diminished chord, bass F")
   ((harm-seq '((17 21 25))) '(4 8)
    "an augmented chord, bass F"))
 :parent-suite 'gct
 :depends-on '(h-cpitch h-gct-root-cpc))

;;;======================
;;;* Chord quality *
;;;======================

;; h-hedges-chord-type

;; The behaviour of this viewpoint can be slightly
;; unintuitive because of the way that chord roots
;; can be unambiguous. In particular:

;; Half-diminished chords get labelled by the GCT
;; algorithm as minor-major-6th chords, and so
;; do not receive the 'half-dim label.

;; Correspondingly, the following test would
;; fail, if implemented:

;; ((harm-seq '((0 3 6 10))) 'half-dim "half-dim. 7th")

;; The "special" chord type is not really well-
;; defined. Hedges and Wiggins (2016) give as an
;; example FM7/Eb, but this chord gets classified
;; by their algorithm as a 7th chord. Instead
;; we use the example of the power chord (i.e.
;; a chord with no third).

(make-viewpoint-tests
 'h-hedges-chord-type
 '(((harm-seq '((0 4 7 10)) :ref-pitch 10)
    7 "a 7th chord")
   ((harm-seq '((0 2 4 7 10)) :ref-pitch 12)
    7 "a 9th chord")
   ((harm-seq '((0 4 7)) :ref-pitch 40)
    'maj "a major chord")
   ((harm-seq '((0 4 7 11)) :ref-pitch 60)
    'maj "a major 7th chord")
   ((harm-seq '((0 4 7 9)) :ref-pitch 65)
    6 "a major 6th chord")
   ((harm-seq '((0 3 7))) 'min "a minor chord")
   ((harm-seq '((0 3 7 10))) 'min7 "a minor 7th chord")
   ((harm-seq '((0 3 7 9))) 'min
    "a min. maj. 6th chord")
   ((harm-seq '((0 3 7 8))) 'min-sharp5
    "a min. min. 6th chord")
   ((harm-seq '((0 3 6))) 'dim "a dim. chord")
   ((harm-seq '((0 3 6 9))) 'dim "a dim. 7th chord")
   ((harm-seq '((0 4 8))) 'aug "an augmented chord")
   ((harm-seq '((0 4 8 10))) 'alt "an altered chord")
   ((harm-seq '((0 5 7))) 'sus "a sus4 chord")
   ((harm-seq '((0 5 7 10))) 'sus "a 7sus4 chord")
   ((harm-seq '((0 7))) 'special "a power chord"))
 :parent-suite 'harmony
 :depends-on 'h-cpc-int-from-gct-root)

;; h-gct-3rd-type
(make-viewpoint-tests
 'h-gct-3rd-type
 '(((harm-seq '((0 4 7 10)) :ref-pitch 10)
    'maj "a 7th chord")
   ((harm-seq '((0 3 7 10)) :ref-pitch 20)
    'min "a minor 7th chord")
   ((harm-seq '((0 3 4 7)) :ref-pitch 15)
    'unclear "a major-minor chord")
   ((harm-seq '((0 7)) :ref-pitch 65)
    'unclear "a power chord"))
 :parent-suite 'harmony
 :depends-on 'h-cpc-int-from-gct-root)

;; h-gct-7th-type
(make-viewpoint-tests
 'h-gct-7th-type
 '(((harm-seq '((0 4 7)) :ref-pitch 10)
    'min-7-absent "a major chord")
   ((harm-seq '((0 4 7 10)) :ref-pitch 20)
    'min-7-present "a 7th chord")
   ((harm-seq '((0 3 7 10)) :ref-pitch 15)
    'min-7-present "a minor seventh chord chord")
   ((harm-seq '((0 4 7 11)) :ref-pitch 65)
    'min-7-absent "a major seventh chord"))
 :parent-suite 'harmony
 :depends-on 'h-cpc-int-from-gct-root)


;;;=========================
;;;* Hutchinson & Knopoff *
;;;=========================

(5am:def-suite hutch-knopoff
    :description "Hutchinson & Knopoff's roughness model"
    :in harmony)

;; midi->freq
(5am:def-suite midi->freq :in hutch-knopoff)
(5am:in-suite midi->freq)
(5am:test midi-freq-ex-1
  (5am:is (equalp (midi->freq 69) 440)))
(5am:test midi-freq-ex-2
  (5am:is (equalp (midi->freq 81) 880)))
(5am:test midi-freq-ex-3
  (5am:is (equalp (midi->freq 57) 220)))
(5am:test midi-freq-ex-4
  (5am:is (equalp (round-to-nearest-decimal-place
		   (midi->freq 20) 4)
		  25.9565)))

;; sum-amplitudes
(5am:def-suite sum-amplitudes :in hutch-knopoff)
(5am:in-suite sum-amplitudes)
(5am:test sum-amplitudes-ex-1
  (5am:is (equal (sum-amplitudes 0 0 :coherent t) 0.0)))
(5am:test sum-amplitudes-ex-2
  (5am:is (equal (sum-amplitudes 1.5 2.5 :coherent t) 4.0)))
(5am:test sum-amplitudes-ex-3
  (5am:is (equal (sum-amplitudes 0.7 0.3 :coherent t) 1.0)))
(5am:test sum-amplitudes-ex-4
  (5am:is (equal (sum-amplitudes 2.5 2.5 :coherent t) 5.0)))
(5am:test sum-amplitudes-ex-5
  (5am:is (equal (sum-amplitudes 0 0 :coherent nil) 0.0)))
(5am:test sum-amplitudes-ex-6
  (5am:is (equal (round-to-nearest-decimal-place
		  (sum-amplitudes 1 1 :coherent nil) 3)
		 1.414)))
(5am:test sum-amplitudes-ex-7
  (5am:is (equal (round-to-nearest-decimal-place
		  (sum-amplitudes 1 0.5 :coherent nil) 4)
		 1.118)))

;; combine-pure-tones
(5am:def-suite combine-pure-tones :in hutch-knopoff)
(5am:in-suite combine-pure-tones)
(5am:test combine-pure-tones-ex-1
  (5am:is (equal (combine-pure-tones nil) nil)))
(5am:test combine-pure-tones-ex-2
  (5am:is (equalp (combine-pure-tones
		  '((1 1) (2 1) (3 1)))
		 '((1 1) (2 1) (3 1)))))
(5am:test combine-pure-tones-ex-3
  (5am:is (equalp (combine-pure-tones
		  '((1 1) (3 1) (2 1)))
		 (list '(1 1) '(2 1) '(3 1)))))
(5am:test combine-pure-tones-ex-4
  (5am:is (equalp (combine-pure-tones
		  '((1 1) (3 1) (1 1))
		  :coherent t)
		 (list (list 1 2) (list 3 1)))))
(5am:test combine-pure-tones-ex-5
  (5am:is (equalp (combine-pure-tones
		  (list '(1 1) '(3 1.0) '(1 1))
		  :coherent nil)
		  `((1 ,(sqrt 2)) (3 1)))))

;; freq->harmonics
(5am:def-suite freq->harmonics :in hutch-knopoff)
(5am:in-suite freq->harmonics)
(5am:test freq->harmonics-ex-1
  (5am:is (equalp (freq->harmonics 10 :num-harmonics 0)
		  '((10 1)))))
(5am:test freq->harmonics-ex-2
  (5am:is (equalp (freq->harmonics 20 :num-harmonics 3)
		  '((20 1) (40 1/2) (60 1/3) (80 1/4)))))
(5am:test freq->harmonics-ex-3
  (5am:is (equalp (freq->harmonics
		   20 :num-harmonics 3
		   :roll-off #'(lambda (n) (/ 1 (1+ n))))
		  '((20 1) (40 1/2) (60 1/3) (80 1/4)))))

;; freq-list->harmonics
(5am:def-suite freq-list->harmonics :in hutch-knopoff)
(5am:in-suite freq-list->harmonics)
(5am:test freq-list->harmonics-ex-1
  (5am:is (equalp (freq-list->harmonics '(1 2) :num-harmonics 0)
		  '((1 1) (2 1)))))
(5am:test freq-list->harmonics-ex-2
  (5am:is (equalp (freq-list->harmonics '(1 2)
					 :num-harmonics 4
					 :coherent t)
		  `((1 1) (2 ,(+ 1/2 1)) (3 1/3) (4 ,(+ 1/4 1/2))
		    (5 1/5) (6 1/3) (8 1/4) (10 1/5)))))
(5am:test freq-list->harmonics-ex-3
  (5am:is (equalp (freq-list->harmonics '(1 2)
					 :num-harmonics 2
					 :coherent nil)
		  `((1 1) (2 ,(sqrt (+ (* 1/2 1/2) (* 1 1))))
		    (3 1/3) (4 1/2) (6 1/3)))))
(utils:set-test-suite-dependencies
 'freq-list->harmonics 'freq->harmonics)

;; hutch-cbw
(5am:def-suite hutch-cbw :in hutch-knopoff)
(5am:in-suite hutch-cbw)
(5am:test hutch-cbw-ex-1
  (5am:is (equalp (hutch-cbw 1 1) 1.72)))
(5am:test hutch-cbw-ex-2
  (5am:is (equalp (hutch-cbw 150 200) (* 1.72 (expt 175 0.65)))))

;; hutch-y
(5am:def-suite hutch-y :in hutch-knopoff)
(5am:in-suite hutch-y)
(5am:test hutch-y-ex-1
  (5am:is (equalp (hutch-y 1 1) 0)))
(5am:test hutch-y-ex-2
  (5am:is (equalp (hutch-y 150 200) (/ 50 (hutch-cbw 150 200)))))
(utils:set-test-suite-dependencies
 'hutch-y 'hutch-cbw)

;; hutch-g
(5am:def-suite hutch-g :in hutch-knopoff)
(5am:in-suite hutch-g)
(5am:test hutch-g-ex-1
  (5am:is (equalp (hutch-g 0) 0)))
(5am:test hutch-g-ex-2
  (5am:is (equalp (hutch-g 1) (expt (* 4 (exp -3)) 2))))

;; hutch-d
(5am:def-suite hutch-d :in hutch-knopoff)
(5am:in-suite hutch-d)
(5am:test hutch-d-ex-1
  (5am:is (equalp (hutch-d (freq-list->harmonics '(440) :num-harmonics 0))
		  0)))
;; The following examples are taken from Mashinter (2006).
;; Note the incorporation of custom harmonic roll-off
;; and cbw-cut-off values to match Mashinter's original
;; implementation.
;; Note 1: I can't replicate Mashinter's values for
;; (60 60) or for (60 62), for some reason.
;; Note 2: We replicate to three decimal places, it looks
;; like rounding errors etc. introduce some slight
;; discrepancies when we do four decimal places.
(5am:test hutch-d-ex-mashinter-1
  (5am:is (equalp (round-to-nearest-decimal-place
		   (hutch-d (freq-list->harmonics
			     (mapcar #'midi->freq '(60 61))
			     :num-harmonics 10
			     :roll-off #'(lambda (n) (/ 1 n)))
			    :cbw-cut-off 1.2)
		   3)
		  0.478)))
(5am:test hutch-d-ex-mashinter-2
  (5am:is (equalp (round-to-nearest-decimal-place
		   (hutch-d (freq-list->harmonics
			     (mapcar #'midi->freq '(60 63))
			     :num-harmonics 10
			     :roll-off #'(lambda (n) (/ 1 n)))
			    :cbw-cut-off 1.2)
		   3)
		  0.092)))
(5am:test hutch-d-ex-mashinter-3
  (5am:is (equalp (round-to-nearest-decimal-place
		   (hutch-d (freq-list->harmonics
			     (mapcar #'midi->freq '(60 64))
			     :num-harmonics 10
			     :roll-off #'(lambda (n) (/ 1 n)))
			    :cbw-cut-off 1.2)
		   3)
		  0.067)))
(5am:test hutch-d-ex-mashinter-4
  (5am:is (equalp (round-to-nearest-decimal-place
		   (hutch-d (freq-list->harmonics
			     (mapcar #'midi->freq '(62 65 70))
			     :num-harmonics 10
			     :roll-off #'(lambda (n) (/ 1 n)))
			    :cbw-cut-off 1.2)
		   3)
		  0.166)))
(5am:test hutch-d-ex-mashinter-5
  (5am:is (equalp (round-to-nearest-decimal-place
		   (hutch-d (freq-list->harmonics
			     (mapcar #'midi->freq '(60 64 67 70))
			     :num-harmonics 10
			     :roll-off #'(lambda (n) (/ 1 n)))
			    :cbw-cut-off 1.2)
		   3)
		  0.233)))
(utils:set-test-suite-dependencies
 'hutch-d 'freq-list->harmonics)

;;;; h-hutch-rough
;; Our implementation uses slightly different options to
;; Mashinter (2006), so we have no absolute references
;; against which to compare our viewpoint values.
;; However, we can still compare the relative roughness of
;; different chords, to check that we get appropriate
;; trends.

(5am:def-suite h-hutch-rough :in hutch-knopoff)
(5am:in-suite h-hutch-rough)

(5am:test h-hutch-rough-ex-1
  (5am:is (< (h-hutch-rough
	      (harm-seq '((60))))
	     (h-hutch-rough
	      (harm-seq '((60 67)))))
	  "A fifth should be less consonant than a unison."))
(5am:test h-hutch-rough-ex-2
  (5am:is (> (h-hutch-rough
	      (harm-seq '((60 61))))
	     (h-hutch-rough
	      (harm-seq '((60 62)))))
	  "A semitone should be rougher than a tone."))
(5am:test h-hutch-rough-ex-3
  (5am:is (> (h-hutch-rough
	      (harm-seq '((60 63))))
	     (h-hutch-rough
	      (harm-seq '((60 64)))))
	  "A minor third should be rougher than a major third."))
(5am:test h-hutch-rough-ex-4
  (5am:is (> (h-hutch-rough
	      (harm-seq '((60 63 66))))
	     (h-hutch-rough
	      (harm-seq '((60 64 67)))))
	  "A diminished triad should be rougher than a major triad."))
(5am:test h-hutch-rough-ex-5
  (5am:is (> (h-hutch-rough
	      (harm-seq '((64 67 72))))
	     (h-hutch-rough
	      (harm-seq '((60 64 67)))))
	  "A first inversion major triad should be rougher than a second inversion major triad."))



;;(utils:set-test-suite-dependencies
;; 'h-gct '(general-chord-type key-finding))


;;;===========================
;;;* Milne's spectral model *
;;;===========================

(5am:def-suite milne
    :description "Milne's spectral model"
    :in harmony)

;; gaussian-pdf
(5am:def-suite gaussian-pdf :in milne)
(5am:in-suite gaussian-pdf)
(5am:test gaussian-pdf-ex-1
  (5am:is (= (round-to-nearest-decimal-place
		   (gaussian-pdf 0 0 1) 4)
		  0.3989d0)))
(5am:test gaussian-pdf-ex-2
  (5am:is (= (round-to-nearest-decimal-place
		   (gaussian-pdf -1 0 1) 4)
		  0.2420d0)))
(5am:test gaussian-pdf-ex-3
  (5am:is (= (round-to-nearest-decimal-place
		   (gaussian-pdf 1.5 0.25 1) 4)
		  0.1826d0)))
(5am:test gaussian-pdf-ex-4
  (5am:is (= (round-to-nearest-decimal-place
		   (gaussian-pdf -1.7 0.4 5.5) 4)
		  0.0674d0)))
		  
;;;; make-gaussian-spectral-template
(5am:def-suite make-gaussian-spectral-template :in milne)
(5am:in-suite make-gaussian-spectral-template)
;; These tests aren't very complete.
;; Annoyingly, 5am doesn't work for let bindings.
(5am:test make-gaussian-spectral-template-ex-1
  (5am:is (vectorp (make-gaussian-spectral-template 1200 6.93))))
(5am:test make-gaussian-spectral-template-ex-2
  (5am:is (equal (svref (make-gaussian-spectral-template
			 1200 6.93)
			0)
		 (apply #'max
			(coerce (make-gaussian-spectral-template
				 1200 6.93)
				'list)))
	  "The first element of the template should be the largest."))

		
;;;; make-gaussian-spectrum
(5am:def-suite make-gaussian-spectrum :in milne)
(5am:in-suite make-gaussian-spectrum)

(5am:test make-gaussian-spectrum-ex-1
  (5am:is (equalp
	   (make-gaussian-spectrum 120 0 1 0.5)
	   (make-gaussian-spectral-template 120 0.5))
	  "A spectrum with mean 0 and mass 1 should be equal to the template."))
(5am:test make-gaussian-spectrum-ex-2
  (5am:is (equalp
	   (make-gaussian-spectrum 1200 0 1 0.3)
	   (make-gaussian-spectral-template 1200 0.3))
	  "A spectrum with mean 0 and mass 1 should be equal to the template."))

(5am:def-suite cosine-similarity :in milne)
(5am:in-suite cosine-similarity)
(5am:test cosine-similarity-ex-1
  (5am:is (equalp (round-to-nearest-decimal-place
		   (cosine-similarity
		    (vector 5 0 3 0 2 0 0 2 0 0)
		    (vector 3 0 2 0 1 1 0 1 0 1))
		   2)
		  0.94)))
(5am:test cosine-similarity-ex-2
  (5am:is (equalp (round-to-nearest-decimal-place
		   (cosine-similarity
		    (vector 2 1 0 2 0 1 1 1)
		    (vector 2 1 1 1 1 0 1 1))
		   3)
		  0.822)))

;;;; h-cpc-milne-sd-cont=min
(5am:def-suite h-cpc-milne-sd-cont=min :in milne)
(5am:in-suite h-cpc-milne-sd-cont=min)

(5am:test h-cpc-milne-sd-cont=min-ex-1
  (5am:is (equal (h-cpc-milne-sd-cont=min
		  (harm-seq '((0 4 7))))
		 +undefined+)))

;; These examples were derived from Milne's
;; MATLAB implementation.
(5am:test h-cpc-milne-sd-cont=min-ex-2
  (5am:is (= (round-to-nearest-decimal-place
		   (h-cpc-milne-sd-cont=min
		    (harm-seq '((0 4 7) (0 4 7))))
		   4)
		  0)))
(5am:test h-cpc-milne-sd-cont=min-ex-3
  (5am:is (= (round-to-nearest-decimal-place
		   (h-cpc-milne-sd-cont=min
		    (harm-seq '((0 4 7) (0 3 7))))
		   4)
		  0.2597d0)))
(5am:test h-cpc-milne-sd-cont=min-ex-4
  (5am:is (= (round-to-nearest-decimal-place
		   (h-cpc-milne-sd-cont=min
		    (harm-seq '((0 4 7) (0 2 7))))
		   4)
		  0.2201d0)))
(5am:test h-cpc-milne-sd-cont=min-ex-5
  (5am:is (= (round-to-nearest-decimal-place
		   (h-cpc-milne-sd-cont=min
		    (harm-seq '((0 4 7) (1 5 8))))
		   4)
		  0.8527d0)))
(5am:test h-cpc-milne-sd-cont=min-ex-6
  (5am:is (= (round-to-nearest-decimal-place
		   (h-cpc-milne-sd-cont=min
		    (harm-seq '((0 4 7) (2 7 11))))
		   4)
		  0.4155d0)))
(5am:test h-cpc-milne-sd-cont=min-ex-7
  (5am:is (= (round-to-nearest-decimal-place
		   (h-cpc-milne-sd-cont=min
		    (harm-seq '((0 4 7) (0 5 9))))
		   4)
		  0.4155d0)))
(5am:test h-cpc-milne-sd-cont=min-ex-8
  (5am:is (= (round-to-nearest-decimal-place
		   (h-cpc-milne-sd-cont=min
		    (harm-seq '((2 7 8) (3 4 8 9))))
		   4)
		  0.5619d0)))
(5am:test h-cpc-milne-sd-cont=min-ex-9
  (5am:is (= (round-to-nearest-decimal-place
		   (h-cpc-milne-sd-cont=min
		    (harm-seq '((2 6 8 9) (1 4))))
		   4)
		  0.5940d0)))
   

;;;==================================
;;;* Tymoczko's voice-leading model *
;;;==================================

(5am:def-suite tymoczko
    :description "Tymoczko's voice-leading model"
    :in harmony)

;;;; vl-ascending-distance
(5am:def-suite vl-ascending-distance :in tymoczko)
(5am:in-suite vl-ascending-distance)
(5am:test vl-ascending-distance-ex-1
  (5am:is (eql (vl-ascending-distance 60 70 :pitch) 10)))
(5am:test vl-ascending-distance-ex-2
  (5am:is (eql (vl-ascending-distance 1 5 :pitch-class) 4)))
(5am:test vl-ascending-distance-ex-3
  (5am:is (eql (vl-ascending-distance 7 10 :pitch-class) 3)))
(5am:test vl-ascending-distance-ex-4
  (5am:is (eql (vl-ascending-distance 11 0 :pitch-class) 1)))
(5am:test vl-ascending-distance-ex-5
  (5am:is (eql (vl-ascending-distance 7 1 :pitch-class) 6)))




;; vl-elt-distance
(5am:def-suite vl-elt-distance :in tymoczko)
(5am:in-suite vl-elt-distance)
(5am:test vl-elt-distance-ex-1
  (5am:is (eql (vl-elt-distance 60 64 :pitch) 4)))
(5am:test vl-elt-distance-ex-2
  (5am:is (eql (vl-elt-distance 50 47 :pitch) 3)))
(5am:test vl-elt-distance-ex-3
  (5am:is (eql (vl-elt-distance 10 7 :pitch-class) 3)))
(5am:test vl-elt-distance-ex-4
  (5am:is (eql (vl-elt-distance 11 0 :pitch-class) 1)))
(5am:test vl-elt-distance-ex-5
  (5am:is (eql (vl-elt-distance 7 1 :pitch-class) 6)))

;;;; vl-set-distance
(5am:def-suite vl-set-distance :in tymoczko)
(5am:in-suite vl-set-distance)

;; These examples are taken from Figure S12 in
;; Tymoczko (2006).
(5am:test vl-set-distance-ex-1
  (5am:is (eql (vl-set-distance
		'(4 7 11) '(4 4 4)
		:pitch-class
		:taxicab)
	       8)))
(5am:test vl-set-distance-ex-2
  (5am:is (eql (vl-set-distance
		'(4 7 7 7) '(4 8 11 3)
		:pitch-class
		:taxicab)
	       9)))
(5am:test vl-set-distance-ex-3
  (5am:is (eql (vl-set-distance
		'(4 7 11 0) '(4 8 11 11)
		:pitch-class
		:taxicab)
	       2)))

;; These examples are made up
(5am:test vl-set-distance-ex-4
  (5am:is (eql (vl-set-distance
		'(60 64 67) '(60 63 67)
		:pitch
		:taxicab)
	       1)))
(5am:test vl-set-distance-ex-5
  (5am:is (eql (vl-set-distance
		'(58 64 67) '(60 63 67)
		:pitch
		:taxicab)
	       3)))
(5am:test vl-set-distance-ex-4
  (5am:is (equalp (vl-set-distance
		   '(60 64 67) '(60 63 67)
		   :pitch
		   :euclidean)
		  1)))
(5am:test vl-set-distance-ex-5
  (5am:is (equalp (vl-set-distance
		   '(58 64 67) '(55 68 67)
		   :pitch
		   :euclidean)
		  5)))
(5am:test vl-set-distance-ex-6
  (5am:is (equalp (vl-set-distance
		   '(11 0 4) '(2 0 8)
		   :pitch-class
		   :euclidean)
		  5)))
(5am:test vl-set-distance-ex-7
  (5am:is (equalp (vl-set-distance
		   '(37 38 45) '(47 40 42)
		   :pitch
		   :infinity)
		  10)))
(5am:test vl-set-distance-ex-8
  (5am:is (equalp (vl-set-distance
		   '(11 0 4) '(5 0 8)
		   :pitch-class
		   :infinity)
		  6)))

;;;; vl-set-distance
(5am:def-suite vl-array :in tymoczko)
(5am:in-suite vl-array)

;; These examples are taken from Figure S12 in
;; Tymoczko (2006).

(5am:test vl-array-ex-1
  (5am:is (eql (cdr (assoc :size
			    (minimal-vl
			     (make-instance 'vl-array
					    :set-1 '(4 7 11 0 4)
					    :set-2 '(4 8 11 3 4)
					    :elt-type :pitch-class
					    :norm :taxicab))))
	       3)))
(5am:test vl-array-ex-2
  (5am:is (equal (cdr (assoc :start
			      (minimal-vl
			       (make-instance 'vl-array
					      :set-1 '(4 7 11 0 4)
					      :set-2 '(4 8 11 3 4)
					      :elt-type :pitch-class
					      :norm :taxicab))))
		 '(4 7 11 0 4))))
(5am:test vl-array-ex-3
  (5am:is (equal (cdr (assoc :end
			      (minimal-vl
			       (make-instance 'vl-array
					      :set-1 '(4 7 11 0 4)
					      :set-2 '(4 8 11 3 4)
					      :elt-type :pitch-class
					      :norm :taxicab))))
		 '(4 8 11 11 3))))
(5am:test vl-array-ex-4
  (5am:is (eql (extract-cell
		(make-instance 'vl-array
			       :set-1 '(4 7 11 0 4)
			       :set-2 '(4 8 11 3 4)
			       :elt-type :pitch-class
			       :norm :taxicab)
		2 0 :values t)
	       8)))
(5am:test vl-array-ex-5
  (5am:is (eql (extract-cell
		(make-instance 'vl-array
			       :set-1 '(4 7 11 0 4)
			       :set-2 '(4 8 11 3 4)
			       :elt-type :pitch-class
			       :norm :taxicab)
		1 3 :values t)
	       9)))

(5am:test vl-array-ex-6
  (5am:is (eql (extract-cell
		(make-instance 'vl-array
			       :set-1 '(4 7 11 0 4)
			       :set-2 '(4 8 11 3 4)
			       :elt-type :pitch-class
			       :norm :taxicab)
		3 1 :values t)
	       8)))

;;;; vl-get-minimal-voice-leading
(5am:def-suite vl-get-minimal-voice-leading :in tymoczko)
(5am:in-suite vl-get-minimal-voice-leading)

(5am:test vl-get-minimal-voice-leading-ex-1
  (5am:is (eql (cdr (assoc :size (vl-get-minimal-voice-leading
				  '(0 4 7) '(0 3 7)
				  :pitch-class :taxicab)))
	       1)))
(5am:test vl-get-minimal-voice-leading-ex-2
  (5am:is (eql (cdr (assoc :size (vl-get-minimal-voice-leading
				  '(0 4 7) '(0 5 9)
				  :pitch-class :taxicab)))
	       3)))
(5am:test vl-get-minimal-voice-leading-ex-3
  (5am:is (eql (cdr (assoc :size (vl-get-minimal-voice-leading
				  '(0 4 7) '(0 3 8)
				  :pitch-class :euclidean)))
	       (sqrt 2))))
(5am:test vl-get-minimal-voice-leading-ex-4
  (5am:is (eql (cdr (assoc :size (vl-get-minimal-voice-leading
				  '(0 4 7) '(0 3 9)
				  :pitch-class :infinity)))
	       2)))
(5am:test vl-get-minimal-voice-leading-ex-5
  (5am:is (eql (cdr (assoc :size (vl-get-minimal-voice-leading
				  '(50 60 65) '(48 62 67)
				  :pitch :taxicab)))
	       6)))
(5am:test vl-get-minimal-voice-leading-ex-6
  (5am:is (eql (cdr (assoc :size (vl-get-minimal-voice-leading
				  '(40 60 65) '(50 60)
				  :pitch :taxicab)))
	       15)))

;; Viewpoints
(make-viewpoint-tests
 'h-cpc-vl-dist-p=1
 '(((harm-seq '((0 4 7) (0 3 7))) 1)
   ((harm-seq '((0 2 5 7) (0 4 7))) 3)
   ((harm-seq '((2 5 8) (2 5 9))) 1))
 :parent-suite 'tymoczko)
(make-viewpoint-tests
 'h-cpc-vl-dist-p=2
 '(((harm-seq '((0 4 7) (0 3 8))) (sqrt 2))
   ((harm-seq '((0 2 5 7) (0 4 7))) (sqrt 5))
   ((harm-seq '((2 5 8) (2 5 9))) 1))
 :test #'equalp
 :parent-suite 'tymoczko)
(make-viewpoint-tests
 'h-cpitch-vl-dist-p=1
 '(((harm-seq '((40 41) (38 42))) 3)
   ((harm-seq '((16 20 25 26) (16 19 25))) 2)
   ((harm-seq '((15) (14 16))) 2))
 :parent-suite 'tymoczko)
(make-viewpoint-tests
 'h-cpitch-vl-dist-p=2
 '(((harm-seq '((40 41) (37 45))) 5)
   ((harm-seq '((16 20 25 26) (16 19 25))) (sqrt 2))
   ((harm-seq '((15) (14 16))) (sqrt 2)))
 :test #'equalp
 :parent-suite 'tymoczko)

