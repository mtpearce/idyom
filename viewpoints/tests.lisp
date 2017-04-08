;;;; ======================================================================
;;;; File:       tests.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-03-28 21:04:43 peter>                             
;;;; Time-stamp: <2017-04-08 22:44:28 peter>                           
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

(5am:in-suite h-gct)
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


