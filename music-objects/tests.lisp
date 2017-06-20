;;;; ======================================================================
;;;; File:       tests.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-04-24 20:51:10 peter>                            
;;;; Time-stamp: <2017-06-20 14:16:41 peter>                           
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; This code defines tests for the music-data package.

(cl:in-package #:music-data)

;;;=====================
;;;* Utility functions *
;;;=====================

(defun make-ex-harm-slice (pitches onset dur &key (ref-pitch 0))
  "Makes a simple harmonic slice for testing purposes,
with pitches described by the list <pitches> and
onset and duration defined by <onset> and <dur>.
Pitches are expressed relative to <ref-pitch>."
  (let ((events (mapcar #'(lambda (pitch)
			    (let ((event (make-instance
					  'md::music-event
					  :cpitch (+ ref-pitch pitch))))
			      (md:set-attribute event 'onset onset)
			      (md:set-attribute event 'dur dur)
			      event))
			pitches))
	(slice (make-instance 'md:music-slice
			      :onset onset :duration dur)))
    (sequence:adjust-sequence slice (length events)
			      :initial-contents events)
    slice))

(defun harm-seq
    (cpitch &key onset dur (ref-pitch 0) (timebase 96) (type :slices))
  "Intended for testing purposes. Makes an object of class md:harmonic-sequence
with attributes specified by <cpitch>, <onset>, <dur>, and <ref-pitch>.
<cpitch> should be a list of lists, with the ith element of the jth list
corresponding to the ith chromatic pitch in the jth chord, after transposition
by <ref-pitch>. By default, each chord has duration 100 and onset 100 basic
time units after the onset of the previous chord. It is possible to
specify <onset> manually, in which case <onset> should be a list with the ith
element being the onset of the ith chord. Chords should be listed in order
of increasing onset. In this case the duration of each
chord will default to the inter-onset interval between each chord,
unless <dur> is specified manually, in which case <dur> should 
be a list the ith element of which is the duration of the ith chord.
"
  (assert (member type '(:slices :chords)))
  (if (or (null cpitch) (not (listp cpitch)))
      (error "<cpitch> must be a non-empty list."))
  (let* ((num-chords (length cpitch))
	 (onset (cond ((null onset)
		       (loop for x from 0 to (1- num-chords)
			  collect (* 100 x)))
		      ((eql (length onset) num-chords)
		       onset)
		      (t (error "<onset> must be NIL or a list of same length as <cpitch>."))))
	 (dur (cond ((null dur)
		     (if (eql (length onset) 1)
			 '(100)
			 (append (loop
				    for a in (butlast onset)
				    for b in (cdr onset)
				    collect (- b a))
				 '(100))))
		    ((eql (length dur) num-chords) dur)
		    (t (error "<dur> must be NIL or a list of same length as <cpitch>.")))))
    (if (not (every #'(lambda (x) (> x 0)) dur))
	(error "Every element of <dur> must be greater than 0."))
    (let* ((seq (make-instance 'md:harmonic-sequence
			       :onset 0
			       :duration (+ (car (last onset)) (car (last dur)))
			       :midc 60 :timebase timebase))
	   (slices nil))
      (loop
	 for chord in cpitch
	 for o in onset
	 for d in dur
	 do (progn
	      (let ((events (mapcar #'(lambda (pitch)
					(let ((event (make-instance
						      'md::music-event
						      :cpitch (+ ref-pitch pitch)
						      :timebase timebase)))
					  (md:set-attribute event 'onset o)
					  (md:set-attribute event 'dur d)
					  event))
				    chord))
		    (slice (make-instance 'md:music-slice
					  :onset o :duration d)))
		(sequence:adjust-sequence slice (length events)
					  :initial-contents events)
		(push (case type
			(:slices slice)
			(:chords (slice->chord slice)))
		      slices))))
      (sequence:adjust-sequence seq (length slices)
				:initial-contents (reverse slices))
      seq)))

;;;=========
;;;* Tests *
;;;=========

(5am:def-suite music-data)
(5am:in-suite music-data)

;;;; trim
(5am:def-suite trim :in music-data)
(5am:in-suite trim)
;; Trimming music events
(5am:test trim-ex-1
  (5am:is (equalp (onset (trim (make-instance 'music-event
					      :onset 0 :duration 100)
			       50 100))
		  50)))
(5am:test trim-ex-2
  (5am:is (equalp (onset (trim (make-instance 'music-event
					      :onset 100 :duration 100)
			       50 300))
		  100)))
(5am:test trim-ex-3
  (5am:is (equalp (duration (trim (make-instance 'music-event
						 :onset 200 :duration 100)
				  100 225))
		  25)))
(5am:test trim-ex-4
  ;; A trim producing an event of zero length should result in nil.
  (5am:is (null (trim (make-instance 'music-event
				     :onset 200 :duration 100)
		      100 150))))
;; Trimming music slices
(5am:test trim-ex-5
  (5am:is (equalp (duration (trim (make-ex-harm-slice '(0 3 7) 0 100)
				  50 75))
		  25)))
(5am:test trim-ex-6
  (5am:is (equalp (onset (trim (make-ex-harm-slice '(0 3 7) 0 100)
				  50 85))
		  50)))
(5am:test trim-ex-7
  (5am:is (every #'(lambda (e) (equalp (duration e) 75))
		 (coerce (trim (make-ex-harm-slice '(0 3 7) 0 200)
			       125 200)
			 'list))))
(5am:test trim-ex-8
  (5am:is (every #'(lambda (e) (equalp (onset e) 125))
		 (coerce (trim (make-ex-harm-slice '(0 3 7) 0 200)
			       125 200)
			 'list))))

;;;; get-sounding-objects
(5am:def-suite get-sounding-objects :in music-data)
(5am:in-suite get-sounding-objects)

(5am:test get-sounding-objects-ex-1
  (5am:is (equal (mapcar #'(lambda (s) (onset s))
			 (get-sounding-objects
			  (harm-seq '((0) (1) (2) (3))
				    :onset '(0 100 200 300))
			  150 200))
		 '(100))))
(5am:test get-sounding-objects-ex-2
  (5am:is (equal (mapcar #'(lambda (s) (onset s))
			 (get-sounding-objects
			  (harm-seq '((0) (1) (2) (3))
				    :onset '(0 100 200 300))
			  299 nil))
		 '(200 300))))
(5am:test get-sounding-objects-ex-3
  (5am:is (equal (mapcar #'(lambda (s) (onset s))
			 (get-sounding-objects
			  (harm-seq '((0) (1) (2) (3))
				    :onset '(0 100 200 300))
			  -50 200))
		 '(0 100))))

;;;; slices->bars
(5am:def-suite slices->bars :in music-data)
(5am:in-suite slices->bars)

;; Check that bar numbers work properly
(5am:test slices->bars-ex-1
  (5am:is (equal (mapcar
		  #'(lambda (x) (cdr (assoc :bar x)))
		  (slices->bars (coerce (harm-seq '((0) (1) (2) (3) (4) (5) (6))
						  :onset '(0 50 100 133 166 200 300))
					'list)
				'(0 1 2 3) '(0 100 200 300)))
		 '(0 1 2 3))))

;; Bars should still be created even if they're empty
(5am:test slices->bars-ex-2
  (5am:is (equal (mapcar
		  #'(lambda (x) (cdr (assoc :bar x)))
		  (slices->bars (coerce (harm-seq '((0) (1) (2) (3) (4) (5))
						  :onset '(0 50 100 133 166 200))
					'list)
				'(0 1 2 3) '(0 100 200 300)))
		 '(0 1 2 3))))

;; Check that the onsets of the slices make sense
(5am:test slices->bars-ex-3
  (5am:is (equal (mapcar
		  #'(lambda (x) (mapcar #'onset (cdr (assoc :slices x))))
		  (slices->bars (coerce (harm-seq '((0) (1) (2) (3) (4) (5))
						  :onset '(0 50 100 133 166 200))
					'list)
				'(0 1 2 3) '(0 100 200 300)))
		 '((0 50) (100 133 166) (200) nil))))

;; Check that onsets are trimmed to the starts of bars, and that slices
;; are replicated if they cross bar boundaries
(5am:test slices->bars-ex-4
  (5am:is (equal (mapcar
		  #'(lambda (x) (mapcar #'onset (cdr (assoc :slices x))))
		  (slices->bars (coerce (harm-seq '((0) (1) (2) (3) (4) (5))
						  :onset '(0 80 150 180 210 300))
					'list)
				'(0 1 2 3) '(0 100 200 300)))
		 '((0 80) (100 150 180) (200 210) (300)))))

;; Check slice replication from a pitch perspective
(5am:test slices->bars-ex-5
  (5am:is (equal (mapcar
		  #'(lambda (x)
		      (mapcar #'(lambda (y) (chromatic-pitch (car (coerce y 'list))))
			      (cdr (assoc :slices x))))
		  (slices->bars (coerce (harm-seq '((0) (1) (2) (3) (4) (5))
						  :onset '(0 80 150 180 210 300))
					'list)
				'(0 1 2 3) '(0 100 200 300)))
		 '((0 1) (1 2 3) (3 4) (5)))))

;; Check that offsets are trimmed to the ends of bars, and that slices
;; are replicated if they cross bar boundaries
(5am:test slices->bars-ex-6
  (5am:is (equal (mapcar
		  #'(lambda (x) (mapcar #'duration (cdr (assoc :slices x))))
		  (slices->bars (coerce (harm-seq '((0) (1) (2) (3) (4) (5))
						  :onset '(0 80 150 180 210 300))
					'list)
				'(0 1 2 3) '(0 100 200 300)))
		 '((80 20) (50 30 20) (10 90) (100)))))

;;;; get-timesig
(5am:def-suite get-timesig :in music-data)
(5am:in-suite get-timesig)

(5am:test get-timesig-ex-1
  (5am:is (eql (nth-value 0 (get-timesig 6 (* 24 3) 96)) 6)))
(5am:test get-timesig-ex-2
  (5am:is (eql (nth-value 1 (get-timesig 6 (* 24 3) 96)) 8)))
(5am:test get-timesig-ex-3
  (5am:is (eql (nth-value 0 (get-timesig 3 (* 24 3) 96)) 3)))
(5am:test get-timesig-ex-4
  (5am:is (eql (nth-value 1 (get-timesig 3 (* 24 3) 96)) 4)))
(5am:test get-timesig-ex-5
  (5am:is (eql (nth-value 0 (get-timesig 2 (* 24 2) 96)) 2)))
(5am:test get-timesig-ex-6
  (5am:is (eql (nth-value 1 (get-timesig 2 (* 24 2) 96)) 4)))

;;;; find-harmonic-rhythm
(5am:def-suite find-harmonic-rhythm :in music-data)
(5am:in-suite find-harmonic-rhythm)

(5am:test find-canonic-harmonic-rhythm-ex-1
  (5am:is (equal (cdr (assoc :segment-starts-relative
			     (find-canonic-harmonic-rhythm 2 48)))
		 '(0 24))))
(5am:test find-canonic-harmonic-rhythm-ex-2
  (5am:is (equal (cdr (assoc :segment-ends-relative
			     (find-canonic-harmonic-rhythm 2 48)))
		 '(24 48))))
(5am:test find-canonic-harmonic-rhythm-ex-3
  (5am:is (equal (cdr (assoc :segment-starts-relative
			     (find-canonic-harmonic-rhythm 3 72)))
		 '(0))))
(5am:test find-canonic-harmonic-rhythm-ex-4
  (5am:is (equal (cdr (assoc :segment-ends-relative
			     (find-canonic-harmonic-rhythm 3 72)))
		 '(72))))
(5am:test find-canonic-harmonic-rhythm-ex-5
  (5am:is (equal (cdr (assoc :segment-starts-relative
			     (find-canonic-harmonic-rhythm 6 72)))
		 '(0 36))))
(5am:test find-canonic-harmonic-rhythm-ex-6
  (5am:is (equal (cdr (assoc :segment-ends-relative
			     (find-canonic-harmonic-rhythm 6 72)))
		 '(36 72))))

;;;; slices->pc-weights
(5am:def-suite slices->pc-weights :in music-data)
(5am:in-suite slices->pc-weights)

(5am:test slices->pc-weights-ex-1
  (5am:is (equalp (slices->pc-weights (coerce (harm-seq '((0 3 7))) 'list)
				      :weight :num-segments)
		  (vector 1 0 0 1 0 0 0 1 0 0 0 0))))
(5am:test slices->pc-weights-ex-2
  (5am:is (equalp (slices->pc-weights (coerce (harm-seq '((0 3 7))) 'list)
				      :weight :duration)
		  (vector 100 0 0 100 0 0 0 100 0 0 0 0))))
(5am:test slices->pc-weights-ex-3
  (5am:is (equalp (slices->pc-weights
		   (coerce (harm-seq '((0 3 7) (0 5 8) (0 3 7))) 'list)
		   :weight :num-segments)
		  (vector 3 0 0 2 0 1 0 2 1 0 0 0))))
(5am:test slices->pc-weights-ex-4
  (5am:is (equalp (slices->pc-weights
		   (coerce (harm-seq '((0 3 7) (0 5 8) (0 3 7))
				     :onset '(0 100 101))
			   'list)
		   :weight :duration)
		  (vector 201 0 0 200 0 1 0 200 1 0 0 0))))

;;;; *pb-all-chord-templates*
(5am:def-suite pb-all-chord-templates :in music-data)
(5am:in-suite pb-all-chord-templates)

;; *pb-all-chord-templates should be a list of 12 lists,
;; each corresponding to templates for one particular root.

;; Check for pitch classes not betwen 0 and 11, and for duplicated pitch classes
(5am:test pb-all-chord-templates-ex-1
  (5am:is (every #'(lambda (ct)
		     (and (every #'(lambda (pc)
				     (and (integerp pc) (>= pc 0) (< pc 12)))
				 ct)
			  (eql (length ct)
			       (length (remove-duplicates ct)))))
		 (mapcar #'(lambda (x) (cdr (assoc :pc x)))
			 *pb-all-chord-templates*))))

;;;; score-template
(5am:def-suite score-template :in music-data)
(5am:in-suite score-template)

;; Example derived from Pardo and Birmingham (2002), p. 29
(5am:test score-template-ex-1
  (5am:is (eql (score-template (vector 4 0 1 0 4 0 0 0 0 0 0 0)
			       (list (cons :root 9) (cons :pc (list 0 4 9))
				     (cons :prior 0.194)))
	       6)))


;; Tie break by root weight
(5am:def-suite tie-break-by-root-weight :in music-data)
(5am:in-suite tie-break-by-root-weight)

(5am:test tie-break-by-root-weight-ex-1
  (5am:is (equal (tie-break-by-root-weight
		  (list (list (cons :root 0))
			(list (cons :root 7))
			(list (cons :root 9)))
		  (vector 5 0 0 0 0 0 0 0 0 2.5 0 0))
		 (list (list (cons :root 0))))))
(5am:test tie-break-by-root-weight-ex-2
  (5am:is (equal (tie-break-by-root-weight
		  (list (list (cons :root 0))
			(list (cons :root 7))
			(list (cons :root 9)))
		  (vector 5 0 0 0 0 0 0 0 0 5 0 0))
		 (list (list (cons :root 0))
		       (list (cons :root 9))))))
		  
;; Tie break by prior
(5am:def-suite tie-break-by-prior :in music-data)
(5am:in-suite tie-break-by-prior)

(5am:test tie-break-by-prior-ex-1
  (5am:is (equal (tie-break-by-prior
		  (list (list (cons :prior 0.5))
			(list (cons :prior 1.3))
			(list (cons :prior 0.2))))
		 (list (list (cons :prior 1.3))))))
(5am:test tie-break-by-prior-ex-2
  (5am:is (equal (tie-break-by-prior
		  (list (list (cons :tonic 0) (cons :prior 0.5))
			(list (cons :tonic 7) (cons :prior 1.3))
			(list (cons :tonic 9) (cons :prior 1.3))))
		 (list (list (cons :tonic 7) (cons :prior 1.3))
		       (list (cons :tonic 9) (cons :prior 1.3))))))

;; Random tie break
(5am:def-suite random-tie-break :in music-data)
(5am:in-suite random-tie-break)

(let ((templates (loop for template in *pb-all-chord-templates*
		    if (= (random 2) 1) collect template)))
  (eval
   `(5am:test random-tie-break-ex-1
      (5am:is 
       (utils:all-eql (list (random-tie-break ',templates)
			    (random-tie-break ',templates)
			    (random-tie-break ',templates)
			    (random-tie-break ',templates)
			    (random-tie-break ',templates)
			    (random-tie-break ',templates))
		      :predicate #'equal)))))

;;;; slices->slice
(5am:def-suite slices->slice :in music-data)
(5am:in-suite slices->slice)

;; Test that plausible pitches are produced with pitch classes as input
(5am:test slices->slice-ex-1
  (5am:is (equal (mapcar
		  #'chromatic-pitch
		  (coerce 
		   (slices->slice (coerce (harm-seq '((0) (4) (7) (12))) 'list)
				  0 100)
		   'list))
		 (list 48 64 67))))
;; Test that plausible pitches are produced with pitches as input
(5am:test slices->slice-ex-2
  (5am:is (equal (mapcar
		  #'chromatic-pitch
		  (coerce 
		   (slices->slice (coerce (harm-seq
					   '((60) (64) (67) (72)))
					  'list)
				  0 100)
		   'list))
		 (list 48 64 67))))
;; Check slice onset
(5am:test slices->slice-ex-3
  (5am:is (equal (onset (slices->slice (coerce (harm-seq
					   '((60) (64) (67) (72)))
					  'list)
				       250 300))
		 250)))
;; Check slice duration
(5am:test slices->slice-ex-4
  (5am:is (equal (duration (slices->slice (coerce (harm-seq
					   '((60) (64) (67) (72)))
					  'list)
				       250 300))
		 50)))
;; Check event onsets
(5am:test slices->slice-ex-5
  (5am:is (equal (mapcar
		  #'onset
		  (coerce 
		   (slices->slice (coerce (harm-seq
					   '((60) (64) (67) (72)))
					  'list)
				  150 200)
		   'list))
		 (list 150 150 150))))
;; Check event durations
(5am:test slices->slice-ex-6
  (5am:is (equal (mapcar
		  #'duration
		  (coerce 
		   (slices->slice (coerce (harm-seq
					   '((60) (64) (67) (72)))
					  'list)
				  150 200)
		   'list))
		 (list 50 50 50))))

;;;; Serialization
(5am:def-suite serial :in music-data)
(5am:in-suite serial)

;; Files and folders
(defparameter *temp-dir* (merge-pathnames
			  (make-pathname :directory
					 '(:relative "temp" "tests"))
			  cl-user::*idyom-root*))
(ensure-directories-exist *temp-dir*)

(defun test-serial (input file-id)
  (assert (stringp file-id))
  (let ((path (merge-pathnames
	       (make-pathname :name file-id :type "cache")
	       *temp-dir*)))
    (cl-store:store input path)
    (cl-store:restore path)))

(5am:test serial-ex-1
  (5am:is
   (equal (mapcar #'(lambda (e) (md:get-attribute e 'h-cpitch))
		  (coerce (test-serial (harm-seq '((0 4 7) (0 3 7) (0 4 7)))
				       "harm-seq-1")
			  'list))
	  '((0 4 7) (0 3 7) (0 4 7)))))

;;;; remove-repeated-chords
(5am:def-suite remove-repeated-chords :in music-data)
(5am:in-suite remove-repeated-chords)

(5am:test remove-repeated-chords-ex-1
  (5am:is
   (equal (mapcar #'(lambda (e) (md:get-attribute e 'h-cpitch))
		  (%list-slot-sequence-data
		   (remove-repeated-chords
		    (harm-seq '((0 4 7) (0 4 7) (0 3 7)) :type :chords))))
	  (list (list 0 4 7) (list 0 3 7)))))
(5am:test remove-repeated-chords-ex-2
  (5am:is
   (equal (mapcar #'(lambda (e) (md:get-attribute e 'dur))
		  (%list-slot-sequence-data
		   (remove-repeated-chords
		    (harm-seq '((0 4 7) (0 4 7) (0 3 7)) :type :chords))))
	  (list 200 100))))
(5am:test remove-repeated-chords-ex-3
  (5am:is
   (equal (mapcar #'(lambda (e) (md:get-attribute e 'dur))
		  (%list-slot-sequence-data
		   (remove-repeated-chords
		    (harm-seq '((0 4 7) (0 4 7) (0 3 7))
			      :onset '(0 100 200)
			      :dur '(50 100 100)
			      :type :chords))))
	  (list 200 100))))


				   
	  
   
