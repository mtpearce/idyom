;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       scales.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@eecs.qmul.ac.uk>
;;;; Created:    <2013-01-24 15:00:00 jeremy>
;;;; Time-stamp: <2013-02-21 00:03:51 jeremy>
;;;; ======================================================================

(cl:in-package #:viewpoints)

;; Original Western scale viewpoints
;;
;; To do: make these compatible with cents pitch values

(defun diatonic-set (referent mode)
  (let* ((diatonic-set '(0 2 4 5 7 9 11))
         (start (position mode diatonic-set)))
    (mapcar #'(lambda (x) (mod (+ (- x mode) referent) 12))
            (append (subseq diatonic-set start)
                    (subseq diatonic-set 0 start)))))


(define-viewpoint (inscale derived (cpitch))
    (events element) 
  :function (let ((cpitch-class (cpitch-class events))
                  (referent (referent events))
                  (mode (mode events)))
              (cond ((undefined-p cpitch-class referent mode) +undefined+)
                    ((member cpitch-class (diatonic-set referent mode) 
                             :test #'=)
                     1)
                    (t 0)))
  :function* (let* ((referent (referent events)) 
                    (mode (mode events))
                    (ds (diatonic-set referent mode)))
               (remove-if #'(lambda (e) (case element 
                                          (0 (member (mod e 12) ds))
                                          (1 (not (member (mod e 12) ds)))))
                          (viewpoint-alphabet (get-viewpoint 'cpitch)))))

;; Pitch scales
;;
;; A scale is defined as a list of pitch values, including a
;; distinguished tonic pitch.

;; Named pitches
(defvar *c4* 5994)
(defvar *a3* 5700) 
(defvar *a4* 6900) 
(defvar *a5* 8100) 

(defvar *octave* 1200)

(defun bottom-pitch (pitch)
  "Minimum octave equivalent pitch"
  (mod pitch *octave*))

(defun make-scale (pitches offset)
  "Define scale as set of pitches, plus offset to convert position to
 degree (tonic = 1)"
  (list :pitches pitches :offset offset))

(defun intervals->scale (tonic intervals)
  "Convert interval list to scale"
  (flet ((abs-pitch (interval) (+ tonic interval)))
    (make-scale (nmapcar #'abs-pitch intervals)
		;; offset indicates position of tonic
		(- (position 0 intervals) 1))))

(defun bottom-scale (scale)
  "List of minimum pitches that is pairwise octave equivalent to the
scale."
  (make-scale (nmapcar #'bottom-pitch (getf scale :pitches))
	      (getf scale :offset)))
        

;; Turkish Makams
;;
;; Makam scales are based on a set of intervals in Holdarian commas
;; (1, 3, 4, 5, 8, 9, 12-13), which define a set of tetrachords and
;; pentachords.
;;
;; Tetrachords (4 notes, 3 intervals)
;; Hicaz = [5 12 5] 
;; Ussak = [8 5 9]
;; Buselik = [9 4 9]
;;
;; Pentachords (5 notes, 4 intervals)
;; Rast = [9 8 5 9]
;; Buselik = [9 4 9 9]
;;
;; Alternative tunings mean that some notes in a scale may have
;; multiple pitches.  Some scales use different tetra/pentachords when
;; ascending and descending.  A nested list is used to represent
;; alternative pitches for a specific scale position.
;;
;; Makam scales may extend beyond a single octave, but (in theory)
;; this is not done via octave equivalency.  Instead, a single
;; "development" tetra/pentachord is specified for notes below the
;; tonic, and another for notes above the tonic+octave.


;;; Hicaz makam

(defvar *hicaz-intervals* '(;; Development: Rast pentachord
			    -702 -498 (-317 -340) -204 
			    ;; Hicaz tetrachord, from tonic
			    0 113 (362 385) ; 498
			    ;; Rast pentachord (default) OR Buselik pentachord
			    ;; (optionally used when descending)
			    498 702 (770 792 860 883) 996 ; 1200
			    ;; Development: Buselik tetrachord
			    1200 1404 1494 1698))

(defvar *hicaz-a3* (intervals->scale *a3* *hicaz-intervals*))
(defvar *hicaz-a4* (intervals->scale *a4* *hicaz-intervals*))
(defvar *hicaz-a5* (intervals->scale *a5* *hicaz-intervals*))

(defvar *hicaz-bottom* (bottom-scale *hicaz-a4*))


;;; Ussak makam

(defvar *ussak-intervals* '(;; Development: Rast pentachord
			    -702 -498 (-317 -340) -204 ; 0
			    ;; Ussak tetrachord, from tonic
			    0 (158 181) 294 ; 498
			    ;; Buselik pentachord
			    498 702 792 996 ; 1200
			    ;; Development: Ussak tetrachord
			    1200 (1358 1381) 1494 1698))

(defvar *ussak-a3* (intervals->scale *a3* *ussak-intervals*))
(defvar *ussak-a4* (intervals->scale *a4* *ussak-intervals*))
(defvar *ussak-a5* (intervals->scale *a5* *ussak-intervals*))


(defvar *ussak-bottom* (bottom-scale *ussak-a4*))


;;; Scale degree

(defun scaledegrees (pitch scale)
  "Positions this pitch takes in the scale, relative to the tonic pitch (=1)"
  (mapcar #'(lambda (x) (- x (getf scale :offset)))
	  (npositions pitch (getf scale :pitches))))
 
(defun scaledegrees-octave (pitch bscale)
  "Positions this pitch is octave equivalent in the scale, relative to
the tonic pitch (=1)"
  (scaledegrees (bottom-pitch pitch) bscale))

(defun smallest-positive (xs)
  "Return smallest positive value, else smallest non-positive."
  (if (null xs) nil
      (let* ((pos (remove-if-not #'(lambda (x) (>= x 1)) xs)))
	(if (null pos)
	    (apply #'max xs)
	    (apply #'min pos)))))

(defun scaledegree (pitch scale)
  (smallest-positive (scaledegrees pitch scale)))

(defun scaledegree-octave (pitch scale)
  (smallest-positive (scaledegrees-octave pitch scale)))

(defun first-scaledegree (pitch scales)
  "Find the first scale which defines a degree for the pitch, and
return that degree"
  (dolist (scale scales)
    (let ((sd (scaledegree pitch scale)))
      (if (not (null sd))
	  (return sd)))))



;;; Scale degree viewpoints

(define-viewpoint (sd-hicaz-a4 derived (cpitch))
    (events element)
  :function (let* ((pitch (cpitch events))
		   (sd (scaledegree pitch *hicaz-a4*)))
	      (if (not (null sd))
		  sd
		  pitch)))

(define-viewpoint (sd-hicaz-a4-total derived (cpitch))
    (events element)
  :function (let* ((pitch (cpitch events))
		   (sd (scaledegree pitch *hicaz-a4*)))
	      (if (not (null sd))
		  sd
		  100)))

(define-viewpoint (sd-hicaz-a3-5 derived (cpitch))
    (events element)
  :function (let* ((pitch (cpitch events))
		   (sd (first-scaledegree pitch
					  (list *hicaz-a4*
						*hicaz-a3*
						*hicaz-a5*))))
	      (if (not (null sd))
		  sd
		  pitch)))

(define-viewpoint (sd-hicaz-a3-5-total derived (cpitch))
    (events element)
  :function (let* ((pitch (cpitch events))
		   (sd (first-scaledegree pitch
					  (list *hicaz-a4*
						*hicaz-a3*
						*hicaz-a5*))))
	      (if (not (null sd))
		  sd
		  100)))

(define-viewpoint (sd-hicaz-octave derived (cpitch))
    (events element)
  :function (let* ((pitch (cpitch events))
		   (sd (scaledegree-octave (mod pitch *octave*)
					   *hicaz-bottom*)))
	      (if (not (null sd))
		  sd
		  pitch)))

(define-viewpoint (sd-ussak-a4 derived (cpitch))
    (events element)
  :function (let* ((pitch (cpitch events))
		   (sd (scaledegree pitch *ussak-a4*)))
	      (if (not (null sd))
		  sd
		  pitch)))

(define-viewpoint (sd-ussak-a4-total derived (cpitch))
    (events element)
  :function (let* ((pitch (cpitch events))
		   (sd (scaledegree pitch *ussak-a4*)))
	      (if (not (null sd))
		  sd
		  100)))

(define-viewpoint (sd-ussak-a3-5 derived (cpitch))
    (events element)
  :function (let* ((pitch (cpitch events))
		   (sd (first-scaledegree pitch
					  (list *ussak-a4*
						*ussak-a3*
						*ussak-a5*))))
	      (if (not (null sd))
		  sd
		  pitch)))

(define-viewpoint (sd-ussak-a3-5-total derived (cpitch))
    (events element)
  :function (let* ((pitch (cpitch events))
		   (sd (first-scaledegree pitch
					  (list *ussak-a4*
						*ussak-a3*
						*ussak-a5*))))
	      (if (not (null sd))
		  sd
		  100)))

(define-viewpoint (sd-ussak-octave derived (cpitch))
    (events element)
  :function (let* ((pitch (cpitch events))
		   (sd (scaledegree-octave (mod pitch *octave*)
					   *ussak-bottom*)))
	      (if (not (null sd))
		  sd
		  pitch)))
