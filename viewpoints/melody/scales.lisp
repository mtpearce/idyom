;;;; ======================================================================
;;;; File:       scales.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2005-11-29 10:41:20 marcusp>
;;;; Time-stamp: <2023-05-22 14:27:08 marcusp>
;;;; ======================================================================

(cl:in-package #:viewpoints)

;; Original Western scale viewpoints
;;

(defun diatonic-set (referent mode)
  (let* ((diatonic-set '(0 2 4 5 7 9 11))
         (start (position mode diatonic-set)))
    (mapcar #'(lambda (x) (mod (+ (- x mode) referent) *octave*))
            (append (subseq diatonic-set start)
                    (subseq diatonic-set 0 start)))))

;; Returns 1 if event would not require an accidental as a result of
;; the prevailing key signature and 0 if it would (i.e. in A minor,
;; members of the set {A, B, C, D, E, F, G} return true, with all
;; others, including the leading note returning 0).
(define-viewpoint (inscale derived (cpitch))
    ((events md:melodic-sequence) element) 
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
                                          (0 (member (mod e *octave*) ds))
                                          (1 (not (member (mod e *octave*) ds)))))
                          (viewpoint-alphabet (get-viewpoint 'cpitch)))))


;; Pitch scales
;;
;; A scale is defined as a list of pitch values, including a
;; distinguished tonic pitch.

;; Named pitches
(defvar *a3* 57) 
(defvar *c4* 60)
(defvar *cs4* 61)
(defvar *d4* 62)
(defvar *ds4* 63)
(defvar *e* 64)
(defvar *f* 65)
(defvar *fs4* 66)
(defvar *g* 67)
(defvar *gs4* 68)
(defvar *a4* 69) 
(defvar *as4* 70)
(defvar *b4* 71)
(defvar *a5* 81) 

(defvar *major-intervals* '(0 2 4 5 7 9 11))
(defvar *minor-intervals* '(0 2 3 5 7 8 10))


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


;; Western scale degree
;;

;; Pitch to tonic interval
(define-viewpoint (tonint derived (cpitch))
    ((events md:melodic-sequence) element) 
  :function (- (cpitch events)
	       (+ (* (octave (list (car events))) *octave*)
		  (referent events))))

(define-viewpoint (sdeg-west derived (cpitch))
    ((events md:melodic-sequence) element) 
  :function (let* ((pitch (cpitch events))
		   (interval (tonint events))
		   (scale-intervals (case (mode events)
				      (0 *major-intervals*)
				      (9 *minor-intervals*)))
		   (degree (degree interval scale-intervals)))
	      (if (null degree)
		  pitch
		  (+ degree
		     (* (- (utils:quotient interval *octave*)
			   (if (< interval 0) 1 0))
			7)))))
		 
;; Degree number for pitch interval
(defun degree (interval scale-intervals)
  (let ((degree nil)
	(eqint (mod interval *octave*)))
    (dotimes (n (length scale-intervals) degree)
      (if (eq eqint (nth n scale-intervals))
	  (setq degree (+ n 1))))))


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

(defun divide-by-100 (x)
  (mapcar #'(lambda (y) 
              (if (numberp y)
                  (/ y 100)
                  (mapcar #'(lambda (z) (/ z 100)) y)))
          x))

;;; Hicaz makam

(defvar *hicaz-intervals* (divide-by-100
                           '(;; Development: Rast pentachord
                             -702 -498 (-317 -340) -204 
                             ;; Hicaz tetrachord, from tonic
                             0 113 (362 385) ; 498
                             ;; Rast pentachord (default) OR Buselik pentachord
                             ;; (optionally used when descending)
                             498 702 (770 792 860 883) 996 ; 1200
                             ;; Development: Buselik tetrachord
                             1200 1404 1494 1698)))

(defvar *hicaz-a3* (intervals->scale *a3* *hicaz-intervals*))
(defvar *hicaz-a4* (intervals->scale *a4* *hicaz-intervals*))
(defvar *hicaz-a5* (intervals->scale *a5* *hicaz-intervals*))

(defvar *hicaz-bottom* (bottom-scale *hicaz-a4*))


;;; Ussak makam

(defvar *ussak-intervals* (divide-by-100
                           '(;; Development: Rast pentachord
                             -702 -498 (-317 -340) -204 ; 0
                             ;; Ussak tetrachord, from tonic
                             0 (158 181) 294 ; 498
                             ;; Buselik pentachord
                             498 702 792 996 ; 1200
                             ;; Development: Ussak tetrachord
                             1200 (1358 1381) 1494 1698)))

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
;;; 
;;; Various definitions of scale degree are implemented below, varying
;;; in
;;; a) which octaves are considered (just a3, a3-a5, or all)
;;; b) how off-scale pitches are treated (use pitch value, or use 100
;;; as an off-scale symbol) 
;;;
;;; Each viewpoint is specific to a particular scale, either
;;; Hicaz or Ussak.  Note many other Turkish Makams/scales exist!
;;;
;;; This code could be replaced with a few macros.
;;;

;; Pitches in the a4 scale are replaced with scale degree, the rest
;; are untouched.
(define-viewpoint (sd-hicaz-a4 derived (cpitch))
    ((events md:melodic-sequence) element)
  :function (let* ((pitch (cpitch events))
		   (sd (scaledegree pitch *hicaz-a4*)))
	      (if (not (null sd))
		  sd
		  pitch)))

(define-viewpoint (sd-ussak-a4 derived (cpitch))
    ((events md:melodic-sequence) element)
  :function (let* ((pitch (cpitch events))
		   (sd (scaledegree pitch *ussak-a4*)))
	      (if (not (null sd))
		  sd
		  pitch)))


;; Scale degree for a4 pitches, others represented with off scale
;; symbol (1).
;;
(define-viewpoint (sd-hicaz-a4-total derived (cpitch))
    ((events md:melodic-sequence) element)
  :function (let* ((pitch (cpitch events))
		   (sd (scaledegree pitch *hicaz-a4*)))
	      (if (not (null sd))
		  sd
		  1)))

(define-viewpoint (sd-ussak-a4-total derived (cpitch))
    ((events md:melodic-sequence) element)
  :function (let* ((pitch (cpitch events))
		   (sd (scaledegree pitch *ussak-a4*)))
	      (if (not (null sd))
		  sd
		  1)))


;; Pitches in the a4, a3 and a5 scales are replaced with scale degree,
;; the rest are untouched.
;;
(define-viewpoint (sd-hicaz-a3-5 derived (cpitch))
    ((events md:melodic-sequence) element)
  :function (let* ((pitch (cpitch events))
		   (sd (first-scaledegree pitch
					  (list *hicaz-a4*
						*hicaz-a3*
						*hicaz-a5*))))
	      (if (not (null sd))
		  sd
		  pitch)))

(define-viewpoint (sd-ussak-a3-5 derived (cpitch))
    ((events md:melodic-sequence) element)
  :function (let* ((pitch (cpitch events))
		   (sd (first-scaledegree pitch
					  (list *ussak-a4*
						*ussak-a3*
						*ussak-a5*))))
	      (if (not (null sd))
		  sd
		  pitch)))


;; Pitches in the a4, a3 and a5 scales are replaced with scale degree,
;; others represented with off scale symbol (1).
;;
(define-viewpoint (sd-hicaz-a3-5-total derived (cpitch))
    ((events md:melodic-sequence) element)
  :function (let* ((pitch (cpitch events))
		   (sd (first-scaledegree pitch
					  (list *hicaz-a4*
						*hicaz-a3*
						*hicaz-a5*))))
	      (if (not (null sd))
		  sd
		  1)))


(define-viewpoint (sd-ussak-a3-5-total derived (cpitch))
    ((events md:melodic-sequence) element)
  :function (let* ((pitch (cpitch events))
		   (sd (first-scaledegree pitch
					  (list *ussak-a4*
						*ussak-a3*
						*ussak-a5*))))
	      (if (not (null sd))
		  sd
		  1)))


;; Scale degree is assigned over all octaves, off-scale pitches are
;; left untouched.
;;
(define-viewpoint (sd-hicaz-octave derived (cpitch))
    ((events md:melodic-sequence) element)
  :function (let* ((pitch (cpitch events))
		   (sd (scaledegree-octave (mod pitch *octave*)
					   *hicaz-bottom*)))
	      (if (not (null sd))
		  sd
		  pitch)))


(define-viewpoint (sd-ussak-octave derived (cpitch))
    ((events md:melodic-sequence) element)
  :function (let* ((pitch (cpitch events))
		   (sd (scaledegree-octave (mod pitch *octave*)
					   *ussak-bottom*)))
	      (if (not (null sd))
		  sd
		  pitch)))

;; Scale degree is assigned over all octaves, off-scale pitches are
;; left untouched.
;;
(define-viewpoint (sd-hicaz-octave-total derived (cpitch))
    ((events md:melodic-sequence) element)
  :function (let* ((pitch (cpitch events))
		   (sd (scaledegree-octave (mod pitch *octave*)
					   *hicaz-bottom*)))
	      (if (not (null sd))
		  sd
		  1)))

(define-viewpoint (sd-ussak-octave-total derived (cpitch))
    ((events md:melodic-sequence) element)
  :function (let* ((pitch (cpitch events))
		   (sd (scaledegree-octave (mod pitch *octave*)
					   *ussak-bottom*)))
	      (if (not (null sd))
		  sd
		  1)))


;; The viewpoints below have been written to analyse a set of Makam
;; stimuli, and should be used carefully: they use *terrible* hacks to
;; determine the scale of a composition.

;; A scale degree viewpoint for Hicaz and Ussak.  A hack: it uses the
;; composition descriptive text to select a Makam-specific viewpoint.
;;
;; This is very inefficient - use sd-makam-a4 below.
(define-viewpoint (sd-makam-a4-desc derived (cpitch))
    ((events md:melodic-sequence) element)
  :function (let* ((last (last-element events))
		   (comp (md:get-composition (md:get-identifier last)))
		   (desc (md:description comp)))
	      (if (equal (subseq desc 5 10) "hicaz")
		  (sd-hicaz-a4 events)
		  (sd-ussak-a4 events))))

;; A quicker version on Makam scale degree, which relies on the
;; mode viewpoint being used to indicate the current Makam scale.
(define-viewpoint (sd-makam-a4 derived (cpitch))
    ((events md:melodic-sequence) element)
  :function (if (eq (mode events) 0)
		 (sd-hicaz-a4 events)
		 (sd-ussak-a4 events)))

(defvar *hicaz-mode* 0)
(defvar *ussak-mode* 9)

;; Store identity of current Makam in unused mode viewpoint
(defun makam-modes (set-id)
  (dotimes (n (md:count-compositions set-id))
    (let* ((text (md:get-description set-id n))
	   (mode (if (string= (subseq text 5 10)
			    "hicaz")
		     *hicaz-mode*
		     *ussak-mode*))
	   (cmd (concatenate 'string
			     "UPDATE mtp_event SET mode=" (write-to-string mode)
			     " WHERE dataset_id=" (write-to-string set-id)
			     " and composition_id=" (write-to-string n))))
      (clsql:execute-command cmd))))


;; A 'general' scale degree viewpoint - a horrible hack that uses the
;; dataset id to select a Western or Turkish scale degree viewpoint.
(define-viewpoint (sdeg derived (cpitch))
    ((events md:melodic-sequence) element)
  :function (let* ((last (last-element events))
		   (set-id (md:get-dataset-index (md:get-identifier last))))
	      (if (< set-id 400)
		  (sdeg-west events)
		  (sd-makam-a4 events))))

;; Scale degree interval
(define-viewpoint (sdint derived (cpitch))
    ((events md:melodic-sequence) element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((degree1 (sdeg (list e1)))
                        (degree2 (sdeg (list e2))))
                    (if (undefined-p degree1 degree2) +undefined+
                        (let ((interval (- degree2 degree1)))
			  (if (> (abs interval) 0.2)
			      1
			      interval)))))))
			
