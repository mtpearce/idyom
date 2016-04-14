;;;; ======================================================================
;;;; File:       temporal.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2005-11-29 10:41:20 marcusp>
;;;; Time-stamp: <2016-04-13 13:34:38 marcusp>
;;;; ======================================================================

(cl:in-package #:viewpoints)

;;; Barlength, Pulses ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Denominator of the time signature

(define-viewpoint (beatunit derived (barlength pulses))
    ((events md:music-sequence) element) 
  :function (let ((barlength (barlength events))
		  (pulses (pulses events))
		  (timebase (md:timebase (last-element events))))
	      (cond ((undefined-p barlength pulses) +undefined+)
		    ((zerop barlength) +undefined+)
		    ((zerop pulses) +undefined+)
		    (t (/ (* timebase pulses) barlength))))
  ;; TODO: function*
  )


;;; Dur ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-viewpoint (dur-ratio derived (dur))
    ((events md:music-sequence) element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((dur1 (dur (list e1)))
                        (dur2 (dur (list e2))))
                    (declare (type fixnum dur1 dur2))
                    (if (undefined-p dur1 dur2) +undefined+
                        (/ dur2 dur1)))))
  :function* (list (* element (dur (list (penultimate-element events))))))


;;; BIOI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bioi divided by the previous ioi (requires at least 3 events).
(define-viewpoint (bioi-ratio derived (bioi))
    ((events md:melodic-sequence) element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((ioi1 (bioi (list e1)))
                        (ioi2 (bioi (list e2))))
                    ;;(print (list (slot-value e1 'md::bioi) (slot-value e2 'md::bioi)))
                    ;;(print (list ioi1 ioi2))
                    (if (or (zerop ioi1) (undefined-p ioi1 ioi2)) +undefined+
                        (/ ioi2 ioi1)))))
  :function* (let ((penultimate-element (list (penultimate-element events))))
               (list (* element (bioi penultimate-element)))))

;; Whether bioi gets larger, smaller or stays the same between
;; consecutive events
(define-viewpoint (bioi-contour derived (bioi))
    ((events md:melodic-sequence) element) 
  :function (let ((bioi-ratio (bioi-ratio events)))
              (if (undefined-p bioi-ratio) +undefined+
                  (signum (- bioi-ratio 1))))
  :function* (let ((bioi (bioi (list (penultimate-element events)))))
               (remove-if #'(lambda (a) (case element
                                          (-1 (>= a bioi))
                                          (0  (not (= a bioi)))
                                          (1  (<= a bioi))))
                          (viewpoint-alphabet (get-viewpoint 'bioi)))))



;;; ONSET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Like bioi, but undefined for first event in sequence
(define-viewpoint (ioi derived (onset))
    ((events md:music-sequence) element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((onset1 (onset (list e1)))
                        (onset2 (onset (list e2))))
                    (if (undefined-p onset1 onset2) +undefined+
                        (- onset2 onset1)))))
  :function* (list (+ element (onset (list (penultimate-element events))))))

;; ioi divided by the previous ioi (requires at least 3 events).
(define-viewpoint (ioi-ratio derived (onset))
    ((events md:music-sequence) element) 
  :function (multiple-value-bind (e1 e2 e3)
                (values-list (last events 3))
              (if (or (null e1) (null e2) (null e3)) +undefined+
                  (let ((ioi1 (ioi (list e1 e2)))
                        (ioi2 (ioi (list e2 e3))))
                    (if (undefined-p ioi1 ioi2) +undefined+
                        (/ ioi2 ioi1)))))
  :function* (let ((penultimate-element (list (penultimate-element events))))
               (list (+ (onset penultimate-element)
                        (* element (bioi penultimate-element))))))

;; Whether ioi gets larger, smaller or stays the same between consecutive events
(define-viewpoint (ioi-contour derived (onset))
    ((events md:music-sequence) element) 
  :function (let ((ioi-ratio (ioi-ratio events)))
              (if (undefined-p ioi-ratio) +undefined+
                  (signum (- ioi-ratio 1))))
  :function* (let* ((penultimate-element (list (penultimate-element events)))
                    (ioi (bioi penultimate-element))
                    (onset (onset penultimate-element))
                    (ioi-set (remove-if #'(lambda (a) (case element
                                                        (-1 (>= a ioi))
                                                        (0  (not (= a ioi)))
                                                        (1  (<= a ioi))))
                                        (viewpoint-alphabet (get-viewpoint 'bioi)))))
               (mapcar #'(lambda (x) (+ onset x)) ioi-set)))

;; Time offset from beginning of bar.
(define-viewpoint (posinbar derived (onset))
    ((events md:music-sequence) element) 
  :function (let ((onset (onset events))
                  (barlength (barlength events)))
              (cond ((undefined-p onset barlength) +undefined+)
                    ((zerop barlength) +undefined+)
                    ((zerop onset) 0)
                    ((> onset 0) (mod onset barlength))
                    (t +undefined+)))
  ;; TODO: function*
  )

;; First In Bar (Is this the first note in the current bar?)
(define-viewpoint (fib test (onset))
    ((events md:music-sequence) element) 
  :function (let ((posinbar (posinbar events)))
              (cond ((undefined-p posinbar) +undefined+)
                    ((= posinbar 0) 1)
                    (t 0)))
  ;; TODO: function* 
  )

;; does this note fall on a crotchet beat from the first note in the piece?
(define-viewpoint (crotchet test (onset))
    ((events md:music-sequence) element) 
  :function (let ((e1 (car events))
                  (e2 (car (last events))))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((onset1 (onset (list e1)))
                        (onset2 (onset (list e2))))
                    (if (undefined-p onset1 onset2) +undefined+
                        ;;this only works if crotchet == 24 
                        (if (zerop (mod (- onset2 onset1) 24)) 1 0)))))
  ;; TODO: function* 
  )

;; tactus
;;
;; Is this note on tactus pulse?
(define-viewpoint (tactus test (onset))
    ((events md:music-sequence) element) 
  :function (let ((event (last events)))
              (if (null event) +undefined+
                  (let ((barlength (barlength event))
                        (pulses (pulses event))
                        (onset (onset event)))
                    (declare (type fixnum barlength pulses))
                    (if (or (undefined-p barlength pulses onset)
                            (zerop barlength)
                            (zerop pulses))
                        +undefined+
                        (if (zerop (mod onset (/ barlength pulses))) 1 0)))))
  ;; TODO: function* 
  )

;;; Metrical accent
;;;
;;; Emphasis on notes which fall on specific beats or fractions of
;;; beats, relative to a defined beat level.  Defined here as the sum
;;; of: 1) multiple-level accent, a value derived from the particular
;;; beat in the bar the note falls on; 2) division-level accent, a
;;; value derived from the fractional beat the note falls on
;;; (including whole beats).
;;; 
;;; This is best understood as a metrical grid.  For 4/4 time (4 beats
;;; in the bar) we might have the following:
;;;
;;; LEVEL       GRID
;;; Multiple 2  o
;;; Multiple 1  o               o
;;; Beat        o       o       o       o       <--- 4 beats in bar
;;; Division 1  o   o   o   o   o   o   o   o
;;; Division 2  o o o o o o o o o o o o o o o o
;;;
;;; ACCENT =    5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1
;;;
;;; The grid divides up a single bar.  If a note's onset coincides
;;; with one of the columns, it has that column's accent, otherwise
;;; the accent is 0.  For convenience, we define the beat level as
;;; division level 0.  Higher multiples or lower divisions may be
;;; considered when defining accent.
;;;
;;; So for the above example, the accent values are split into:
;;;
;;; ACCENT =    5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1
;;; MULTIPLE =  2               1
;;; DIVISION =  3 1 2 1 3 1 2 1 3 1 2 1 3 1 2 1
;;; BEAT =      1       2       3       4     
;;; 
;;; The structure of multiple levels is a matter of
;;; interpretation/culture, so here we hard code a single grid for
;;; each time signature, representing the "most common" metrical
;;; structure.  For the division levels, we simply keep dividing the
;;; beat by two, until we can no longer use integer onset values (this
;;; limit is defined by the timebase).


;;; List of multiple-level accents for various time signatures,
;;; represented as (beat . accent).
;;;
;;; If a time signature isn't on this list, it will have zero
;;; multiple-level accent everywhere.
(defparameter *multiple-level-accents*
  '(;; 2/4
    (:pulses 2 :units 4 :accents ((1 . 1))) 
    ;; 3/4
    (:pulses 3 :units 4 :accents ((1 . 1))) 
    ;; 4/4
    (:pulses 4 :units 4 :accents ((1 . 2) (3 . 1))) 
    ;; 6/8
    (:pulses 6 :units 8 :accents ((1 . 2) (4 . 1))) 
    ;; 9/8
    (:pulses 9 :units 8 :accents ((1 . 2) (4 . 1) (7 . 1)))))
;; Create an accent hash table
(defun make-multiple-level-accents-table ()
  (let ((table (make-hash-table :test 'equal :size (length *multiple-level-accents*))))
    (dolist (accents *multiple-level-accents* table)
      (let ((key (list (getf accents :pulses)
		       (getf accents :units)))
	    (value (getf accents :accents)))
	  (setf (gethash key table) value)))))
;; Compute default accent hash table
(defparameter *multiple-level-accents-table* (make-multiple-level-accents-table))

;; Lookup mutliple-level accent for time signature
(defun time-signature->metrical-multiple-levels (pulses beatunits)
  (gethash (cons pulses (cons beatunits nil)) *multiple-level-accents-table*))

(defun metrical-accent-multiple (onset pulses barlength timebase)
  "Multiple-level metrical accent for note at onset."
  (declare (optimize speed) (fixnum onset pulses barlength timebase))
  (let* ((beatlength (/ barlength pulses))
	 (beatunit (/ timebase beatlength))
	 (accents (time-signature->metrical-multiple-levels pulses beatunit))
	 (posn (+ (/ (mod onset barlength) beatlength) 1))
	 (accent (cdr (assoc posn accents))))
    (if (null accent) 0 accent)))

(defparameter *factor2* (make-hash-table :test 'eq))
(defun factor2 (n)
  (labels ((f2 (n) ; 2s in prime factorisation
             (declare (fixnum n))
	     (if (zerop (mod n 2))
		 (+ (factor2 (/ n 2)) 1)
		 0)))
    (let ((fs (gethash n *factor2*)))
      (if fs
          fs
          (let ((fs (f2 n)))
            (setf (gethash n *factor2*) fs)
            fs)))))

(defun metrical-accent-division (onset pulses barlength)
  "Division-level metrical accent for note at onset."
  (declare (optimize speed) (fixnum onset pulses barlength))
  (let* (;; Length of single beat
         (beatlength (/ barlength pulses))
         ;; Maximum we can divide beat by 2
         (max-level (factor2 beatlength))
         ;; Assume no accent will be found
         (note-accent 0))
    ;; Iterate through levels (from beat level 0)
    (do ((level 0 (1+ level)))
        ((or (>= level max-level)  
             (not (= note-accent 0))) ; Stop if accent determined
         note-accent) ; Return note accent
      (if (zerop (mod onset (/ beatlength (expt 2 level))))
          (setf note-accent (- (1+ max-level) level))))))

;; Division-level metrical accent
(define-viewpoint (metaccent-div derived (onset))
    ((events md:music-sequence) element)
  :function (let ((event (last events)))
              (if (null event) +undefined+
		  ;; Temporal properties of this event
                  (let ((barlength (barlength event))
			(pulses (pulses event))
                        (onset (onset event)))
		    ;; Check these are properly defined
		    (if (or (undefined-p pulses onset barlength)
                            (zerop barlength)
                            (zerop pulses))
                        +undefined+
			(metrical-accent-division onset pulses barlength))))))


;; Multiple-level metrical accent
(define-viewpoint (metaccent-mult derived (onset))
    ((events md:music-sequence) element)
  :function (let ((event (last events)))
              (if (null event) +undefined+
		  ;; Temporal properties of this event
                  (let ((pulses (pulses event))
			(barlength (barlength event))
                        (onset (onset event))
			(timebase (md:timebase (last-element events))))
		    ;; Check these are properly defined
		    (if (or (undefined-p pulses onset barlength)
                            (zerop barlength)
                            (zerop pulses))
                        +undefined+
			(metrical-accent-multiple onset pulses barlength timebase))))))

;; Metrical accent
(define-viewpoint (metaccent derived (onset))
    ((events md:music-sequence) element)
  :function (let ((event (last events)))
              (if (null event) +undefined+
		  ;; Temporal properties of this event
                  (let ((pulses (pulses event))
			(barlength (barlength event))
                        (onset (onset event))
			(timebase (md:timebase (last-element events))))
		    ;; Check these are properly defined
		    (if (or (undefined-p pulses onset barlength)
                            (zerop barlength)
                            (zerop pulses))
                        +undefined+
			(+ (metrical-accent-multiple onset pulses barlength timebase)
			   (metrical-accent-division onset pulses barlength)))))))

;; Metric accent interval
(define-viewpoint (met-interval derived (onset))
    ((events md:music-sequence) element) 
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
                  (let ((ma1 (metaccent (list e1)))
                        (ma2 (metaccent (list e2))))
                    (if (undefined-p ma1 ma2) +undefined+
                        (- ma2 ma1)))))
  :function* (list (+ element (metaccent (list (penultimate-element events))))))

;; Metrical accent contour: -1 for a descending accent, 0 constant, 1
;; ascending
(define-viewpoint (met-contour derived (onset))
    ((events md:music-sequence) element) 
  :function (let ((metint (met-interval events)))
              (cond ((undefined-p metint) +undefined+)
                    (t (signum metint))))
  :function* (let ((accent (metaccent (list (penultimate-element events)))))
               (remove-if #'(lambda (a) (case element
                                          (-1 (>= a accent))
                                          (0  (not (= a accent)))
                                          (1  (<= a accent))))
                          (viewpoint-alphabet (get-viewpoint 'metaccent)))))
