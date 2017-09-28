(cl:in-package #:viewpoints)

;;;; The definition of an abstract viewpoint involves additional parameters compared
;;;; to those required for defining regular derived viewpoints. 
;;;; <event-attributes> represents the set of basic event attributes that this viewpoint
;;;; is a function of. 
;;;; <parameters> specifies any extra interpretation parameters that are provided
;;;; by the latent state. These can be parameters that have a fixed value in the event
;;;; representation (such as phase, which is always zero in the event representation
;;;; since the onset times are aligned to the first beat of the first bar).
;;;; <function> is a lambda function whose parameters correspond to respectively the
;;;; event-attributes and interpretation parameters in in a single list.
;;;; Each interpretation parameter has to be optional with a default value specified
;;;; to be used during training.

(defun phase-adjust (timepoint barlength phase)
  (+ timepoint (mod (- barlength phase)
       barlength)))

(define-abstract-viewpoint (abs-posinbar (onset) (:barlength) (:phase) abs-posinbar-train)
    ((events md:melodic-sequence) element)
  :function (lambda (barlength &optional (phase 0))
	      (get-posinbar (phase-adjust (onset events) barlength phase) barlength)))

(define-abstract-viewpoint (bardist (onset) (:barlength) (:phase) bardist-train)
    ((events md:melodic-sequence) element)
  :function (lambda (barlength &optional (phase 0))
	      (multiple-value-bind (e1 e2)
		  (values-list (last events 2))
		(if (null e1) +undefined+
		    (let* ((barnum-e1 (floor (/ (phase-adjust (onset (list e1))
							      barlength phase)
						barlength))))
		      (if (null e2) barnum-e1
			  (let ((barnum-e2 (floor (/ (phase-adjust (onset (list e2))
								   barlength phase)
						     barlength))))
			    (- barnum-e2 barnum-e1))))))))

(define-abstract-viewpoint (bardist-legacy (onset) (:barlength) (:phase) bardist-legacy-train)
    ((events md:melodic-sequence) element)
  :function (lambda (barlength &optional (phase 0))
	      (multiple-value-bind (e1 e2)
		  (values-list (last events 2))
		(if (or (null e1) (null e2)) +undefined+
		    ;; Is e1 in the same bar as e2?
		    (let ((barnum-e1 (floor (/ (- (onset (list e1)) phase) barlength)))
			  (barnum-e2 (floor (/ (- (onset (list e2)) phase) barlength))))
		      (- barnum-e2 barnum-e1))))))

(defun get-tonint (cpitch octave keysig mode)
  (- cpitch
     (+ (* octave *octave*)
	(get-referent keysig mode))))

(define-abstract-viewpoint (abs-ioi (onset) () () abs-ioi-training)
    ((events md:melodic-sequence) element) 
  :function (lambda () (ioi events)))

;; Pitch to tonic interval
(define-abstract-viewpoint (abs-tonint (cpitch) (:keysig :mode) () abs-tonint-training)
    ((events md:melodic-sequence) element) 
  :function (lambda (keysig mode)
	      (get-tonint (cpitch events) (octave (list (car events)))
			  keysig mode)))


(define-abstract-viewpoint (abs-sdeg-west (cpitch) (:keysig :mode) () abs-sdeg-west-training)
    ((events md:melodic-sequence) element) 
  :function (lambda (keysig mode)
	      (let* ((pitch (cpitch events))
		     (interval (get-tonint pitch (octave (list (car events)))
					   keysig mode))
		     (scale-intervals (case mode
					(0 viewpoints::*major-intervals*)
					(9 viewpoints::*minor-intervals*)))
		     (degree (degree interval scale-intervals)))
		(if (null degree)
		    pitch
		    (+ degree
		       (* (- (utils:quotient interval *octave*)
			     (if (< interval 0) 1 0))
			  7))))))
