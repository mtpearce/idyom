(cl:in-package #:viewpoints)

(defun calculate-metrical-position (onset interpretation)
  (let ((barlength (md:barlength interpretation))
	(phase (md:interpretation-phase interpretation)))
      (mod (- onset phase) barlength)))

; A derived viewpoint that calculates metrical position under an interpretation as a proportion of the time signature's period
(define-metrical-viewpoint (metrical-onset-position metrical (onset))
    ((events md:melodic-sequence)
     (interpretation md:metrical-interpretation) element)
  :function (let ((onset (onset events)))
	      (calculate-metrical-position onset interpretation)))

(defun calculate-metrical-accent (onset interpretation)
    (let ((pulses (md:pulses interpretation))
	  (barlength (md:barlength interpretation))
	  (phase (md:interpretation-phase interpretation))
	  (timebase (md:timebase interpretation)))
	(+ (metrical-accent-multiple 
	    (- onset phase) pulses barlength timebase)
	   (metrical-accent-division (- onset phase) pulses barlength))))

(define-metrical-viewpoint (metrical-onset-accent metrical (onset))
    ((events md:melodic-sequence) 
     (interpretation md:metrical-interpretation) element)
  :function (let ((event (last events))
		  (onset (onset events)))
              (if (null event) +undefined+
		  (calculate-metrical-accent onset interpretation))))

(define-metrical-viewpoint (bardist metrical (onset))
    ((events md:melodic-sequence)
     (interpretation md:metrical-interpretation) element)
  :function (multiple-value-bind (e1 e2)
                (values-list (last events 2))
              (if (or (null e1) (null e2)) +undefined+
		  (let ((barlength (md:barlength interpretation))
			(phase (md:interpretation-phase interpretation)))
		    ;; Is e1 in the same bar as e2?
		    (let ((barnum-e1 (floor (/ (- (onset (list e1)) phase) barlength)))
			  (barnum-e2 (floor (/ (- (onset (list e2)) phase) barlength))))
		      (- barnum-e2 barnum-e1))))))

