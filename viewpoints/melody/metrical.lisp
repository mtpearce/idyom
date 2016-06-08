(cl:in-package #:viewpoints)

(defun calculate-metrical-position (onset interpretation)
  (let ((barlength (md:barlength interpretation))
	(timebase (md:timebase interpretation))
	(phase (md:meter-phase interpretation)) ;; Confusing: phase is specified in grid-points
	(resolution (md:resolution interpretation)))
    (let ((phase-corrected-onset
	   (- onset (* phase (/ timebase resolution)))))
      ;;      (/ (mod phase-corrected-onset barlength) barlength))))
      (mod phase-corrected-onset barlength))))

; A derived viewpoint that calculates metrical position under an interpretation as a proportion of the time signature's period
(define-metrical-viewpoint (metrical-onset-position metrical (onset))
    ((events md:melodic-sequence)
     (interpretation md:metrical-interpretation) element)
  :function (let ((onset (onset events)))
	      (calculate-metrical-position onset interpretation)))

(defun calculate-metrical-accent (onset interpretation)
    (let ((pulses (md:pulses interpretation))
	  (barlength (md:barlength interpretation))
	  (phase (md:meter-phase interpretation))
	  (resolution (md:resolution interpretation))
	  (timebase (md:timebase interpretation)))
      (let ((phase-corrected-onset
	     (- onset (* phase (/ timebase resolution)))))
	(+ (metrical-accent-multiple 
	    phase-corrected-onset pulses barlength timebase)
	   (metrical-accent-division phase-corrected-onset pulses barlength)))))

(define-metrical-viewpoint (metrical-onset-accent metrical (onset))
    ((events md:melodic-sequence) 
     (interpretation md:metrical-interpretation) element)
  :function (let ((event (last events))
		  (onset (onset events)))
              (if (null event) +undefined+
		  (calculate-metrical-accent onset interpretation))))

