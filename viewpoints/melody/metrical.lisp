(cl:in-package #:viewpoints)

; A derived viewpoint that calculates metrical position under an interpretation as a proportion of the time signature's period
(define-metrical-viewpoint (metrical-onset-position metrical (onset))
    ((events md:melodic-sequence)
     (interpretation md:metrical-interpretation) element)
  :function (let ((period (md:meter-period interpretation))
		  (phase (md:meter-phase interpretation))
		  (timebase (md:timebase (last-element events)))
		  (resolution (md:resolution interpretation))
		  (onset (onset events)))
	      (let ((phase-corrected-onset
		     (- onset (* phase (/ timebase resolution))))
		    (converted-period (* period (/ timebase resolution))))
		(/ (mod phase-corrected-onset converted-period) converted-period))))

(define-metrical-viewpoint (metrical-onset-accent metrical (onset))
    ((events md:melodic-sequence) 
     (interpretation md:metrical-interpretation) element)
  :function (let ((event (last events)))
              (if (null event) +undefined+
                  (let ((pulses (md:pulses interpretation))
			(barlength (md:barlength interpretation))
			(phase (md:meter-phase interpretation))
			(onset (onset event))
			(resolution (md:resolution interpretation))
			(timebase (md:timebase interpretation)))
		    (let ((phase-corrected-onset
			   (- onset (* phase (/ timebase resolution)))))
		      (+ (metrical-accent-multiple 
			  phase-corrected-onset pulses barlength timebase)
			 (metrical-accent-division phase-corrected-onset pulses barlength)))))))
