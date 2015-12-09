(cl:in-package #:viewpoints)

;; Metrical accent
(define-metrical-viewpoint (metrical-onset-accent metrical (onset))
    ((events md:melodic-sequence) 
     (interpretation md:metrical-interpretation) element)
  :function (let ((event (last events)))
              (if (null event) +undefined+
                  (let ((pulses (md:pulses interpretation))
			(barlength (md:barlength interpretation))
                        (onset (onset events))
			(timebase (md:timebase (last-element events))))
		    (+ (metrical-accent-multiple onset pulses barlength timebase)
			   (metrical-accent-division onset pulses barlength))))))
