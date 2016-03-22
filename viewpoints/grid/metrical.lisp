;; To replicate the metrical-interpretation model 
;; as implemented here: https://github.com/bjvanderweij/exposure, 
;; use the metrical position viewpoint linked with isonset and 0th-order models

(cl:in-package #:viewpoints)

; A derived viewpoint that calculates metrical position under an interpretation
(define-metrical-viewpoint (metrical-position metrical (pos))
    ((events md:grid-sequence)
     (interpretation md:metrical-interpretation) element)
  :function (let ((position (pos events))
		  (period (md:meter-period interpretation))
		  (phase (md:meter-phase interpretation)))
	      (mod (- position phase) period))
  :function* (let ((period (md:meter-period interpretation))
		   (phase (md:meter-phase interpretation)))
	       (remove-if #'(lambda (e) (not (equalp element (mod (- (pos e) phase) period))))
			  (viewpoint-alphabet (get-viewpoint 'position)))))

;; Metrical accent
(define-metrical-viewpoint (metrical-accent metrical (pos))
    ((events md:grid-sequence) 
     (interpretation md:metrical-interpretation) element)
  :function (let ((event (last events)))
              (if (null event) +undefined+
                  (let ((pulses (md:pulses interpretation))
			(barlength (md:barlength interpretation))
			(phase (md:meter-phase interpretation))
			(position (pos events))
			(resolution (md:resolution (last-element events)))
			(timebase (md:timebase (last-element events))))
		    (let ((phase-corrected-time
			   (* (/ timebase resolution) (- position phase))))
		      (+ (metrical-accent-multiple 
			  phase-corrected-time pulses barlength timebase)
			 (metrical-accent-division phase-corrected-time pulses barlength)))))))
