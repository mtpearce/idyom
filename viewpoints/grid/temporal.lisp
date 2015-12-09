;; To replicate the metrical-interpretation model 
;; as implemented here: https://github.com/bjvanderweij/exposure, 
;; use the metrical position viewpoint linked with isonset and 0th-order models

(cl:in-package #:viewpoints)

(define-viewpoint (grid-onset derived (onset is-onset))
    ((events md:grid-sequence) element)
  :function (let ((is-onset (is-onset events))
		  (onset (onset events)))
	      (if is-onset onset +undefined+)))

(define-viewpoint (grid-onset-position derived (pos is-onset))
    ((events md:grid-sequence) element)
  :function (let ((is-onset (is-onset events))
		  (position (pos events)))
	      (if is-onset position +undefined+)))

(define-viewpoint (grid-dur derived (dur is-onset))
    ((events md:grid-sequence) element)
  :function (let ((is-onset (is-onset events))
		  (dur (dur events)))
	      (if is-onset dur +undefined+)))

(define-viewpoint (grid-cpitch derived (cpitch is-onset))
    ((events md:grid-sequence) element)
  :function (let ((is-onset (is-onset events))
		  (cpitch (cpitch events)))
	      (if is-onset cpitch +undefined+)))

;; IOI in grid points between current position and last onset
(define-viewpoint (grid-ioi derived (grid-onset pos))
    ((events md:grid-sequence) element)
  :function (let* ((last-onset-position (grid-onset events))
		   (position (pos events)))
	      (- position last-onset-position)))

;; Like onset, but defined at every event
(define-viewpoint (grid-time derived (pos))
    ((events md:grid-sequence) element)
  :function (let ((timebase (md:timebase (last-element events)))
	(resolution (md:resolution (last-element events)))
	(position (pos events)))
    (* position (/ timebase resolution))))

(define-metrical-viewpoint (metrical-position-onset metrical (is-onset metrical-position))
    ((events md:grid-sequence)
     (interpretation md:metrical-interpretation) element)
  :function (let ((is-onset (is-onset events))
		  (metrical-position (metrical-position events interpretation)))
	      (if is-onset metrical-position +undefined+)))




