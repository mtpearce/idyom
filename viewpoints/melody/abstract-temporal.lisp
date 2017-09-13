(cl:in-package #:viewpoints)

(define-abstract-viewpoint (metpos (onset) (:barlength :phase) posinbar)
    ((events md:melodic-sequence) element)
    :function (lambda (barlength phase)
		(let ((onset (onset events)))
		  (mod (- onset phase) barlength))))

(define-abstract-viewpoint (bardist (onset) (:barlength :phase) bardist-train)
    ((events md:melodic-sequence) element)
  :function (lambda (barlength phase)
	      (multiple-value-bind (e1 e2)
		  (values-list (last events 2))
		(if (null e1) +undefined+
		    (let ((barnum-e1 (floor (/ (+ (onset (list e1))
						  (mod (- barlength phase)
						       barlength))
					       barlength))))
		      (if (null e2) barnum-e1
			  (let ((barnum-e2 (floor (/ (+ (onset (list e2))
							(mod (- barlength phase)
							     barlength))
						     barlength))))
			    (- barnum-e2 barnum-e1))))))))

(define-viewpoint (bardist-train derived (onset))
    ((events md:melodic-sequence) element)
  :function (let ((barlength (barlength events)))
	      (multiple-value-bind (e1 e2)
		  (values-list (last events 2))
		(if (null e1) +undefined+
		    (let ((barnum-e1 (floor (/ (onset (list e1))
					       barlength))))
		      (if (null e2) barnum-e1
			  (let ((barnum-e2 (floor (/ (onset (list e2))
						     barlength))))
			    (- barnum-e2 barnum-e1))))))))


;;; DUMMY-VIEWPOINTS for testing only
(define-abstract-viewpoint (style-onset (onset) () onset)
    ((events md:melodic-sequence) element)
  :function (lambda (style)
	      (onset events)))


(define-abstract-viewpoint (abstract-sdeg (sdeg) (:keysig) sdeg)
    ((events md:melodic-sequence) element)
  :function (lambda ()
	      (sdeg events)))
