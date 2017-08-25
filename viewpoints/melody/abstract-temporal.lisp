(cl:in-package #:viewpoints)

(define-abstract-viewpoint (metpos (onset) (metre) (:barlength :phase))
    ((events md:melodic-sequence) element)
    :function (lambda (barlength phase)
		(let ((onset (onset events)))
		  (mod (- onset phase) barlength))))

(define-abstract-viewpoint (bardist (onset) (metre) (:barlength :phase))
    ((events md:melodic-sequence) element)
  :function (lambda (barlength phase)
	      (multiple-value-bind (e1 e2)
		  (values-list (last events 2))
		(cond 
		  ((null e1) +undefined+)
		  ((null e2) 1)
		  (t (let ((barnum-e1 (floor (/ (- (onset (list e1)) phase) barlength)))
                           (barnum-e2 (floor (/ (- (onset (list e2)) phase) barlength))))
                       (- barnum-e2 barnum-e1)))))))
