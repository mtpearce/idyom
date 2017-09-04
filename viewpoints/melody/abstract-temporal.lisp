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
		(cond 
		  ((null e1) +undefined+)
		  ((null e2) 1)
		  (t (let ((barnum-e1 (floor (/ (- (onset (list e1)) phase) barlength)))
                           (barnum-e2 (floor (/ (- (onset (list e2)) phase) barlength))))
                       (- barnum-e2 barnum-e1)))))))

(define-viewpoint (bardist-train derived (onset))
    ((events md:melodic-sequence) element)
  :function (let ((barlength (barlength events)))
	      (multiple-value-bind (e1 e2)
		  (values-list (last events 2))
		(cond 
		  ((null e1) +undefined+)
		  ((null e2) 1)
		  (t (let ((barnum-e1 (floor (/ (onset (list e1)) barlength)))
			   (barnum-e2 (floor (/ (onset (list e2)) barlength))))
		       (- barnum-e2 barnum-e1)))))))


;;; DUMMY-VIEWPOINTS for testing only
(define-abstract-viewpoint (style-onset (onset) (:style) onset)
    ((events md:melodic-sequence) element)
  :function (lambda (style)
	      (onset events)))


(define-abstract-viewpoint (abstract-sdeg (sdeg) (:keysig) sdeg)
    ((events md:melodic-sequence) element)
  :function (lambda ()
	      (sdeg events)))
