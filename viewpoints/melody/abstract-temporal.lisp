(cl:in-package #:viewpoints)

(define-abstract-viewpoint (metpos (onset) 'metre (:barlength :phase) posinbar)
    ((events md:melodic-sequence) element)
    :function (lambda (barlength phase)
		(let ((onset (onset events)))
		  (mod (- onset phase) barlength))))

(define-abstract-viewpoint (bardist (onset) 'metre (:barlength :phase) bardist-train)
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

(define-viewpoint (bardist-train derived (onset barlength))
    ((events md:melodic-sequence) element)
  :function (let ((barlength (barlength events)))
	      (multiple-value-bind (e1 e2)
		  (values-list (last events 2))
		(cond 
		  ((null e1) +undefined+)
		  ((null e2) 1)
		  (t (let ((barnum-e1 (floor (/ (- (onset (list e1)) phase) barlength)))
			   (barnum-e2 (floor (/ (- (onset (list e2)) phase) barlength))))
		       (- barnum-e2 barnum-e1)))))))

(define-abstract-viewpoint (style-onset (onset) 'style () onset)
    ((events md:melodic-sequence) element)
  :function (lambda ()
	      (onset events)))

;;; DUMMY
(define-abstract-viewpoint (abstract-sdeg (sdeg) 'key (:keysig) sdeg)
    ((events md:melodic-sequence) element)
  :function (lambda ()
	      (sdeg events)))
