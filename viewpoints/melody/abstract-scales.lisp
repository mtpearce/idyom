(cl:in-package #:viewpoints)

(defun get-tonint (cpitch octave keysig mode)
  (- cpitch
     (+ (* octave *octave*)
	(get-referent keysig mode))))

(define-abstract-viewpoint (abs-tonint (cpitch) (:keysig :mode) () abs-tonint-training)
    ((events md:melodic-sequence) element) 
  :function (lambda (keysig mode)
	      (get-tonint (cpitch events) (octave (list (car events)))
			  keysig mode)))

(define-abstract-viewpoint (abs-sdeg-west (cpitch) (:keysig :mode) () abs-sdeg-west-training)
    ((events md:melodic-sequence) element) 
  :function (lambda (keysig mode)
	      (let* ((pitch (cpitch events))
		     (interval (get-tonint pitch (octave (list (car events)))
					   keysig mode))
		     (scale-intervals (case mode
					(0 viewpoints::*major-intervals*)
					(9 viewpoints::*minor-intervals*)))
		     (degree (degree interval scale-intervals)))
		(if (null degree)
		    pitch
		    (+ degree
		       (* (- (utils:quotient interval *octave*)
			     (if (< interval 0) 1 0))
			  7))))))
