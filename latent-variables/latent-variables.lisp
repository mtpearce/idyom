(cl:in-package #:latent-variables)

(defparameter *phase-granularity* 1)

(define-latent-variable metre (:barlength :pulses) (:phase))

(defmethod get-latent-states (category (v metre))
  (let ((barlength (get-category-attribute category :barlength v)))
    (loop for phase below barlength by *phase-granularity* collecting
       (create-latent-state v category :phase phase))))

(defclass metre-phase (metre) ()
  (:documentation "Like metre, but calculates the prior distribution
based on the relative frequency of phases in the training data."))

(defmethod get-prior-distribution (training-data categories
					  (v metre-phase))
  (let* ((category-counts (mapcar #'length training-data))
	 (observation-count (apply #'+ category-counts))
	 (distribution))
    (loop for category in categories
       for category-count in category-counts
       for training-set in training-data do
	 (let* ((category-rel-freq (/ category-count observation-count))
		(phases
		 (mapcar #'(lambda (event-sequence) (md:bioi (first event-sequence)))
			 training-set))
		(phase-counts (utils:count-frequencies phases #'<)))
	   (loop for phase-count in phase-counts do
		(let ((phase (car phase-count))
		      (count (cdr phase-count)))
		  (let ((phase-rel-freq (/ count category-count)))
		    (push (cons (create-latent-state v category
						      :phase phase)
				(* category-rel-freq phase-rel-freq))
			  distribution))))))
    distribution))

(defmethod get-latent-states (category (v metre-phase))
  (loop for latent-state in (mapcar #'car (prior-distribution v))
     when (equal (get-category latent-state v) category)
       collect latent-state))

(define-latent-variable key () (:keysig :mode))

(defmethod get-latent-states (category (v key))
  (let ((latent-states))
    (loop for mode in '(0 9) do
	 (dotimes (key 12)
	   (push (create-latent-state v category :keysig (- key 7) :mode mode)
		 latent-states)))
    latent-states))

(define-latent-variable key-mode-models (:mode) (:keysig :mode))

(defmethod get-latent-states (category (v key-mode-models))
  (let ((latent-states))
    (dotimes (key 12)
      (push (create-latent-state v category :keysig (- key 7))
	    latent-states))
    latent-states))

(define-latent-variable style (:style) ())

(defmethod get-event-category (event (v style))
  (intern (md:description event)))
