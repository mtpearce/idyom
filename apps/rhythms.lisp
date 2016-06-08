(cl:in-package #:rhythms)

(defun grid->grid-events (grid &key (resolution 16)
				 (interpretation nil)
				 (timebase 96))
  (loop for is-onset in grid for position to (length grid)
     collecting (make-grid-event (if (eql is-onset 1) t nil)
				 position 
				 :interpretation interpretation
				 :resolution resolution
				 :timebase timebase)))

(defun ioi-list->event-sequence (texture ioi-list &rest kwargs &key (repetitions 1) &allow-other-keys)
  (let ((ioi-list (repeat-list ioi-list repetitions)))
    (cond 
      ((eql texture :grid) (apply #'ioi-list->grid-events (cons ioi-list kwargs)))
      ((eql texture :melody) (apply #'ioi-list->melodic-events
				    (cons ioi-list kwargs))))))
					    

(defun ioi-list->grid-events (ioi-list &key 
					 (source-resolution 8) 
					 (target-resolution 16) 
					 (timebase 96)
					 (interpretation nil)
					 (phase 0) &allow-other-keys)
  (setf ioi-list (mapcar #'(lambda (ioi) (md:rescale ioi target-resolution source-resolution)) ioi-list))
  (let ((onset-times (cons 0 (apply #'utils:cumsum ioi-list))))
    (loop for position in (utils:generate-integers 0 (+ phase (car (last onset-times))))
	 collecting (make-grid-event (if (member (- position phase) onset-times) t nil) 
				     position 
				     :interpretation interpretation
				     :resolution target-resolution
				     :timebase timebase))))

(defun ioi-list->melodic-events (ioi-list &rest kwargs &key (phase 0) (timebase 96) (interpretation nil)
					    &allow-other-keys)
  (let* ((onsets (cons phase (apply #'utils:cumsum ioi-list))))
    (loop for onset in onsets for duration in ioi-list collecting
	 (apply #'make-melodic-event (append (list onset duration duration :timebase timebase :interpretation interpretation)
					     kwargs)))))
	
    
(defun make-grid-event (is-onset position &key
					   (timebase 96)
					   (resolution 16) 
					   (interpretation nil))
  (make-instance 'md::grid-event
		 :is-onset is-onset
		 :pos position
		 :cpitch nil
		 :onset (when is-onset (md:rescale position timebase resolution))
		 :duration (/ timebase resolution)
		 :barlength (when interpretation
			      (md:barlength interpretation))
		 :pulses (when interpretation
			   (md:pulses interpretation))
		 :resolution resolution
		 :id (md:make-event-id 0 0 0)
		 :timebase timebase))

(defun make-melodic-event (onset duration bioi &key (timebase 96) (source-resolution 8) (interpretation nil))
  (make-instance 'md::music-event
		 :onset (* onset (/ timebase source-resolution))
		 :duration duration
		 :cpitch nil
		 :bioi bioi
		 :barlength (when interpretation
			      (md:barlength interpretation))
		 :pulses (when interpretation
			   (md:pulses interpretation))
		 :id (md:make-event-id 0 0 0)
		 :timebase timebase))

(defun repeat-list (list repetitions)
  (apply #'append (loop for x below repetitions collecting (copy-list list))))

(defun make-rhythm (iois &key (r 16))
  (lambda (repetitions &key (r r)) 
    (ioi-list->grid-events (repeat-list iois repetitions) :target-resolution r)))

;; Some rhythms convenient for testing
(defparameter agbekor (make-rhythm '(2 2 1 2 2 2 1) :r 16))
(defparameter shave-and-a-haircut (make-rhythm '(2 1 1 2 4 2 4) :r 16))
(defparameter son-clave (make-rhythm '(3 3 4 2 2) :r 16))
