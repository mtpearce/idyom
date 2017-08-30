(cl:in-package #:latent-variables)

(defmethod latent-state-parameters ((v latent-variable))
  (union (category-parameters v) (interpretation-parameters v)))

(defmethod get-category-parameter (category parameter (v latent-variable))
  (let ((param-category (utils:make-plist (category-parameters v) category)))
    (getf param-category parameter)))

(defmethod get-interpretation-parameter (interpretation parameter (v latent-variable))
  (let ((param-interpretation (utils:make-plist (interpretation-parameters v)
						interpretation)))
    (getf param-interpretation parameter)))

(defmethod get-category (latent-state (v latent-variable))
  (mapcar #'(lambda (param) (getf latent-state param))
	  (category-parameters v)))

(defmethod get-interpretation (latent-state (v latent-variable))
  (mapcar #'(lambda (param) (getf latent-state param))
	  (interpretation-parameters v)))

(defmethod get-event-category (event (v latent-variable))
  (let ((event-attributes (mapcar #'(lambda (attrib)
				      (symbol-name attrib))
				  (category-parameters v))))
    (loop for attrib in event-attributes collect
	 (apply (find-symbol attrib (find-package :md)) (list event)))))

(defmethod create-interpretation ((v latent-variable)
				  &rest keys &key &allow-other-keys)
  (mapcar (lambda (key) (getf keys key)) (interpretation-parameters v)))

(defmethod create-latent-state (category interpretation (v latent-variable))
  (let ((parameters (append (utils:make-plist (category-parameters v) category)
			    (utils:make-plist (interpretation-parameters v) interpretation))))
    (mapcar (lambda (param) (getf parameters param))
	    (latent-state-parameters v))))
  
(defmethod initialise-prior-distribution (categories training-data
					  (v latent-variable))
  (let* ((category-counts (mapcar #'length training-data))
	 (total-observations (apply #'+ category-counts))
	 (latent-states) (distribution))
    (loop for category in categories for count in category-counts collecting
	 (let ((rel-freq (/ count total-observations)))
	   (loop for latent-state in (get-latent-states category v) do
		(progn
		  (push latent-state latent-states)
		  (push rel-freq distribution)))))
    (values latent-states (let ((scaling (apply #' + distribution)))
			    (mapcar #'(lambda (p) (* p scaling)) distribution)))))

(defmethod get-latent-states (category (l linked))
  (let* ((variables (latent-variable-links l))
	 (categories (mapcar #'(lambda (var) (loop for param in (category-parameters var)
						collect (get-category-parameter category
										param l)))
			     variables)) ; The category of each variable
	 (latent-states (mapcar #'(lambda (var category) (get-latent-states category var))
				variables categories)) ; Latent states for each variable
	 (combined-latent-states (print (apply #'utils:cartesian-product latent-states)))
	 ;; So ugly...
	 (combined-latent-states (mapcar (lambda (combined-latent-state)
					   (apply #'append
						  (mapcar (lambda (latent-state var)
							    (utils:make-plist
							     (latent-state-parameters var)
							     latent-state))
							  combined-latent-state variables)))
					 combined-latent-states)))
    (loop for latent-state in combined-latent-states collect
	 (loop for param in (latent-state-parameters l) collect
	      (getf latent-state param)))))

;;;;;; METRE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-latent-states (category (v metre))
  (let ((barlength (get-category-parameter category :barlength v)))
    (loop for phase below barlength collecting
       (create-latent-state category
			    (create-interpretation v :barlength barlength :phase phase)
			    v))))

;;;;;; METRE (empirical phase prior) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialise-prior-distribution (categories training-data
					  (v metre-phase))
  (let* ((category-counts (mapcar #'length training-data))
	 (observation-count (apply #'+ category-counts))
	 (latent-states) (distribution))
    (loop for category in categories for category-count in category-counts collecting
	 (let ((category-rel-freq (/ category-count observation-count))
	       (phases
		(mapcar #'(lambda (event-sequence) (md:bioi (first event-sequence)))))
	       (unique-phases (remove-duplicates phases))
	       (phase-counts (mapcar #'(lambda (phase)
					 (loop for p in phases counting
					      (equalp p phase) into count
					    finally (return counts)))
				     unique-phases)))
	   (loop for phase in unique-phases for phase-count in phase-counts do
		(let ((phase-rel-freq (/ count phase-count category-count)))
		  (push (append category phase) latent-states)
		  (push (* category-rel-freq phase-rel-freq) distribution)))))
    (values latent-states distribution)))
	   
;;;;;; KEY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-latent-states (category (v key))
  (loop for key below 12 collecting
       (create-latent-state category
			    (create-interpretation v :keysig key)
			    v)))

;;;;;; STYLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-latent-states (category (v style))
  (create-latent-state category nil v))
