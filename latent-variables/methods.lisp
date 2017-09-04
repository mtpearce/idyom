(cl:in-package #:latent-variables)

(defmethod latent-variable-attribute ((v latent-variable))
  (type-of v))

(defmethod latent-variable-attribute ((l linked))
  (mapcar #'latent-variable-attribute (latent-variable-links l)))

(defmethod latent-variable-name ((l latent-variable))
  (string-downcase (symbol-name (latent-variable-attribute l))))

(defmethod latent-variable-name ((l linked))
  (format nil "~{~A~^-~}" (latent-variable-attribute l)))

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

(defmethod get-sub-category (category (sub-v latent-variable) (l latent-variable))
  category)

(defmethod get-sub-category (category (sub-v latent-variable) (l linked))
  (let ((annotated-latent-state (utils:make-plist (category-parameters l)
						  category)))
    (mapcar #'(lambda (p) (getf annotated-latent-state p))
	    (category-parameters sub-v))))
	 

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
  
(defmethod initialise-prior-distribution (training-data
					  (v latent-variable))
  (let* ((categories (mapcar #'car training-data))
	 (category-counts (mapcar #'length training-data))
	 (total-observations (apply #'+ category-counts))
	 (latent-states) (distribution))
    (loop for category in categories for count in category-counts do
	 (let ((rel-freq (/ count total-observations)))
	   (loop for latent-state in (get-latent-states category v) do
		(push latent-state latent-states)
		(push rel-freq distribution))))
    (mapcar #'cons
	    latent-states
	    (let ((scaling (apply #' + distribution)))
	      (mapcar #'(lambda (p) (/ p scaling)) distribution)))))

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

(defmethod initialise-prior-distribution (training-data
					  (v metre-phase))
  (let* ((categories (mapcar #'car training-data))
	 (training-sets (mapcar #'cdr training-data))
	 (category-counts (mapcar #'length training-sets))
	 (observation-count (apply #'+ category-counts))
	 (distribution))
    (loop for category in categories
       for category-count in category-counts
       for training-set in training-sets do
	 (let* ((category-rel-freq (/ category-count observation-count))
		(phases
		 (mapcar #'(lambda (event-sequence) (md:bioi (first event-sequence)))
			 training-set))
		(phase-counts (utils:count-frequencies phases #'<)))
	   (loop for phase-count in phase-counts do
		(let ((phase (car phase-count))
		      (count (cdr phase-count)))
		  (let ((phase-rel-freq (/ count category-count)))
		    (push (cons (create-latent-state category
						     (create-interpretation
						      v :barlength
						      (get-category-parameter
						       category :barlength v)
						      :phase phase) v)
				(* category-rel-freq phase-rel-freq))
			  distribution))))))
    distribution))
	   
;;;;;; KEY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-latent-states (category (v key))
  (loop for key below 12 collecting
       (create-latent-state category
			    (create-interpretation v :keysig key)
			    v)))

;;;;;; STYLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-latent-states (category (v style))
  (create-latent-state category nil v))
