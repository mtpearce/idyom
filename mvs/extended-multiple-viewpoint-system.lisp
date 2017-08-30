(cl:in-package #:mvs)

;;;========================================================================
;;; Data Structures
;;;========================================================================

(defclass combined-mvs () 
  (   (mvs-models :reader mvs-models
		  :initarg :generative-mvs-models
		  :type (vector generative-mvs)))
  (:documentation "A combined multiple viewpoint system (MVS) consists 
    zero or one MVSs and one or more generative MVSs."))

(defclass abstract-mvs (mvs)
  ((stm :initarg :stm :type (vector vector))
   (ltm :initarg :ltm :type (vector vector))
   (latent-variable :accessor mvs-latent-variable
		    :initarg :latent-variable))
  (:documentation ""))

(defclass generative-mvs (mvs)
  ((abstract-mvs-models :reader abstract-mvs-models
			:initarg :abstract-mvs-models
			:type (vector abstract-mvs)))
  (:documentation "A generative multiple viewpoint system (MVS) consists 
of a list of long- and short-term models, with one model per category
per viewpoint."))

;;;========================================================================
;;; Model Initialisation 
;;;========================================================================

(defun make-mvs (basic-viewpoints viewpoints ltms)
  "Returns an mvs object initialised with <viewpoints>, a list of
viewpoint objects, <ltm> and <stm> each of which is a list of ppm
objects."
  (let* ((viewpoints-ltms (mapcar #'cons viewpoints ltms))
	 (abstract-viewpoints-ltms (remove-if (lambda (vm) (lv:abstract? (car vm)))
					      viewpoints-ltms))
	 (viewpoints-ltms (remove-if (lambda (vm) (not (lv:abstract? (car vm))))
				     viewpoints-ltms))
	 (generative-mvs-models (when abstract-viewpoints-ltms
				  (make-generative-mvs-models basic-viewpoints
							      abstract-viewpoints-ltms)))
	 (mvs (let* ((viewpoints (mapcar #'car viewpoints-ltms))
		     (ltms (mapcar #'cdr viewpoints-ltms))
		     (merged-typeset (apply #'append (mapcar #'viewpoints:viewpoint-typeset
							     viewpoints)))
		     (basic-viewpoints (remove-if #'(lambda (bv)
						      (not (member (type-of bv)
								   merged-typeset)))
						  basic-viewpoints)))
		(initialise-mvs basic-viewpoints viewpoints ltms))))
    (if (null generative-mvs-models)
	 mvs
	 (make-instance 'combined-mvs :mvs-models (cons mvs generative-mvs-models)))))

(defun make-generative-mvs-models (basic-viewpoints abstract-viewpoints-ltms)
  (let ((variable-sets (get-variable-sets (mapcar #'car abstract-viewpoints-ltms))))
    (loop for variable-set in variable-sets collecting
	 (let* ((viewpoints-ltms (remove-if (lambda (vm)
					      (not (member 
						    (viewpoints:latent-variable-attribute
						     (car vm))
						    variable-set)))
					    abstract-viewpoints-ltms)) ; Obtain all viewpoints and corresponding ltms conditioned on any of the variables in this variable set
		(latent-variable 
		(merged-typeset (apply #'append (mapcar #'viewpoints:viewpoint-typeset
							viewpoints-ltms)))
		(basic-viewpoints (remove-if #'(lambda (bv)
						 (not (member (type-of bv)
							      merged-typeset)))
					     basic-viewpoints))) ;; Collect all basic viewpoints that can be predicted by this generative-mvs
	   (let ((abstract-mvs (initialise-mvs basic-viewpoints
					       (mapcar #'car viewpoints-ltms)
					       (mapcar #'cdr viewpoints-ltms)
					       :class 'abstract-mvs)))
	     (setf (mvs-latent-variable abstract-mvs) variable-set))))))

(defun initialise-mvs (basic-viewpoints viewpoints ltms &key (class 'mvs))
  (flet ((sanity-check-basic-viewpoints ()
           (dolist (bv basic-viewpoints basic-viewpoints)
             (unless (find (type-of bv) viewpoints 
                           :key #'viewpoints:viewpoint-typeset
                           :test #'(lambda (x y) (member x y)))
               (warn "~&None of the supplied viewpoints can predict basic feature ~A.~%"
                     (viewpoints:viewpoint-name bv)))))
         (sanity-check-viewpoints ()
           (dolist (v viewpoints viewpoints)
             (unless (some #'(lambda (bv) 
                               (find (type-of bv) 
                                     (viewpoints:viewpoint-typeset v)
                                     :test #'eql))
                           basic-viewpoints)
               (warn "~&Viewpoint ~A cannot predict any of the supplied basic features.~%"
                     (viewpoints:viewpoint-name v))))))
    ;;(format t "~&MAKE-MVS: basic-viewpoints = ~A~&MAKE-MVS: viewpoints = ~A"
    ;;        (sanity-check-basic-viewpoints) 
    ;;        (sanity-check-viewpoints))
    (let ((mvs (make-instance class
                              :basic (sanity-check-basic-viewpoints)
                              :viewpoints (apply #'vector 
                                                 (sanity-check-viewpoints))
                              :ltm (apply #'vector ltms)
                              :stm (get-short-term-models viewpoints))))
      (set-mvs-parameters mvs)
      mvs)))

(defun get-variable-sets (abstract-viewpoints)
  (let* ((variable-attributes (mapcar #'latent-variable-attribute
				      (mapcar #'viewpoints:latent-variable-attribute
					      abstract-viewpoints)))
	 (attribute-set (remove-duplicates variable-attribute :test #'set-equal)))
    (filter-and-merge-var-sets attribute-set)))

(defun set-equal (a b &key (test #'eql))
  (when (and (null (set-difference a b :test test))
	     (null (set-difference b a :test test)))
    a))

(defun filter-and-merge-var-sets (var-sets &optional result)
  (if (null var-sets) (reverse result)
      (let* ((var-set (car var-sets))
	     (remaining-var-sets (cdr var-sets))
	     (new-remaining-var-sets)
	     (discard?))
	(loop
	   for other-var-set in remaining-var-sets do
	 (let ((set-diff (set-difference var-set other-var-set))
	       (set-diff-r (set-difference other-var-set var-set)))
	   (cond ((and (null set-diff)
		       (not (null set-diff-r))) ; var-set is a subset of other-var-set
		  (progn
		    (setf discard? t)
		    (push other-var-set new-remaining-var-sets))) ; discard  var set
		 ((and (null set-diff-r) ;; other-var-set is a subset of var-set
		       (not (null set-diff)))
		  ()) ; do nothing (discard other-var-set)
		 ((not (null (intersection var-set other-var-set))) ; var-set and other-var set contain overlapping values
		  (progn
		    (push (union var-set other-var-set) new-remaining-var-sets) ; merge var-set and other-var-set
		    (setf discard? t))) ; and discard because var-set is necessarily a subset of the newly created set
		 (t (push other-var-set new-remaining-var-sets)))))
	(filter-and-merge-var-sets new-remaining-var-sets
				   (if discard? result
				       (cons var-set result))))))

(defun merge-latent-variables (variables)
  """Latent-variables that occur in a latent-variable sets created by 
filter-and-merge-var-sets 
  (reduce #'merge-latent-variable variables))
	  
(defun merge-latent-variable (a b)
  

(defun select-generative-mvs-models (mvs models-slot)
  "Select the ppm models based on the current latent-variable-state from
 <models-slot> (currently either 'ltm or 'stm) of generative multiple
 viewpoint system <mvs> for each of its viewpoints."
  (let ((models (slot-value mvs models-slot))
	(viewpoints (mvs-viewpoints mvs)))
    (loop for viewpoint in viewpoints for viewpoint-models in models collecting
	 (let* ((variable (viewpoints:latent-variable viewpoint))
		(category (lv:get-latent-category variable)))
	   (cdr (assoc category viewpoint-models :test #'equal))))))

(defmethod mvs-ltm ((mvs generative-mvs))
  (select-generative-mvs-models mvs 'ltm))

(defmethod mvs-stm ((mvs generative-stm))
  (select-generative-mvs-models mvs 'ltm))

(defmethod set-mvs-parameters ((m generative-mvs) &rest key-args
			       &key &allow-other-keys)
  (map #'vector (lambda (mvs) (apply #'set-mvs-parameters (cons mvs key-args)))
       (mvs-models m)))

(defmethod get-short-term-model ((v viewpoint))
  (make-ppm (viewpoint-alphabet v)))

(defmethod get-short-term-model ((v abstract))
  (let* ((latent-variable (viewpoints:latent-variable v))
	 (categories (lv:categories latent-variable)))
    (loop for category in categories collecting
	 (lv:with-latent-state model
	   (make-ppm (viewpoint-alphabet v))))))

(defun get-short-term-models (viewpoints)
  "Returns a vector of freshly initialised ppm short term models
corresponding to the supplied list of viewpoints and initialised with
the supplied parameters."
  (apply #'vector (mapcar #'get-short-term-model
                          viewpoints)))

;;;========================================================================
;;; Model Construction and Prediction 
;;;========================================================================

(defmethod model-sequence ((m generative-mvs) sequence
			   &rest  key-args &key &allow-other-args)
  (let ((latent-variable (mvs-latent-variable))
	(latent-states (lv:latent-states latent-variable))
	(posteriors (list (lv:prior-distribution latent-variable)))
	(likelihood-distributions (loop for latent-state in latent-states collect
				       (lv:with-latent-variable-state
					   (latent-state latent-variable)
					 (apply #'model-sequence
						(append (list m sequence) key-args)))))
	(evidences))
    (loop for event-likelihood-distribution in likelihood-distributions collecting
	 (let ((evidence (marginal-likelihood event-likelihood-distribution
					      prior-distribution))
	       (prior-distribution (cdr posteriors)))
	   (push (infer-posterior-distribution evidence prior-distribution
					       event-likelihood-distribution)
		     posteriors)
	   (push evidence evidences)))
    (values evidences likelihood-distributions posteriors))))

(defmethod model-sequence ((m combined-mvs) sequence
			   &rest  key-args &key &allow-other-args)
  (let ((predictions (loop for model in (mvs-models m) collect
			  (apply #'model-sequence (append (list model sequence)
							  key-args)))))p
    (loop for position below (length sequence) collect
	 (let ((predictions (mapcar #'(lambda (p) (elt p position)) predictions)))
	   ;; For each basic viewpoint, collect predictions
	   ;; Combine them
	   ))))

;;;========================================================================
;;; Inference 
;;;========================================================================

(defun infer-posterior-distribution (evidence prior-distribution likelihood-distribution)
  (loop
     for likelihood in likelihood-distribution
     for prior in prior-distribution collecting
       (/ (* likelihood prior) evidence)))

(defun marginal-likelihood (prior-distribution likelihood-distribution)
  (apply #'+
	 (mapcar (lambda (prior likelihood) (* prior likelihood))
		 prior-distribution likelihood-distribution)))

(defun category-index (categories category latent-variable)
  (loop
     for i below (length categories)
     for cat in categories do
       (when (equal cat category)
	   (return i))))
       
(defun partition-dataset (dataset latent-variable &optional categories partitioned-dataset)
  (let ((composition (car dataset))
	(remaining-compositions (cdr dataset)))
    (multiple-value-bind (categories partitioned-dataset)
	(partition-composition categories partitioned-dataset
			     (coerce composition 'list) latent-variable)
      (if (null remaining-compositions)
	  (values categories partitioned-dataset)
	  (partition-dataset remaining-compositions latent-variable
			   categories partitioned-dataset)))))

;; Model and latent-variable specific
(defun partitioned-composition (categories partitioned-dataset composition latent-variable
			    &optional subsequence barlength pulses)
  "<categories> is a list of categories encountered so far. <partitioned-dataset> is
a list of lists of excertps associated with each category.
Both <categories> and <partitioned-dataset> may be empty. 
Split <composition> into continuous excerpts during which the category does 
not change. Return <categories>, updated with any new categories encountered, and
<partitioned-dataset> updated with the new excerpts inserted the appropriate place."
  (let* ((event (car composition))
	 (remaining-events (cdr composition))
	 (category (get-event-category event latent-variable)))
    (if (and (equal (get-event-category event)
		    category)
	     (not (null remaining-events)))
	(partition-composition categories partitioned-dataset remaining-events
			     (cons event subsequence) barlength pulses)
	(let* ((index (category-index categories category))
	       (categories (if (null index)
			       (cons category categories)
			       categories))
	       (partitioned-dataset (if (null index)
				      (cons (list (reverse subsequence)) partitioned-dataset)
				      (push (reverse subsequence)
					    (elt partitioned-dataset index)))))
	  (if (null remaining-events)
	      (values categories partitioned-dataset)
	      (partition-composition categories partitioned-dataset remaining-events
				   latent-variable))))))

(defgeneric prediction-set->likelihoods (prediction-set))

(defmethod prediction-set->likelihoods ((sequence-prediction sequence-prediction))
  (mapcar #'prediction-set->likelihoods (prediction-set sequence-prediction)))

(defmethod prediction-set->likelihoods ((dataset-prediction dataset-prediction))
  (mapcar #'prediction-set->likelihoods (prediction-set dataset-prediction)))

(defmethod prediction-set->likelihoods ((prediction-set event-prediction))
  (prediction-sets::distribution-probabilities
   (prediction-sets:event-predictions prediction-set)))

(defgeneric make-viewpoint-model (viewpoint training-set cache-params use-cache?))

(defmethod make-viewpoint-model ((viewpoint abstract-viewpoint) training-set
				 cache-params use-cache?)
  (let* ((latent-variable (viewpoints:latent-variable viewpoint))
	 (training-viewpoint (viewpoints:training-viewpoint abstract-viewpoint)))
    (multiple-value-bind (partition-dataset training-set latent-variable)
	(categories partitioned-training-set)
      (stf (lv:categories latent-variable) categories)
      (multiple-value-bind (latent-states prior-distribution)
	    (lv:initialise-prior-distribution categories
					      (mapcar #'length partitioned-training-set)
					      latent-variable)
	(setf (lv:prior-distribution latent-variable) prior-distribution)
	(setf (lv:latent-states latent-variable) latent-states))
      (loop for category in categories for training-set in partitioned-training-set collect
	   (cons category (make-viewpoint-model training-viewpoint training-set
						(append cache-params (list category))
						use-cache?))))))

(defmethod make-viewpoint-model ((viewpoint viewpoint) training-set
				 cache-params use-cache?)
  (if use-cache?
      #'(lambda (viewpoint)
	  (let ((filename (apply #'get-model-filename cache-params))
		(training-set
		 (viewpoint-sequences viewpoint training-set))
		(alphabet (viewpoint-alphabet viewpoint)))
	    (get-model filename alphabet training-set)))
      #'(lambda (viewpoint)
	  (let ((training-set
		 (viewpoint-sequences viewpoint training-set))
		(alphabet (viewpoint-alphabet viewpoint)))
	    (build-model training-set alphabet)))))
  

(defun get-long-term-models (viewpoints training-set pretraining-ids
                             training-id resampling-id
                             resampling-count 
                             voices texture
                             use-cache?)
  "Returns a vector of long-term models -- one for each viewpoint in
<viewpoints> -- trained on <training-set> and initialised with the
supplied keyword parameters. If <use-cache?> is T, models are written
to file, and reused on subsequent calls, otherise they are constructed
anew each time."
  (mapcar #'(lambda (vp) (make-viewpoint-model vp training-set pretraining-ids
					       training-id resampling-id
					       resampling-count 
					       voices texture
					       use-cache?))
	  viewpoints))

(defun get-model-filename (viewpoint pretraining-ids training-id resampling-id
                           resampling-count voices texture
			   &optional (category nil category-provided-p))
  "Returns the filename in *model-directory* containing the ppm model
for <viewpoint> in <dataset-id>."
  (string-append (namestring *model-dir*)
                 (viewpoint-name viewpoint)
                 (if (null pretraining-ids) "_NIL"
                     (string-append
                      "_"
                      (subseq (apply #'string-append 
                                     (mapcar #'(lambda (x) (format nil "-~A" x))
                                             pretraining-ids)) 1)))
                 (cond ((null training-id) "_NIL-")
                       ((consp training-id)
                        (format nil "_~{~A~^-~}" training-id))
                       (t (format nil "_~A" training-id)))
                 (cond ((and (null resampling-id) (null resampling-count))
                        "")
                       ((null resampling-count) 
                        (format nil "-~A" (if (numberp resampling-id)
                                              (1+ resampling-id)
                                              resampling-id)))
                       (t 
                        (format nil "-~A:~A" (if (numberp resampling-id)
                                              (1+ resampling-id)
                                              resampling-id)
                                resampling-count)))
		 (if (category-provided-p
		      (format nil "_~{~A~^-~}" category))
		     "")
                 (format nil "_~(~A~)" texture)
                 (format nil "~{-~A~}" voices)
                 ".ppm"))

