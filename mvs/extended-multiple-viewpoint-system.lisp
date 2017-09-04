(cl:in-package #:mvs)

;;;========================================================================
;;; Data Structures
;;;========================================================================

(defclass combined-mvs (mvs) 
  ((mvs-models :reader mvs-models
	       :initarg :mvs-models
	       :type (vector generative-mvs)))
  (:documentation "A combined multiple viewpoint system (MVS) consists 
is a meta class that combines one or more generative multiple viewpoint
systems and up to one normal multiple viewpoint system."))

(defclass abstract-mvs (mvs)
  ((stm :initarg :stm :type (vector list))
   (ltm :initarg :ltm :type (vector list))
   (viewpoint-latent-variables :initarg :viewpoint-latent-variables
			       :accessor viewpoint-latent-variables
			       :type (vector latent-variable))
   (latent-variable :accessor mvs-latent-variable
		    :initarg :latent-variable))
  (:documentation "An abstract mvs is a chameleon-like class that 
masquerades as a normal mvs, but uses models that depend on the current
latent category."))

(defclass generative-mvs ()
  ((abstract-mvs :initarg :mvs :reader abstract-mvs :type abstract-mvs))
  (:documentation "A generative-mvs is a wrapper for an abstract-mvs. 
Methods that need to be applied to various latent-state dependent models of
an abstract mvs can specialize on the generative-mvs wrapper, iterate over
 latent states, and apply the method to the abstract-mvs."))


;;;========================================================================
;;; Model Initialisation 
;;;========================================================================

(defun make-combined-mvs (basic-viewpoints mvs generative-mvs-models)
  (let ((mvs-models (if (null mvs) generative-mvs-models
			(cons mvs generative-mvs-models))))
    (make-instance 'combined-mvs :mvs-models mvs-models
		   :basic basic-viewpoints)))

(defun make-generative-mvs-models (target-sets source-sets latent-variable-sets
				   merged-latent-variables generative-ltms
				   prior-distributions)
  (loop
     for targets in target-sets
     for sources in source-sets
     for latent-variables in latent-variable-sets
     for latent-variable in merged-latent-variables
     for ltms in generative-ltms
     for prior-distribution in prior-distributions collect
       (progn
	 (setf (lv:prior-distribution latent-variable) prior-distribution)
	 (make-instance 'generative-mvs
			:mvs (make-mvs targets sources ltms 
				       :class 'abstract-mvs
				       :class-args
				       (list :latent-variable latent-variable
					     :viewpoint-latent-variables
					     (apply #'vector latent-variables)))))))

(defun select-generative-mvs-models (mvs models-slot)
  "Select the applicable ppm model based on the current latent-variable-state from
 <models-slot> (currently either 'ltm or 'stm) of generative multiple
 viewpoint system <mvs> for each of its viewpoints."
  (let ((models (slot-value mvs models-slot))
	(viewpoint-count (count-viewpoints mvs))
	(latent-variables (viewpoint-latent-variables mvs)))
    (loop for i below viewpoint-count collect
	 (let* ((viewpoint-models (aref models i))
		(variable (aref latent-variables i))
		(category (lv:get-latent-category variable)))
	   (cdr (assoc category viewpoint-models :test #'equal))))))

;;;========================================================================
;;; Methods for abstract and generative multiple viewpoint systems
;;;========================================================================

(defmethod mvs-ltm ((mvs abstract-mvs))
  (apply #'vector (select-generative-mvs-models mvs 'ltm)))

(defmethod mvs-stm ((mvs abstract-mvs))
  (apply #'vector (select-generative-mvs-models mvs 'stm)))

(defmethod mvs-basic ((m generative-mvs))
  (mvs-basic (abstract-mvs m)))

(defmethod viewpoint-latent-variables ((m generative-mvs))
  (viewpoint-latent-variables (abstract-mvs m)))

(defmethod mvs-latent-variable ((m generative-mvs))
  (mvs-latent-variable (abstract-mvs m)))

(defmethod get-short-term-model ((v viewpoints:abstract))
  (loop for category in (lv:categories (viewpoints:latent-variable v)) collect
       (cons category (make-ppm (viewpoint-alphabet v)))))

(defmethod set-mvs-parameters ((m abstract-mvs) &rest parameters)
  (let ((latent-variable (mvs-latent-variable m)))
    (loop for category in (lv:categories latent-variable) collect
	 (lv:with-latent-category (category latent-variable)
	   (apply #'set-mvs-parameters-function (cons m parameters))))))

(defmethod operate-on-models ((m generative-mvs) operation &key (models 'both) 
							     ltm-args stm-args)
  "Calls a function <operation> which accepts a <ppm> object as it
sole argument on all the long- and short-term models in mvs <m>."
  (let ((latent-variable (mvs-latent-variable m)))
    (dolist (category (lv:categories latent-variable))
      (lv:with-latent-category (category latent-variable)
	(operate-on-models (abstract-mvs m) operation :stm-args stm-args
			   :ltm-args ltm-args :models models)))))

(defmethod operate-on-models ((m combined-mvs) operation &key (models 'both) 
							     ltm-args stm-args)
  "Calls a function <operation> which accepts a <ppm> object as it
sole argument on all the long- and short-term models in mvs <m>."
  (dolist (mvs (mvs-models m))
	  (operate-on-models mvs operation :stm-args stm-args
			   :ltm-args ltm-args :models models)))

;;;========================================================================
;;; Model construction and prediction 
;;;========================================================================

(defmethod model-sequence ((m generative-mvs) events
			   &rest other-args)
  "Models a sequence <sequence> given the generative multiple-viewpoint system <m>.
This method iterates over the latent-variable states over which the prior distribution
in of the (combined) latent-variable of <m> is defined, sets the latent-variable 
state accordingly and calls model-sequence on the abstract-mvs embedded in <m> to
obtain sequence-predictions conditioned on the latent-variable states.
From the prior distribution and the sequence predictions per latent variable state
a posterior distribution over latent-variable states is inferred. A sequence of
event predictions is obtained by marginalizing event predictions out of the joint
distribution over events and latent variable states. 
A sequence-prediction is returned."
  (let* ((latent-variable (mvs-latent-variable m))
	 (prior-distribution (lv:prior-distribution latent-variable))
	 (latent-states (mapcar #'car prior-distribution))
	 (posteriors (list (mapcar #'cdr prior-distribution)))
	 (predictions (loop for i below (length (mvs-basic m)) collect nil)))
    (loop for latent-state in latent-states do
	 (lv:with-latent-variable-state
	     (latent-state latent-variable)
	   (let ((sequence-predictions
		  (apply #'model-sequence
			 (append (list (abstract-mvs m) events)
				 other-args))))
	     (loop for basic-viewpoint-index below (length (mvs-basic m)) do
		  (push (elt (mapcar #'prediction-set sequence-predictions)
			     basic-viewpoint-index)
			(elt predictions basic-viewpoint-index))))))
    (let* ((prediction-sets (mapcar #'transpose-predictions predictions))
	   (sequence-predictions))
      (loop for index below (length events) collect
	   (let* ((events (subseq events 0 (1+ index)))
		  (event-predictions (mapcar (lambda (ps) (elt ps index))
					     prediction-sets))
		  (likelihood-distribution (likelihood-distribution (length latent-states)
								    event-predictions))
		  (prior-distribution (car posteriors))
		  (marginal-event-predictions (marginalize-event-predictions
					       prior-distribution events event-predictions
					       (mvs-basic m))))
	     (push marginal-event-predictions sequence-predictions)
	     (let ((evidence (marginal-likelihood likelihood-distribution
						  prior-distribution)))
	       (push (infer-posterior-distribution evidence prior-distribution
						   likelihood-distribution)
		     posteriors))))
      (sequence-prediction-sets (abstract-mvs m)
				events (reverse sequence-predictions)))))

(defmethod model-sequence ((m combined-mvs) events
			   &rest other-args)
  (let* ((mvs-models (mvs-models m))
	 (mvs-predictions (apply #'append
				   (loop for model in mvs-models collect
					(apply #'model-sequence
					       (append (list model events)
						       other-args)))))
	 (basic-viewpoints (mvs-basic m))
	 (prediction-sets))
    (dolist (basic-viewpoint basic-viewpoints)
      (push (remove-if (lambda (ps) (not (eq (type-of
					      (prediction-viewpoint ps))
					     (type-of basic-viewpoint))))
		       mvs-predictions)
	    prediction-sets))
    (let ((combined-predictions))
      (dotimes (index (length events))
	(let ((combined-basic-predictions))
	  (dotimes (basic-index (length basic-viewpoints))
	    (let ((event-predictions (mapcar (lambda (sequence-prediction)
					       (elt (prediction-set sequence-prediction)
						    index))
					     (elt prediction-sets basic-index))))
	      (push (combine-distributions event-predictions
					   *mvs-combination*
					   *mvs-bias*
					   :mvs)
		    combined-basic-predictions)))
	  (push combined-basic-predictions combined-predictions)))
      (sequence-prediction-sets m events (reverse combined-predictions)))))

;;;========================================================================
;;; Inference utility functions
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

(defun marginalize-event-predictions (prior-distribution events event-predictions
				      basic-viewpoints)
  (mapcar #'(lambda (ep bv) (make-marginal-event-prediction prior-distribution
							    events ep bv))
	  event-predictions basic-viewpoints))

(defun transpose-predictions (predictions)
  "<predictions> is a list of lists representing the series of event likelihoods
under each interpretation. The output of this function is a list of lists where
the first list contains the likelihoods of the first event under each interpretation
the second list the likelihoods of the second event under each interpretation etc."
  (unless (null predictions)
    (unless (null (car predictions))
      (cons (mapcar #'car predictions)
	    (transpose-predictions (mapcar #'cdr predictions))))))

(defun likelihood-distribution (n-interpretations event-predictions)
  (loop for i below n-interpretations collect
       (apply #'* (mapcar (lambda (ep) (cadr (event-prediction (elt ep i))))
			  event-predictions))))


