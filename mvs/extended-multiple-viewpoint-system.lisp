(cl:in-package #:mvs)

;;;========================================================================
;;; Data Structures
;;;========================================================================

(defclass combined-mvs () 
  ((mvs-models :reader mvs-models
	       :initarg :mvs-models
	       :type (vector generative-mvs)))
  (:documentation "A combined multiple viewpoint system (MVS) combines 
one or more generative multiple viewpoint systems with a normal 
multiple viewpoint system."))

(defclass abstract-mvs (mvs)
  ((stm :initarg :stm :type hashtable)
   (ltm :initarg :ltm :type hashtable)
   (viewpoint-latent-variables :initarg :viewpoint-latent-variables
			       :accessor viewpoint-latent-variables
			       :type (vector latent-variable))
   (latent-variable :accessor mvs-latent-variable
		    :initarg :latent-variable))
  (:documentation "An abstract mvs pretends to be a normal mvs, but switches 
between predictive models based on categories encoded in the current latent 
variable state."))

(defclass generative-mvs ()
  ((abstract-mvs :initarg :mvs :reader abstract-mvs :type abstract-mvs))
  (:documentation "A generative-mvs is a wrapper for an abstract-mvs. 
Methods that need to be applied to various latent-state dependent models of 
an abstract mvs can specialize on the generative-mvs wrapper, iterate over 
latent states, and apply the method to the abstract-mvs."))

;;;========================================================================
;;; Model Initialisation 
;;;========================================================================

(defun get-predictive-system (basic-viewpoints viewpoints
			      basic-viewpoint-sets
			      viewpoint-sets latent-variable-sets
			      mvs-latent-variables ltms generative-ltms)
  (let* ((mvs-viewpoints (remove-if (lambda (v) (viewpoints:abstract? v))
				    viewpoints))
	 (mvs-basic-viewpoints (remove-if (lambda (b)
					    (not (find (type-of b) viewpoints
						       :key #'viewpoints:viewpoint-typeset
						       :test #'(lambda (x y) (member x y)))))
					  basic-viewpoints))
	 (generative-models (get-generative-mvs-models basic-viewpoint-sets
						       viewpoint-sets
						       latent-variable-sets
						       mvs-latent-variables
						       generative-ltms)))
    (cond ((and (null mvs-basic-viewpoints)
		(null basic-viewpoint-sets))
	   (warn "~&None of the supplied basic features can be predicted by any of the
supplied viewpoints.~%"))
	  ((null basic-viewpoints)
	   (make-instance 'combined-mvs :mvs-models generative-models
			  :basic basic-viewpoints))
	  ((null basic-viewpoint-sets)
	   (make-mvs mvs-basic-viewpoints mvs-viewpoints
		     ltms))
	  (t (make-instance 'combined-mvs
			    :mvs-models
			    (cons (make-mvs mvs-basic-viewpoints
					    mvs-viewpoints ltms)
				  generative-models)
			    :basic basic-viewpoints)))))

(defun get-generative-mvs-models (target-sets source-sets latent-variable-sets
				   mvs-latent-variables generative-ltms)
  (loop
     for targets in target-sets
     for sources in source-sets
     for latent-variables in latent-variable-sets
     for latent-variable in mvs-latent-variables
     for ltms in generative-ltms collect
       (progn
	 (make-instance 'generative-mvs
			:mvs (make-mvs targets sources ltms 
				       :class 'abstract-mvs
				       :latent-variable latent-variable
				       :viewpoint-latent-variables
				       latent-variables)))))

;;;========================================================================
;;; Methods for abstract and generative multiple viewpoint systems
;;;========================================================================

(defmethod mvs-ltm ((m abstract-mvs))
  (let ((latent-variable (mvs-latent-variable m))
	(viewpoint-latent-variables (viewpoint-latent-variables m)))
    (when (not (lv:latent-category-set-p latent-variable))
      (warn "Attempt to retrieve long-term model of abstract mvs while latent category is not set."))
    (let ((category (lv:get-latent-category (mvs-latent-variable m))))
      (map 'vector (lambda (link)
		     (gethash (cons (lv:latent-variable-attribute link)
				    (lv:get-link-category latent-variable category link))
			      (slot-value m 'ltm)))
	   viewpoint-latent-variables))))
  
(defmethod mvs-stm ((m abstract-mvs))
  (let ((latent-variable (mvs-latent-variable m))
	(viewpoint-latent-variables (viewpoint-latent-variables m)))
    (when (not (lv:latent-category-set-p latent-variable))
      (warn "Attempt to retrieve short-term model of abstract mvs while latent category is not set."))
    (let ((category (lv:get-latent-category (mvs-latent-variable m))))
      (map 'vector (lambda (link)
		     (gethash (cons (lv:latent-variable-attribute link)
				    (lv:get-link-category latent-variable category link))
			      (slot-value m 'stm)))
	   viewpoint-latent-variables))))

(defmethod mvs-basic ((m generative-mvs))
  (mvs-basic (abstract-mvs m)))

(defmethod viewpoint-latent-variables ((m generative-mvs))
  (viewpoint-latent-variables (abstract-mvs m)))

(defmethod mvs-latent-variable ((m generative-mvs))
  (mvs-latent-variable (abstract-mvs m)))

(defmethod get-short-term-model ((v viewpoints:abstract))
  (let ((latent-variable (viewpoints:latent-variable v)))
    (loop for category in (lv:categories latent-variable) collect
       (lv:with-latent-category (category latent-variable)
         (cons category (make-ppm (viewpoint-alphabet v)))))))

(defmethod set-mvs-parameters ((m abstract-mvs) &rest parameters)
  (let ((latent-variable (mvs-latent-variable m)))
    (loop for category in (lv:categories latent-variable) collect
         (lv:with-latent-category (category latent-variable)
           (apply #'set-mvs-parameters-function (cons m parameters))))))

(defmethod operate-on-models ((m generative-mvs) operation &key (models 'both) 
							     ltm-args stm-args)
  "The generative version of this method needs to take care that each operation is
only carried out once for each models. Iterating over the (joint) latent categories
of M does not guarantee this as one model may appear in multiple latent categories."
  (let* ((m (abstract-mvs m))
	 (viewpoint-count (count-viewpoints m)))
    (dotimes (model-index viewpoint-count)
      (let ((latent-variable (elt (viewpoint-latent-variables m) model-index))
	    (ltm-args (mapcar #'(lambda (x) (nth model-index x)) ltm-args))
	    (stm-args (mapcar #'(lambda (x) (nth model-index x)) stm-args)))
	(dolist (category (lv:categories latent-variable))
	  (let ((key (cons (lv:latent-variable-attribute latent-variable) category)))
	    (unless (eql models 'stm)
	      (let ((model (gethash key (slot-value m 'ltm))))
		(apply operation (cons model ltm-args))))
	    (unless (eql models 'ltm)
	      (let ((model (gethash key (slot-value m 'stm))))
		(apply operation (cons model stm-args))))))))))

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

(defmethod model-sequence-interpretations ((m generative-mvs) latent-states
					   latent-variable events other-args)
  (unless (null latent-states)
    (let ((latent-state (car latent-states)))
      (cons (lv:with-latent-variable-state (latent-state latent-variable)
	      (apply #'model-sequence
		     (append (list (abstract-mvs m) events)
			     other-args)))
	    (lv:with-latent-variable-state (latent-state latent-variable)
	      (unless (null (cdr latent-states))
		(operate-on-models m #'reinitialise-ppm :models 'mvs::stm))
	      (model-sequence-interpretations m (cdr latent-states) latent-variable
					      events other-args))))))

(defmethod get-event-predictions (sequence-interpretation-predictions event-index
				  (m generative-mvs))
  (let ((event-interpretation-predictions))
    (dotimes (bv-index (length (mvs-basic m)))
      (push (mapcar (lambda (sequence-interpretation-prediction)
		      (elt (prediction-set
			    (elt sequence-interpretation-prediction bv-index))
			   event-index))
		    sequence-interpretation-predictions)
	    event-interpretation-predictions))
    (reverse event-interpretation-predictions)))

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
;  (assert (not (member *models* '(:both+ :ltm+)))
;	  "LTM model cannot be updated when modelling inference.")
  (let* ((latent-variable (mvs-latent-variable m))
	 (prior-distribution (lv:prior-distribution latent-variable))
	 (latent-states (mapcar #'car prior-distribution))
	 (posteriors (list (mapcar #'cdr prior-distribution)))
	 (sequence-interpretation-predictions
	  (model-sequence-interpretations m latent-states latent-variable
					  events other-args))
	 (sequence-predictions))
    ;; Transform each list of interpretations (where each such list is a list of
    ;; event predictions in the corresponding interpretation) into a list of 
    ;; each element of which is a list of event predictions for one event 
    ;; containing predictions of the that event for each interpretation.
    ;; This simply means applying the transpose operation to the list of interpretations
    ;; (which can be seen as a matrix of interpretation by event position).
    (loop for event-index below (length events) do
	 (let* ((events (subseq events 0 (1+ event-index)))
		(event-interpretation-predictions
		 (get-event-predictions sequence-interpretation-predictions
					event-index m))
		(likelihoods (joint-interpretation-likelihoods
			      event-interpretation-predictions))
		(prior-distribution (car posteriors))
		(marginal-event-predictions (marginalize-event-predictions
					     prior-distribution events
					     event-interpretation-predictions
					     (mvs-basic m))))
	   (push marginal-event-predictions sequence-predictions)
	   (let ((evidence (marginal-likelihood likelihoods
						prior-distribution)))
	     (push (infer-posterior-distribution evidence prior-distribution
						 likelihoods)
		   posteriors))))
    (sequence-prediction-sets (abstract-mvs m)
			      events (reverse sequence-predictions))))

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

(defun infer-posterior-distribution (evidence prior-distribution likelihoods)
  (loop
     for likelihood in likelihoods
     for prior in prior-distribution collecting
       (/ (* likelihood prior) evidence)))

(defun marginal-likelihood (prior-distribution likelihoods)
  (apply #'+
	 (mapcar (lambda (prior likelihood) (* prior likelihood))
		 prior-distribution likelihoods)))

(defun marginalize-event-predictions (prior-distribution events event-predictions
				      basic-viewpoints)
  (mapcar #'(lambda (ep bv) (make-marginal-event-prediction prior-distribution
							    events ep bv))
	  event-predictions basic-viewpoints))

(defun transpose-lists (lists)
  (apply #'mapcar #'list lists))

(defun joint-interpretation-likelihoods (event-interpretation-predictions)
  "<event-interpretation-predictions> is a list whose elements are lists of
event predictions, one for each interpretation. Each list of event predictions 
corresponds to one basic viewpoint. Convert this structure to a list where each
item of the list is the product of the event likelihoods (obtained from the 
EVENT-PREDICTION-SET object using EVENT-PREDICTION) for each basic viewpoint and
a single interpretation."
  (mapcar (lambda (interpretation-predictions)
	    (apply #'* (mapcar #'cadr (mapcar #'event-prediction interpretation-predictions))))
	  (transpose-lists event-interpretation-predictions)))
