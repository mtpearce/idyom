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
  ((stm :initarg :stm :type hashtable)
   (ltm :initarg :ltm :type hashtable)
   (all-stm :initarg :all-stm :accessor %all-stm :type (vector ppm))
   (all-ltm :initarg :all-ltm :accessor %all-ltm :type (vector ppm))
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
  (when (not (lv:latent-category-set-p (mvs-latent-variable m)))
    (warn "Attempt to retrieve long-term model of abstract mvs while latent category is not set."))
  (let ((models (slot-value m 'ltm))
	(category (lv:get-latent-category (mvs-latent-variable m))))
    (gethash category models)))
  
(defmethod mvs-stm ((m abstract-mvs))
  (when (not (lv:latent-category-set-p (mvs-latent-variable m)))
    (warn "Attempt to retrieve long-term model of abstract mvs while latent category is not set."))
  (let ((models (slot-value m 'stm))
	(category (lv:get-latent-category (mvs-latent-variable m))))
    (gethash category models)))

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
  (let ((m (abstract-mvs m)))
    (let ((viewpoint-count (count-viewpoints m))
	  (model-index 0)
	  (ltm-models (%all-ltm m))
	  (stm-models (%all-stm m)))
      (dotimes (viewpoint-index viewpoint-count)
	(let* ((latent-variable (elt (viewpoint-latent-variables m) viewpoint-index))
	       (category-count (length (lv:categories latent-variable)))
	       (ltm-args (mapcar #'(lambda (x) (nth model-index x)) ltm-args))
	       (stm-args (mapcar #'(lambda (x) (nth model-index x)) stm-args)))
	  (unless (eql models 'stm)
	    (dotimes (i category-count)
	      (apply operation (cons (aref ltm-models (+ model-index i))
				     ltm-args))))
	  (unless (eql models 'ltm) 
	    (dotimes (i category-count)
	      (apply operation (cons (aref stm-models (+ model-index i))
				     stm-args))))
	  (setf model-index (+ model-index category-count)))))))


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
    (loop for latent-state in latent-states
       for i below (length latent-states) do
	 (lv:with-latent-variable-state
	     (latent-state latent-variable)
	   (let ((sequence-predictions
		  (apply #'model-sequence
			 (append (list (abstract-mvs m) events)
				 other-args))))
	     ;; Reset the short-term model for re-use in the next interpretation
	     (unless (= i (1- (length latent-states)))
	       (operate-on-models m #'reinitialise-ppm :models 'mvs::stm))
	     (loop for basic-viewpoint-index below (length (mvs-basic m)) do
		  (push (elt (mapcar #'prediction-set sequence-predictions)
			     basic-viewpoint-index)
			(elt predictions basic-viewpoint-index))))))
    (let* ((prediction-sets (mapcar #'reverse (mapcar #'transpose-lists predictions)))
	   (sequence-predictions))
      (loop for index below (length events) do
	   (let* ((events (subseq events 0 (1+ index)))
		  (event-predictions (mapcar (lambda (ps) (elt ps index))
					     prediction-sets))
		  (likelihoods (likelihoods (length latent-states)
					    event-predictions))
		  (prior-distribution (car posteriors))
		  (marginal-event-predictions (marginalize-event-predictions
					       prior-distribution events event-predictions
					       (mvs-basic m))))
	     (push marginal-event-predictions sequence-predictions)
	     (let ((evidence (marginal-likelihood likelihoods
						  prior-distribution)))
	       (push (infer-posterior-distribution evidence prior-distribution
						   likelihoods)
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
;;; Inference states
;;;========================================================================


(defclass marginal-event-prediction (event-prediction)
  ((inference-state :accessor prediction-inference-state :initarg :inference-state)))

(defclass inference-state ()
  ((viewpoints :accessor inference-state-viewpoints
	       :initarg :viewpoints)
   (distribution :accessor inference-state-prior-distribution
		       :initarg :prior-distribution)
   (latent-variable :accessor inference-state-latent-variable
		    :initarg :latent-variable)))

(defun next-inference-state (inference-state likelihoods)
  (let ((prior-distribution (inference-state-prior-distribution inference-state))
	(evidence (marginal-likelihood prior-distribution likelihoods))
	(posterior-distribution (infer-posterior-distribution evidence prior-distribution
							      likelihoods)))
  (make-instance 'inference-state
		 :viewpoints (inference-state-viewpoints inference-state)
		 :distribution posterior-distribution
		 :latent-variable (inference-state-latent-variable inference-state))))

(defun marginal-distribution (basic-viewpoint inference-state event-predictions)
  "Given all event predictions for the current basic feature and the prior distribution,
create a marginal event prediction."
  (when (string-equal (viewpoint-name basic-viewpoint) "onset")
    (viewpoints:set-onset-alphabet (butlast events)))
  (let ((distribution-symbols (viewpoint-alphabet basic-viewpoint))
	(distribution))
    (dolist (symbol distribution-symbols distribution)
	 (let ((likelihood 
		(marginal-likelihood prior
				     (mapcar (lambda (ep) (cadr (assoc symbol
								       (prediction-set ep)
								       :test #'equal)))
					     event-predictions))))
	   (push (list symbol likelihood) distribution)))))


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

(defun likelihoods (n-interpretations event-predictions)
  (let ((event-probabilities (mapcar #'event-predictions event-predictions)
  (loop for i below n-interpretations collect
       (apply #'* (mapcar (lambda (ep) (cadr (event-prediction (elt ep i))))
			  event-predictions))))
