(cl:in-package #:mvs)

(defparameter *output-csv* nil "Turns on csv-formatted output")

;;;========================================================================
;;; Data Structures
;;;========================================================================

(defclass combined-mvs (mvs) 
  ((mvs-models :reader mvs-models
	       :initarg :mvs-models
	       :type (vector generative-mvs)))
  (:documentation "A combined multiple viewpoint system (MVS) combines 
one or more generative multiple viewpoint systems with a normal 
multiple viewpoint system."))

(defclass abstract-mvs (mvs)
  ((stm :initarg :stm :type hashtable)
   (ltm :initarg :ltm :type hashtable)
   (latent-variable :accessor latent-variable
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

(defun get-predictive-system (target-viewpoints viewpoints
			      generative-viewpoints latent-variables
			      ltms generative-ltms)
  (let* ((targets (remove-if (lambda (tv)
			       (not (find (viewpoint-typeset tv) viewpoints
					  :key #'viewpoint-typeset
					  :test #'(lambda (x y) (subsetp x y)))))
			     target-viewpoints))
	 (generative-models (get-generative-mvs-models target-viewpoints
						       generative-viewpoints
						       latent-variables generative-ltms)))
    (cond ((and (null targets)
		(null viewpoints)) ; is the case when only using generative systems
	   (make-instance 'combined-mvs :target target-viewpoints
			  :mvs-models generative-models))
	  ((null generative-models) ; is the case when only using sequential prediction
	   (make-mvs targets viewpoints
		     ltms))
	  (t (make-instance 'combined-mvs
			    :target target-viewpoints
			    :mvs-models
			    (cons (make-mvs targets
					    viewpoints ltms)
				  generative-models))))))

(defun get-generative-mvs-models (targets generative-viewpoints latent-variables
				  generative-ltms)
  (loop
     for sources in generative-viewpoints
     for latent-variable in latent-variables
     for ltms in generative-ltms collect
       (let ((targets (remove-if (lambda (tv)
				   (not (find (viewpoint-typeset tv) sources
					      :key #'viewpoint-typeset
					      :test #'(lambda (x y) (subsetp x y)))))
				 targets)))
	 (when (null targets)
	   (warn "~&No predictable targets available for generative system with latent variable
~A and viewpoint(s) ~{~A~^, ~}.~%"
		 (lv:latent-variable-name latent-variable)
		 (mapcar #'viewpoints:viewpoint-name sources)))
	 (make-instance 'generative-mvs
			:mvs (make-mvs targets sources ltms 
				       :class 'abstract-mvs
				       :latent-variable latent-variable)))))

;;;========================================================================
;;; Methods for abstract and generative multiple viewpoint systems
;;;========================================================================

(defmethod mvs-ltm ((m abstract-mvs))
  (let ((latent-variable (latent-variable m)))
    (when (not (lv:latent-category-set-p latent-variable))
      (warn "Attempt to retrieve long-term model of abstract mvs while latent category is not set."))
    (let ((category (lv:get-latent-category (latent-variable m))))
      (gethash category (slot-value m 'ltm)))))
  
(defmethod mvs-stm ((m abstract-mvs))
  (let ((latent-variable (latent-variable m)))
    (when (not (lv:latent-category-set-p latent-variable))
      (warn "Attempt to retrieve short-term model of abstract mvs while latent category is not set."))
    (let ((category (lv:get-latent-category (latent-variable m))))
      (gethash category (slot-value m 'stm)))))

(defmethod mvs-target ((m generative-mvs))
  (mvs-target (abstract-mvs m)))

(defmethod latent-variable ((m generative-mvs))
  (latent-variable (abstract-mvs m)))

(defmethod set-mvs-parameters ((m abstract-mvs) &rest parameters)
  (let ((latent-variable (latent-variable m)))
    (loop for category in (lv:categories latent-variable) collect
         (lv:with-latent-category (category latent-variable)
           (apply #'set-mvs-parameters-function (cons m parameters))))))

(defmethod operate-on-models ((m generative-mvs) operation &key (models 'both) 
							     ltm-args stm-args)
  "Calls a function <operation> which accepts a <ppm> object as it
sole argument on all the long- and short-term models of all categories in mvs <m>."
  (let* ((m (abstract-mvs m))
	 (latent-variable (latent-variable m))
	 (viewpoint-count (count-viewpoints m)))
    (dolist (category (lv:categories latent-variable))
      (let ((ltms (gethash category (slot-value m 'ltm)))
	    (stms (gethash category (slot-value m 'stm))))
	(dotimes (model-index viewpoint-count)
	  (let ((ltm (aref ltms model-index))
		(stm (aref stms model-index))
		(ltm-args (mapcar #'(lambda (x) (nth model-index x)) ltm-args))
		(stm-args (mapcar #'(lambda (x) (nth model-index x)) stm-args)))
	    (unless (eql models 'stm) (apply operation (cons ltm ltm-args)))
	    (unless (eql models 'ltm) (apply operation (cons stm stm-args)))))))))

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
  "Call model-sequence for each latent-variable-state in latent states of 
M's latent variable. Return one list of sequence predictions (one sequence prediction
for each target viewpoint) for each latent state."
  (unless (null latent-states)
    (let ((latent-state (car latent-states)))
      (cons (lv:with-latent-variable-state (latent-state latent-variable)
	      (apply #'model-sequence
		     (append (list (abstract-mvs m) events)
			     other-args)))
	    (progn 
	      (lv:with-latent-variable-state (latent-state latent-variable)
		(unless (null (cdr latent-states))
		  ;; Reset the STM PPMs unless this is the last latent state
		  (operate-on-models (abstract-mvs m) #'reinitialise-ppm :models 'mvs::stm)))
	      (model-sequence-interpretations m (cdr latent-states) latent-variable
					      events other-args))))))

(defmethod get-event-predictions (sequence-interpretation-predictions event-index
				  (m generative-mvs))
  (let ((event-interpretation-predictions))
    (dotimes (tv-index (length (mvs-target m)))
      (push (mapcar (lambda (sequence-interpretation-prediction)
		      (elt (prediction-set
			    (elt sequence-interpretation-prediction tv-index))
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
;;  (assert (not (member *models* '(:both+ :ltm+)))
  ;;	  "LTM model cannot be updated when modelling inference.")
  ;; A somewhat ad-hoc solution to predicting onset from periodic viewpoints whose
  ;; period depends on the latent state.
  (let* ((latent-variable (latent-variable m))
	 (prior-distribution (lv:prior-distribution latent-variable))
	 (latent-states (mapcar #'car prior-distribution))
	 (sequence-interpretation-predictions
	  (model-sequence-interpretations m latent-states latent-variable
					  events other-args))
	 (posteriors (list (mapcar #'cdr prior-distribution)))
	 (event-identifier (md:get-identifier (car events)))
	 (dataset-id (md:get-dataset-index event-identifier))
	 (composition-id (md:get-composition-index event-identifier))
	 (partition-id (md::get-partition-index event-identifier))
	 (sequence-predictions))
    (when *output-csv*
      (output-distribution dataset-id composition-id partition-id -1 latent-variable latent-states (car posteriors)))
    ;; Transform each list of interpretations (where each such list consists of
    ;; event predictions in the corresponding interpretation) into a list, each
    ;; element of which is a list of event predictions for one event 
    ;; containing predictions of the that event for each interpretation.
    ;; This corresponds to applying the transpose operation to the list of interpretations
    ;; (which can be seen as a matrix of interpretation by event position).
    (loop for event-index below (length events) do
	 (let ((events (subseq events 0 (1+ event-index))))
	   (set-target-alphabets m events t)
	   (let* ((event-id (md:get-event-index (md:get-identifier (elt events event-index))))
		  (event-interpretation-predictions
		   (get-event-predictions sequence-interpretation-predictions
					  event-index m))
		  (likelihoods (joint-interpretation-likelihoods
				event-interpretation-predictions))
		  (prior-distribution (car posteriors))
		  (marginal-event-predictions (marginalize-event-predictions
					       latent-variable
					       latent-states prior-distribution events
					       event-interpretation-predictions
					       (mvs-target m))))
	     (push marginal-event-predictions sequence-predictions)
	     (let ((evidence (marginal-likelihood likelihoods
						  prior-distribution)))
	       (push (infer-posterior-distribution evidence prior-distribution
						   likelihoods)
		     posteriors)
	       (when *output-csv*
		 (output-distribution dataset-id composition-id partition-id event-id
				      latent-variable latent-states (car posteriors)))))))
    (sequence-prediction-sets (abstract-mvs m)
			      events (reverse sequence-predictions))))
    
(defmethod model-sequence ((m combined-mvs) events
			   &rest other-args)
  (let* ((mvs-models (mvs-models m))
	 ;; Gather predictions using the prediction models
	 (mvs-predictions (apply #'append
				 (loop for model in mvs-models collect
				      (apply #'model-sequence
					     (append (list model events)
						     other-args)))))
	 (target-viewpoints (mvs-target m))
	 (prediction-sets))
    ;; Group predictions per target viewpoint
    (dolist (target-viewpoint target-viewpoints)
      (push (remove-if (lambda (ps) (not (eq (type-of
					      (prediction-viewpoint ps))
					     (type-of target-viewpoint))))
		       mvs-predictions)
	    prediction-sets))
    (let ((combined-predictions))
      (dotimes (index (length events))
	(let ((combined-target-predictions))
	  (dotimes (target-index (length target-viewpoints))
	    (let ((event-predictions (mapcar (lambda (sequence-prediction)
					       (elt (prediction-set sequence-prediction)
						    index))
					     (elt prediction-sets target-index))))
	      (push (combine-distributions event-predictions
					   *mvs-combination*
					   *mvs-bias*
					   :mvs)
		    combined-target-predictions)))
	  (push combined-target-predictions combined-predictions)))
      (sequence-prediction-sets m events (reverse combined-predictions)))))

;;;========================================================================
;;; Inference utility functions
;;;========================================================================

(defun output-distribution (did cid pid eid latent-variable latent-states distribution)
  (let* ((name (lv:latent-variable-name latent-variable))
	 (attributes (lv:latent-state-attributes latent-variable))
	 (columns (mapcar #'string-downcase (mapcar #'symbol-name attributes))))
    (loop for latent-state in latent-states
       for p in distribution do
	 (format t "~A,~A,~A,~A,\"lv\",\"~A\",\"~{~A~^,~}\",\"~{~A~^,~}\",~F~%"
		 did cid pid eid name columns latent-state p))))

(defun infer-posterior-distribution (evidence prior-distribution likelihoods)
  (loop
     for likelihood in likelihoods
     for prior in prior-distribution collecting
       (/ (* likelihood prior) evidence)))

(defun marginal-likelihood (prior-distribution likelihoods)
  (apply #'+
	 (mapcar (lambda (prior likelihood) (* prior likelihood))
		 prior-distribution likelihoods)))

(defun marginalize-event-predictions (latent-variable latent-states prior-distribution events
				      event-predictions target-viewpoints)
  "Create an event prediction for each target viewpoint given a prior distribution,
the list of events, a one list of event prediction sets per target viewpoint containing 
event prediction for each latent state."
  (mapcar #'(lambda (ep tv)
	      (make-marginal-event-prediction latent-variable latent-states
					      prior-distribution events ep tv))
	  event-predictions target-viewpoints))

(defun transpose-lists (lists)
  (apply #'mapcar #'list lists))

(defun joint-interpretation-likelihoods (event-interpretation-predictions)
  "<event-interpretation-predictions> is a list whose elements are lists of
event predictions, one for each interpretation. Each list of event predictions 
corresponds to a target viewpoint. Convert this structure to a list where each
item is the product of the event likelihoods for different target viewpoints of that
event (obtained from the EVENT-PREDICTION-SET object using EVENT-PREDICTION) for 
each target viewpoint and a single interpretation."
  (mapcar (lambda (interpretation-predictions)
	    (apply #'* (mapcar #'cadr (mapcar #'event-prediction interpretation-predictions))))
	  (transpose-lists event-interpretation-predictions)))

;;;========================================================================
;;; Model inspection
;;;========================================================================

(defmethod print-mvs ((m combined-mvs))
  (let ((models (mvs-models m)))
    (format t "Combined MVS~%")
    (loop for model in models do
	 (print-mvs model))))
(defmethod print-mvs ((m generative-mvs))
  (print-mvs (abstract-mvs m)))
(defmethod print-mvs ((m abstract-mvs))
  (format t "Generative MVS (~A): (~{~A~^, ~}) --> (~{~A~^, ~})~%"
	  (lv:latent-variable-name (latent-variable m))
	  (map 'list #'viewpoints:viewpoint-name (mvs-viewpoints m))
	  (map 'list #'viewpoints:viewpoint-name (mvs-target m)))
  (lv::print-latent-variable (latent-variable m)))
(defmethod print-mvs ((m mvs))
    (format t "MVS: (~{~A~^, ~}) --> (~{~A~^, ~})~%"
	  (map 'list #'viewpoints:viewpoint-name (mvs-viewpoints m))
	  (map 'list #'viewpoints:viewpoint-name (mvs-target m))))
