(cl:in-package #:generative-models)

(defparameter *log-level* 0)
(defparameter *graph-cache-enabled* t)

(defun logm (msg args &optional level)
  (when (>= *log-level* level) (apply #'format (cons t (cons msg args))) (format t "~%")))

(defun infom (msg &rest args)
  (apply #'logm (list msg args 1)))

(defun debugm (msg &rest args)
  (apply #'logm (list msg args 2)))

(defmacro get-or-create-and-set (key hashtable default)
  "Default is not evaluated if KEY is found in HASHTABLE."
  `(multiple-value-bind (value hit?)
       (gethash ,key ,hashtable)
     (if (not hit?)
	 (setf (gethash ,key ,hashtable) ,default)
	 value)))

(defmacro defmemoizedmethod (name keys arguments &body body)
  "Defines a method called NAME whose first argument is called GRAPH and 
specialises on FEATURE-GRAPH and remaining arguments are ARGUMENTS.
Function results are stored in a cache associated with the graph instance.
KEYS is a list of forms which specify the parameters for which the method
returns unique results."
  `(defmethod ,name ((graph feature-graph) ,@arguments)
     (let ((cache (%method-cache graph)))
       (get-or-create-and-set (cons ',name (list ,@keys)) cache (progn ,@body)))))

(defun symbol-index-map (symbols &key (test #'eq))
  (let ((map (make-hash-table :test test)))
    (loop for s in symbols for i below (length symbols) do
	 (setf (gethash s map) i))
    map))

(defun dictionary (keys values)
  (let ((dict (make-hash-table)))
    (loop for key in keys for value in values do
	 (setf (gethash key dict) value))
    dict))

(defclass feature-graph (graphs:dag)
  ((features :initarg :features :reader features)
   (feature-table :initarg :feature-table :accessor feature-table)
   (feature-closures :initarg :feature-closures :accessor feature-closures)
   (traversal-table :initarg :traversal-table :accessor traversal-table)
   (horizontal-arguments :initform nil :accessor horizontal-arguments)
   (horizontal-depth :initform -1 :accessor horizontal-depth)
   (%method-cache :initform (make-hash-table :test #'equal) :accessor %method-cache)))

(defun make-feature-graph (&rest features)
  ;; TODO: check constraints.
  ;; * No loops in horizontal and vertical graphs
  ;; * Feature-ids must be unique by #'EQ
  ;; * Vertical args must be of horizontal depth <= horizontal-feature-depth
  ;; * Horizontal and vertical init args must be subset of horizontal and
  ;;   vertical args resp.
  ;; * Horizontal and vertical args must be members of features by #'EQ
  (let ((table (make-hash-table)))
    (dolist (f features) (setf (gethash (f:identifier f) table) f))
    (make-instance 'feature-graph
		   :feature-table table
		   :features features
		   :vertices nil
		   :order (length features))))

(defmemoizedmethod horizontal-feature-depth ((f:identifier feature)) ((feature f:normal))
  (let ((horizontal-graph
	 (make-instance 'graphs:edge-function-dag :order (graphs:order graph)
			:vertices (mapcar #'f:identifier (features graph))
			:edge-function
			(lambda (f) (f:horizontal-args (feature graph f))))))
    (graphs:depth horizontal-graph (f:identifier feature))))
	   
(defmethod feature ((graph feature-graph) feature)
  (gethash feature (feature-table graph)))

(defmethod set-horizontal-arguments ((graph feature-graph) arguments)
  (setf (horizontal-arguments graph) arguments)
  (let ((table (make-hash-table)))
    (setf (feature-closures graph) table)
    (dolist (f (active graph))
      (setf (gethash (f:identifier f) table) (feature-closure graph f)))))

(defmemoizedmethod max-horizontal-depth nil ()
  (apply #'max (mapcar (lambda (f) (horizontal-feature-depth graph f))
		       (features graph))))

(defmethod set-horizontal-depth ((graph feature-graph) depth)
  (setf (horizontal-depth graph)
	(min depth (1+ (max-horizontal-depth graph)))))

(defmethod next ((graph feature-graph))
  (when (< (horizontal-depth graph) (1+ (max-horizontal-depth graph)))
    (incf (horizontal-depth graph)))
  (setf (graphs:vertices graph) (mapcar #'f:identifier (active graph)))
  (setf (traversal-table graph) (make-hash-table :test #'equal))
  (traverse-graph graph (graphs:leafs graph)))

(defmethod reset ((graph feature-graph))
  (setf (horizontal-depth graph) -1))

(defmethod flush-cache ((graph feature-graph))
  (setf (%method-cache graph) (make-hash-table :test #'equal)))

(defmemoizedmethod active ((horizontal-depth graph)) ()
  (loop for f in (features graph)
     if (<= (horizontal-feature-depth graph f) (horizontal-depth graph))
     collect f))

(defmemoizedmethod modeled ((horizontal-depth graph)) ()
  (loop for f in (features graph) if (f:modeled? f) collect f))

(defmemoizedmethod observed ((horizontal-depth graph)) ()
  (loop for f in (features graph) if (f:observable? f) collect f))

(defmemoizedmethod representational ((horizontal-depth graph)) ()
  (loop for f in (features graph) if (f:representational? f) collect f))

(defmemoizedmethod graphs:edges ((horizontal-depth graph) feature) (feature)
  (let ((f (feature graph feature)))
    (if (typep f 'f:recursive)
	(if (eq (horizontal-depth graph)
		(horizontal-feature-depth graph f))
	    (f:vertical-init-args f)
	    (f:vertical-args f))
	(f:vertical-args f))))

(defmemoizedmethod horizontal-edges
    ((horizontal-depth graph) (f:identifier feature))
    ((feature f:recursive))
  (if (eq (horizontal-depth graph)
	  (horizontal-feature-depth graph feature))
      (f:horizontal-init-args feature)
      (cons (f:identifier feature) (call-next-method))))

(defmemoizedmethod horizontal-edges
    ((horizontal-depth graph) (f:identifier feature))
    ((feature f:normal))
  (f:horizontal-args feature))

(defmemoizedmethod marginal ((horizontal-depth graph)) ()
  (flet ((identifiers (features)
	   (mapcar #'f:identifier features)))
    (let ((depth (horizontal-depth graph)))
      (setf (horizontal-depth graph) (min (1+ depth)
					  (1+ (max-horizontal-depth graph))))
      (let ((argument-set
	     (reduce #'union (mapcar (lambda (f) (horizontal-edges graph f))
				     (active graph)))))
	(setf (horizontal-depth graph) depth)
	(reduce #'union
		(list argument-set
		      (identifiers (representational graph))
		      (identifiers (modeled graph))))))))

(defmethod observe ((graph feature-graph) event)
    (loop for f in (observed graph) collect
	 (f:observe f event)))

(defmethod model ((graph feature-graph) feature arguments)
  (let* ((instance (feature graph feature))
	 (model-accessor (f:model-accessor instance)))
    (funcall model-accessor arguments (eq (horizontal-depth graph)
					  (horizontal-feature-depth graph instance)))))

(defmethod feature-moment-function ((graph feature-graph) feature)
  (gethash feature (feature-closures graph)))

(defmemoizedmethod feature-function
    ((horizontal-depth graph) (f:identifier f))
    ((f f:recursive))
  (if (eq (horizontal-depth graph)
	  (horizontal-feature-depth graph f))
      (f:init-function f)
      (call-next-method)))

(defmemoizedmethod feature-function
    ((horizontal-depth graph) (f:identifier f))
    ((f f:normal))
  (f:f f))

(defmethod feature-closure ((graph feature-graph) (feature f:normal))
  (let* ((horizontal-args (mapcar (lambda (f) (gethash f (horizontal-arguments graph)))
				  (horizontal-edges graph feature)))
	 (function (feature-function graph feature))
	 (table (get-or-create-and-set
		 horizontal-args (f:table feature) (make-hash-table :test #'equal))))
    (lambda (&rest args)
      (get-or-create-and-set
       args table (apply (apply function horizontal-args) args)))))

(defmethod distribution ((model ppm:ppm) location alphabet)
  (ppm:set-alphabet model alphabet)
  (let ((distribution (ppm::get-distribution model location)))
    (loop for s in alphabet for p in distribution collect
	 (list s (probabilities:in (cadr p))))))

(defmethod traverse-graph ((graph feature-graph) features)
  (let* ((ancestor-set (apply #'graphs:ancestor-set (cons graph features)))
	 (bypass (loop for f in features if (member f ancestor-set) collect f))
	 (parent-set (union (apply #'graphs:edge-set (cons graph features)) bypass))
	 (feature-parents (mapcar (lambda (f) (graphs:edges graph f)) features))
	 (non-roots (loop for f in features for p in feature-parents
			 if (or (not (null p)) (member f bypass)) collect (list f p)))
	 (roots (remove-if (lambda (f) (member f non-roots :key #'car)) features)))
    (setf (gethash features (traversal-table graph))
	  (list parent-set bypass non-roots roots))
    (unless (null parent-set)
      (traverse-graph graph parent-set))))

(defmethod feature-value-distribution ((graph feature-graph) f arguments locations
				       &key predict?)
  (let* ((function (feature-moment-function graph f))
	 (feature (feature graph f))
	 (values (apply function arguments)))
    (unless (null values)
      (if (and (f:modeled? (feature graph f)) predict?)
	  (let ((model (model graph f arguments))
		(location (gethash f locations (ppm:get-root))))
	    (distribution model location values))
	  (let ((p (if (f:representational? feature)
		       1 (/ 1 (length values)))))
	    (loop for v in values collect (list v (probabilities:in p))))))))

(defmethod possible-states ((graph feature-graph) features locations
			    &key predict?)
  (multiple-value-bind (parent-set bypass non-roots roots)
      (values-list (gethash features (traversal-table graph)))
    (let* ((parent-map (symbol-index-map parent-set :test #'equal))
	   (feature-map (symbol-index-map features :test #'equal))
	   (parent-states
	    (if (null parent-set) '(())
		(possible-states graph parent-set locations :predict? predict?)))
	   (roots-states
	    (loop for f in roots collect
		 (feature-value-distribution graph f nil locations :predict? predict?)))
	   (roots-states (apply #'utils:cartesian-product roots-states))
	   (possible-states))
      (dolist (source parent-states possible-states)
	(let* ((elements (car source))
	       (distributions 
		(loop for (f parents) in non-roots collect
		     (if (member f bypass)
			 (list (list (car (svref elements (gethash f parent-map)))
				     (probabilities:in 1)))
			 (let* ((arguments
				 (mapcar (lambda (f)
					   (car (svref elements (gethash f parent-map))))
					 parents)))
			   (feature-value-distribution graph f arguments locations
						       :predict? predict?)))))
	       (non-roots-states (apply #'utils:cartesian-product distributions)))
	  (dolist (state (utils:cartesian-product roots-states non-roots-states))
	    (let* ((root-state (car state))
		   (non-root-state (cadr state))
		   (group-state (make-array (length features))))
	      (loop for item in non-roots for parameter in non-root-state do
		   (setf (svref group-state (gethash (car item) feature-map))
			 parameter))
	      (loop for f in roots for parameter in root-state do
		   (setf (svref group-state (gethash f feature-map))
			 parameter))
	      (push (list group-state source) possible-states))))))))

(defmethod traverse-state ((graph feature-graph) features state
			   &optional (probability (probabilities:in 1))
			     (elements-table (make-hash-table)))
  (multiple-value-bind (parent-set)
      (values-list (gethash features (traversal-table graph)))
    (let* ((distribution (car state))
	   (source (cadr state)))
      (dotimes (i (length features))
	(let* ((param (svref distribution i))
	       (value (car param))
	       (p (cadr param)))
	  (setf (gethash (elt features i) elements-table) value)
	  (setf probability (probabilities:mul probability p))))
      (if (null parent-set)
	  (values elements-table probability)
	  (traverse-state graph parent-set source probability elements-table)))))
    
(defmethod next-location ((m ppm:ppm) symbol location &key construct?)
  (ppm::add-event-to-model-dataset m symbol)
  (let* ((novel? (when construct? (unless (ppm::occurs? m location symbol) t)))
         (next-location (ppm::ukkstep m nil location symbol construct?)))
    (when construct? (ppm::increment-counts m next-location novel?))
    (ppm:increment-event-front m)
    next-location))

(defmethod next-locations ((graph feature-graph) locations elements &key construct?)
  "Update PPM locations with respective elements."
  (let ((new-locations (make-hash-table)))
    (dolist (f (modeled graph) new-locations)
      (let* ((identifier (f:identifier f))
	     (model (model graph identifier
			   (mapcar (lambda (a) (gethash a elements))
				   (graphs:edges graph identifier))))
	     (location (gethash identifier locations (ppm:get-root))))
	(setf (gethash identifier new-locations)
	      (next-location model (gethash identifier elements) location
				  :construct? construct?))))))

(defun update-marginals (observation-distribution marginals observation-params
			 marginal-params elements probability
			 &optional marginal-prefix)
  "Add joint state to a table whose parameters are TABLE-PARAMS. Each cell
contains a list of marginal states, and a hash table with states associated with each
marginal state. Each cell of that table contains a marginal state."
  (multiple-value-bind (observation-parameter)
      (marginals:update observation-distribution elements
			observation-params probability)
    (let ((marginal (gethash observation-parameter marginals
			     (marginals:make))))
      (multiple-value-bind (marginal-parameter)
	  (marginals:update marginal elements
			    marginal-params probability marginal-prefix)
	(setf (gethash observation-parameter marginals) marginal)
	(values observation-parameter marginal-parameter)))))

(defmethod apply-to-all-models ((graph feature-graph) fun &rest args)
  (dolist (f (features graph))
    (when (f:modeled? f)
      (dolist (m (f:models f))
	(apply fun m args)))))
	  
(defmethod model-dataset ((graph feature-graph)
			  dataset &key construct? predict? initialise?)
  (labels ((model-d (dataset sequence-index results)
	     (if (null dataset) (reverse results)
		 (let ((result
			(model-sequence graph (car dataset)
					:construct? construct? :predict? predict?)))
		   (unless (= sequence-index 1)
		     ;; (models:next-sequence)
		     (apply-to-all-models graph #'ppm:increment-sequence-front))
		   (when initialise?
		     ;; (models:initialise)
		     (apply-to-all-models graph #'ppm:reinitialise-ppm))
		   (model-d (cdr dataset)
			    (1- sequence-index)
			    (cons result results))))))
    (model-d dataset (length dataset) '())))

(defmethod model-sequence ((graph feature-graph)
			   (events md:melodic-sequence)
			   &key construct? predict?)
  (model-sequence graph (coerce events 'list)
		  :construct? construct? :predict? predict?))

(defmethod model-sequence ((graph feature-graph) events &key construct? predict?)
  (labels ((model-seq (events plausible-states results)
	     (next graph)
	     (multiple-value-bind (plausible-states evidence)
		 (model-event graph (car events)
			      :plausible-states plausible-states
			      :construct? construct? :predict? predict?)
	       ;; (models:next-event)
	       (if (null (cdr events))
		   (progn
		     (when construct?
		       (dolist (plausible-state plausible-states)
			 (let ((elements (car plausible-state))
			       (locations (fifth plausible-state)))
			   (dolist (f (modeled graph))
			     (let* ((id (f:identifier f))
				    (args (mapcar (lambda (f) (gethash f elements))
						  (graphs:edges graph id)))
				    (model (model graph id args))
				    (location (gethash id locations)))
			       (ppm:model-sentinel-event model location))))))
		     ;; (models:prepare-next-sequence)
		     (apply-to-all-models graph #'ppm:initialise-virtual-nodes)
		     (values plausible-states evidence))
		   (model-seq (cdr events) plausible-states (cons evidence results))))))
    (reset graph)
    (model-seq events (list (root-state)) nil)))

;; Branch, filter/prune implausible/select plausible, marginalize
(defmethod model-event ((graph feature-graph) event
			&key plausible-states predict? construct?)
  (let* ((observation (observe graph event))
	 (observation-distribution (marginals:make))
	 (marginals-table (make-hash-table :test #'equal))
	 (locations-table (make-hash-table :test #'equal))
	 (source-table (make-hash-table :test #'equal))
	 (leafs (graphs:leafs graph))
	 (unique-model-state 0))
    (flet ((model-param (marginal-param)
	     (mapcar (lambda (f)
		       (elt marginal-param (position (f:identifier f) (marginal graph))))
		     (modeled graph))))
      (dolist (state plausible-states)
	(multiple-value-bind (source-elements backtrace model-id probability locations)
	    (values-list state)
	  (set-horizontal-arguments graph source-elements)
	  (dolist (state (possible-states graph leafs locations :predict? predict?))
	    (multiple-value-bind (elements probability)
		(traverse-state graph leafs state probability)
	      (debugm "P: ~A. E: ~A~%" probability elements)
	      (multiple-value-bind (obs-param marginal-param)
		  (update-marginals observation-distribution marginals-table
				    (mapcar #'f:identifier (observed graph))
				    (marginal graph) elements probability (list model-id))
		(let* ((source-param (append obs-param marginal-param))
		       (location-param (cons model-id (model-param (cdr marginal-param)))))
		  (setf (gethash source-param source-table)
			(cons (list source-elements backtrace)
			      (gethash source-param source-table)))
		  (when (and (null (gethash location-param locations-table))
			     (equal obs-param observation))
		    (setf (gethash location-param locations-table)
			  (list unique-model-state
				(next-locations graph locations
						elements :construct? construct?)))
		    (incf unique-model-state))))))))
      (let* ((marginal (gethash observation marginals-table))
	     (plausible-states (marginals:parameters marginal))
	     (evidence (marginals:probability observation-distribution observation))
	     (hidden-states
	      (loop for marginal-parameter in plausible-states collect
		   (let* ((location-parameter
			   (cons (car marginal-parameter)
				 (model-param (cdr marginal-parameter))))
			  (source-parameter (append observation marginal-parameter))
			  (sources (gethash source-parameter source-table))
			  (probability (marginals:probability
					marginal marginal-parameter)))
		     (multiple-value-bind (model-id locations)
			 (values-list (gethash location-parameter locations-table))
		       (list (dictionary (marginal graph)
					 (cdr marginal-parameter))
			     sources model-id probability locations))))))
	(when (null marginal)
	  (warn "Model cannot generate observation ~A" observation))
	(values hidden-states evidence)))))

(defun root-state ()
  (multiple-value-bind (model-id elements probability locations sources)
      (values 0 nil (probabilities:in 1) (make-hash-table) nil)
    (list elements sources model-id probability locations)))

(defun trace-back (trace &rest features)
    (let ((elements (car trace))
	  (sources (cadr trace)))
    (when (> (length sources) 1)
      (warn "Traceback: ignoring branching trace (TODO: deal with branching traces)"))
    (unless (null sources)
      (cons (mapcar (lambda (f) (gethash f elements)) features)
	    (apply #'trace-back (cons (first sources) features))))))

;; To not ignore a branching trace:
;;	      (mapcar (lambda (s)
;;			(apply #'trace-back s features))
;;		      sources)))))



















