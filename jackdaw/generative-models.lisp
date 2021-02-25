(cl:in-package #:generative-models)

(defparameter *graph-cache-enabled* t)

;; Alternatively and possibly faster: define a FUNCTION with NAME and
;; a METHOD with %NAME. The function checks whether a cached results
;; exists (needs to add types of arguments to key as well?), otherwise
;; calls method.

(defmacro defmemoisedmethod (name keys arguments &body body)
  "Defines a method called NAME whose first argument is called GRAPH and 
specialises on FEATURE-GRAPH and remaining arguments are ARGUMENTS.
Function results are stored in a cache associated with the graph instance.
KEYS is a list of forms which specify the parameters for which the method
returns unique results."
  `(defmethod ,name ((graph feature-graph) ,@arguments)
     (if *graph-cache-enabled*
	 (let ((cache (%method-cache graph)))
	   (get-or-create-and-set (cons ',name (list ,@keys)) cache (progn ,@body)))
	 (progn ,@body))))

(defclass feature-graph (graphs:dag)
  ((features :accessor features)
   ;; If set to true, a state is plausible if it is a *member* of the generated
   ;; observations. If false, a state is plausible only if it is equal to the
   ;; generated observation.
   ;; There is no initarg because this field is an object property, not a parameter.
   (multiple-observations? :reader multiple-observations? :initform nil)
   ;; Feature function memoisation
   (feature-table :accessor feature-table :initform (make-hash-table))
   (current-observation :accessor current-observation)
   ;; State cache
   (feature-closures :accessor feature-closures)
   (observation-closures :accessor observation-closures)
   ;; Horizontal state
   (probability :accessor probability :initform (probs:in 1))
   (branch-ids :accessor branch-ids)
   (active-branch-ids :accessor active-branch-ids)
   (horizontal-arguments :initform nil :accessor horizontal-arguments)
   (horizontal-depth :initform 0 :accessor horizontal-depth)
   ;; General purpose memoisation cache
   (%method-cache :initform (make-hash-table :test #'equal) :accessor %method-cache)))

(defclass generative-model (feature-graph)
  ((latent :accessor latent)))

(defmethod initialize-instance :after ((graph feature-graph) &key)
  (let* ((features (create-features graph))
	 (vertices (f:identifiers features)))
    (setf (features graph) features)
    (setf (graphs:vertices graph) vertices)
    (when (not (eq (length (remove-duplicates vertices)) (length vertices)))
      (error "Feature identifiers must be unique."))
    (dolist (f features)
      (when (not (every (lambda (f) (member f vertices))
			(f:all-arguments f)))
	(error "Some arguments of ~A not found among graph features."
	       (f:identifier f))))
    (dolist (f features)
      (setf (gethash (f:identifier f) (feature-table graph)) f))
    (handler-case (graphs:topological-sort graph)
      (graphs::dag-contains-cycle nil (error "Feature graph must be acyclic.")))
    (dolist (f features)
      (check-argument-constraints graph f))))

(defun trace-back (graph state model-locations &rest features)
  (let* ((branch-ids (car state))
	 (elements (cadr state))
	 (modeled (f:identifiers (modeled graph :active? nil)))
	 (features (if (null features) modeled
		      (intersection features modeled))))
    (flet ((find-trace (v)
	     (when (null features)
	       (error
		"No traces found for some provided features because they are not modeled."))
	     (let* ((branch-id (gethash v branch-ids))
		    (feature (feature graph v))
		    (arguments (vertical-arguments graph v elements))
		    (model-args (model-conditioning graph feature arguments)))
	       (multiple-value-bind (trace-location found?)
		   (gethash (list v branch-id model-args) model-locations)
		 (when (not found?)
		   (warn "Feature ~A not found in marginalised state." v))
		 (car trace-location)))))
      (let ((traces (remove-if #'null (mapcar #'find-trace features))))
	(unless (null traces)
	  (reverse (apply #'mapcar #'list traces)))))))

(defgeneric check-argument-constraints (graph feature))
(defgeneric horizontal-feature-depth (graph feature))
(defgeneric feature (graph feature))
(defgeneric set-horizontal-state (graph state))
(defgeneric max-horizontal-depth (graph))
(defgeneric next (graph))
(defgeneric reset (graph))
(defgeneric flush-cache (graph))
(defgeneric active (graph &optional horizontal-depth)
  (:documentation "Memoiseable on (HORIZONTAL-DEPTH GRAPH). See DEFMEMOISEDMETHOD."))
(defgeneric modeled (graph &key active?)
  (:documentation "When memoisation is enabled, this method is cached 
for each unique horizontal-depth of GRAPH."))
(defgeneric observed (graph &key active?)
  (:documentation "When memoisation is enabled, this method is cached 
for each unique horizontal-depth of GRAPH."))
(defgeneric horizontal-edges (graph feature &optional horizontal-depth)
  (:documentation "When memoisation is enabled, this method is cached 
for each unique IDENTIFIER of FEATURE and horizontal-depth of GRAPH."))
(defgeneric marginal (graph)
  (:documentation "When memoisation is enabled, this method is cached 
for each unique horizontal-depth of GRAPH."))
(defgeneric plausible? (graph elements event))
(defgeneric model (graph feature arguments))
(defgeneric location (graph feature model arguments model-locations))
(defgeneric update-location (graph feature symbol args model-locations
			     new-model-locations new-branch-id
			     &key construct?))
(defgeneric feature-function (graph feature &key type)
  (:documentation "When memoisation is enabled, this method is cached 
for each unique IDENTIFIER of FEATURE and horizontal-depth of GRAPH."))
(defgeneric feature-closure (graph feature))
(defgeneric observation-closure (graph feature))
(defgeneric possible-values-distribution (graph feature arguments locations
					  &key predict?))
(defgeneric possible-states (graph locations &key predict?))
(defgeneric generative-sets (graph features))
(defgeneric traverse-state (graph features state
			    &optional probability elements-table))
(defgeneric model-dataset (graph dataset &key construct? predict? &allow-other-keys))
(defgeneric model-sequence (graph events &key construct? predict? &allow-other-keys))
(defgeneric model-event (graph event &key predict? construct? &allow-other-keys))
(defgeneric root-state (graph))
(defgeneric create-features (model)
  (:documentation "Methods should set LATENT and return the model's feature set"))
(defgeneric parameterise (model dataset &key writers))
(defgeneric set-domains (model dataset))
(defgeneric test (model dataset &key writers))

(defmethod set-domains ((model feature-graph) dataset))
	   
(defmethod feature ((graph feature-graph) feature)
  (gethash feature (feature-table graph)))

(defmethod parameterise ((model generative-model) dataset &key writers)
  (apply #'f:observe (observable model))
  (flush-cache model)
  (model-dataset model dataset :construct? t :writers writers))

(defmethod test ((model generative-model) dataset &key writers)
  (apply #'f:hide (latent model))
  (gm:flush-cache model)
  (gm:model-dataset model dataset :predict? t :writers writers))

(defmethod generate-labels ((model feature-graph) dataset &key writers)
  (apply #'f:observe (observable model))
  (gm::flush-cache model)
  (gm::model-dataset model dataset :writers writers))

(defmethod check-argument-constraints ((graph feature-graph) (f f:normal))
  (let ((vertical-args (f:vertical-args f))
	(horizontal-args (f:horizontal-args f))
	(depth (horizontal-feature-depth graph f)))
    (when (not (and (every (lambda (f)
			     (member f (active graph depth) :key #'f:identifier))
			   vertical-args)
		    (every (lambda (f)
			     (member f (active graph (1- depth)) :key #'f:identifier))
			   horizontal-args)))
      (error "Some arguments of ~A are not active at its horizontal depth (~A)."
	     (f:identifier f) depth))))

(defmethod check-argument-constraints ((graph feature-graph) (f f:recursive))
  (let ((vertical-init-args (f:vertical-init-args f))
	(horizontal-init-args (f:horizontal-init-args f))
	(vertical-args (f:vertical-args f))
	(horizontal-args (f:horizontal-args f))
	(depth (horizontal-feature-depth graph f)))
    (when (not (and (every (lambda (f)
			     (member f (active graph (1+ depth)) :key #'f:identifier))
			   vertical-args)
		    (every (lambda (f)
			     (member f (active graph depth) :key #'f:identifier))
			   horizontal-args)))
      (error "Some initialisation arguments of ~A are not active at its horizontal depth (~A)."
	     (f:identifier f) depth))
    (when (not (and (every (lambda (f)
			     (member f (active graph depth) :key #'f:identifier))
			   vertical-init-args)
		    (every (lambda (f)
			     (member f (active graph (1- depth)) :key #'f:identifier))
			   horizontal-init-args)))
      (error "Some arguments of ~A are not active at its horizontal depth (~A)."
	     (f:identifier f) depth))))

(defmemoisedmethod horizontal-feature-depth ((f:identifier feature)) ((feature f:feature))
  (let ((horizontal-graph
	 (make-instance 'graphs:edge-function-dag
			:vertices (f:identifiers (features graph))
			:edge-function
			(lambda (f)
			  (f:horizontal-args (feature graph f))))))
    (graphs:depth horizontal-graph (f:identifier feature))))

(defmethod set-horizontal-state ((graph feature-graph) state)
  (multiple-value-bind (branch-ids elements probability) (values-list state)
    (setf (branch-ids graph) branch-ids)
    (setf (active-branch-ids graph)
	  (mapcar (lambda (f) (gethash f branch-ids))
		  (f:identifiers (modeled graph))))
    (setf (horizontal-arguments graph) elements)
    (setf (probability graph) probability))
  (let ((fc (make-hash-table))
	(oc (make-hash-table)))
    (setf (feature-closures graph) fc)
    (setf (observation-closures graph) oc)
    (dolist (f (observed graph))
      (setf (gethash (f:identifier f) oc) (observation-closure graph f)))
    (dolist (f (active graph))
      (setf (gethash (f:identifier f) fc) (feature-closure graph f)))))

(defmemoisedmethod max-horizontal-depth nil ()
  (apply #'max (mapcar (lambda (f) (horizontal-feature-depth graph f))
		       (features graph))))

(defmethod next ((graph feature-graph)) 
  (when (< (horizontal-depth graph) (1+ (max-horizontal-depth graph)))
    (incf (horizontal-depth graph)))
  (setf (graphs:vertices graph) (f:identifiers (active graph))))

(defmethod reset ((graph feature-graph))
  (setf (horizontal-depth graph) 0))

(defmethod flush-cache ((graph feature-graph))
  (setf (%method-cache graph) (make-hash-table :test #'equal)))

(defmemoisedmethod active (horizontal-depth)
    (&optional (horizontal-depth (horizontal-depth graph)))
  (let ((active
	 (loop for f in (features graph)
	    if (<= (horizontal-feature-depth graph f) horizontal-depth)
	    collect f)))
    (debugm "Active at ~A: ~A." horizontal-depth (f:identifiers active))
    active))

(defmemoisedmethod modeled ((horizontal-depth graph) active?) (&key (active? t))
  (remove-if-not #'f:modeled? (if active? (active graph) (features graph))))

(defmemoisedmethod observable ((horizontal-depth graph) active?) (&key (active? t))
  (remove-if-not #'f:observable? (if active? (active graph) (features graph))))

(defmemoisedmethod observed ((horizontal-depth graph) active?) (&key (active? t))
  (remove-if-not #'f:observed? (if active? (active graph) (features graph))))

(defmemoisedmethod graphs:edges ((horizontal-depth graph) feature) (feature)
  (let ((f (feature graph feature)))
    (if (typep f 'f:recursive)
	(if (eq (horizontal-depth graph)
		(horizontal-feature-depth graph f))
	    (f:vertical-init-args f)
	    (f:vertical-args f))
	(f:vertical-args f))))

(defmemoisedmethod horizontal-edges
    (horizontal-depth (f:identifier feature))
    ((feature f:recursive) &optional (horizontal-depth (horizontal-depth graph)))
  (if (eq horizontal-depth
	  (horizontal-feature-depth graph feature))
      (f:horizontal-init-args feature)
      (cons (f:identifier feature) (f:horizontal-args feature))))

(defmemoisedmethod horizontal-edges
    (horizontal-depth (f:identifier feature))
    ((feature f:normal) &optional (horizontal-depth (horizontal-depth graph)))
  (f:horizontal-args feature))

(defmemoisedmethod marginal ((horizontal-depth graph)) ()
  (let* ((depth (horizontal-depth graph))
	 (next (min (1+ depth) (1+ (max-horizontal-depth graph))))
	 (modeled (f:identifiers (modeled graph)))
	 (observed (f:identifiers (observed graph)))
	 (observed-argument-vertices
	  (mapcar (lambda (v) (graphs:edges graph v)) observed))
	 (first-order-argument-vertices
	  (mapcar (lambda (f) (horizontal-edges graph f next)) (active graph next)))
	 (vertices
	  (reduce #'union (append (list observed modeled)
				  observed-argument-vertices
				  first-order-argument-vertices))))
    (mapcar (lambda (f) (feature graph f)) vertices)))

(defmemoisedmethod first-occurrence? ((horizontal-depth graph) (f:identifier feature))
    (feature)
  (eq (horizontal-depth graph)
      (horizontal-feature-depth graph feature)))

(defmethod model-conditioning ((graph feature-graph) feature args)
  "Given a set of vertical arguments to a feature, return the subset
of values that the feature's model is conditioned on."
  (funcall (f:model-arguments-accessor feature) args
	   (eq (horizontal-depth graph)
	       (horizontal-feature-depth graph feature))))

(defmethod vertical-arguments ((graph feature-graph) v elements)
  "Fetch the vertical arguments of V from elements."
  (mapcar (lambda (a) (gethash a elements)) (graphs:edges graph v)))

(defmethod model ((graph feature-graph) v elements)
  (let* ((feature (feature graph v))
	 (arguments (vertical-arguments graph v elements))
	 (conditioning (model-conditioning graph feature arguments)))
    (f:get-model feature conditioning (first-occurrence? graph feature))))

(defmethod location ((graph feature-graph) v model elements model-locations)
  "Fetch the location of the context of V from the current horizontal state."
  (let* ((branch-id (gethash v (branch-ids graph)))
	 (feature (feature graph v))
	 (arguments (vertical-arguments graph v elements))
	 (conditioning (model-conditioning graph feature arguments)))
    (multiple-value-bind (location found?)
	(gethash (list v branch-id conditioning) model-locations)
      (if found? location
	  (list () (models:root-location model))))))

(defmethod update-location ((graph feature-graph) v symbol elements
			    model-locations new-model-locations new-branch-id
			    &key construct?)
  (let* ((args (vertical-arguments graph v elements))
	 (model (model graph v elements))
	 (feature (feature graph v))
	 (conditioning (model-conditioning graph feature args))
	 (trace-location (location graph v model elements model-locations))
	 (next-location (models:next-location model symbol (cadr trace-location)
					      :construct? construct?)))
    ;;(format t "~a (~a ~a): updating ~a with ~a~%"
    ;;v (gethash v (branch-ids graph)) conditioning (car trace-location) symbol)
    (setf (gethash (list v new-branch-id conditioning) new-model-locations)
	  (list (cons symbol (car trace-location)) next-location))))

(defmemoisedmethod root-state () ()
  (multiple-value-bind (branch-ids elements probability)
      (values (make-hash-table) (make-hash-table) (probs:in 1))
    (list branch-ids elements probability)))

(defmemoisedmethod feature-function
    ((horizontal-depth graph) (f:identifier f) type)
    ((f f:recursive) &key (type :generative))
  (if (eq (horizontal-depth graph)
	  (horizontal-feature-depth graph f))
      (case type
	(:generative (f:init-function f))
	(:observation (f:init-observation-function f)))
      (call-next-method)))

(defmemoisedmethod feature-function
    ((horizontal-depth graph) (f:identifier f) type)
    ((f f:normal) &key (type :generative))
  (case type
    (:generative (f:f f))
    (:observation (f:observation-function f))))

(defmethod observation-closure ((graph feature-graph) (feature f:feature))
  (let* ((horizontal-args (mapcar (lambda (f) (gethash f (horizontal-arguments graph)))
				  (horizontal-edges graph feature)))
	 (function (feature-function graph feature :type :observation)))
    (when (null function)
      (error "Observation function missing for observed feature ~A"
	     (f:identifier feature)))
    (lambda (event &rest args)
      (apply (apply function horizontal-args) (cons event args)))))

(defmethod feature-closure ((graph feature-graph) (feature f:feature))
  (let* ((horizontal-args (mapcar (lambda (f) (gethash f (horizontal-arguments graph)))
				  (horizontal-edges graph feature)))
	 (function (feature-function graph feature))
	 (table (get-or-create-and-set
		 horizontal-args (f:table feature) (make-hash-table :test #'equal))))
    (lambda (&rest args)
      (get-or-create-and-set
       args table (apply (apply function horizontal-args) args)))))

(defmethod plausible? ((graph feature-graph) elements event)
  (%plausible? graph elements event (observed graph)))

(defmethod correct? ((graph feature-graph) elements event)
  "A state encoded by ELEMENTS is 'correct' if it is plausible given
the observation of *all* observable variables (that is, including 
latent variables). This allows checking if a state is congruent with
labels in a supervised scenario."
  (%plausible? graph elements event (observable graph)))

(defmethod %plausible? ((graph feature-graph) elements event observed)
  (debugm "active features: ~a" (f:identifiers (active graph)))
  (debugm "generated values: ~a" (element-list graph elements))
  (let* ((observed (f:identifiers observed))
	 (generated (mapcar (lambda (v) (gethash v elements)) observed))
	 (observation 
	  (loop for v in observed collect 
	       (let* ((function (gethash v (observation-closures graph)))
		      (arguments (vertical-arguments graph v elements)))
		 (apply function (cons event arguments))))))
    (debugm "observed features: ~a" observed)
    (debugm "observation: ~a" observation)
    (if (multiple-observations? graph)
	(every (lambda (element plausible-set)
		 (member element plausible-set :test #'equal))
	       generated observation)
	(equal generated observation))))

(defmethod possible-values-distribution ((graph feature-graph) v elements model-locations
					 &key predict?)
  (let* ((function (gethash v (feature-closures graph)))
	 (feature (feature graph v))
	 (arguments (vertical-arguments graph v elements))
	 (values (apply function arguments)))
    (unless (null values)
      (if (and (f:modeled? feature) predict?)
	  (let* ((model (model graph v elements))
		 (location (cadr (location graph v model elements model-locations)))
		 (distribution (models:distribution model location values)))
	    distribution)
	  (let ((p (/ 1 (length values))))
	    (loop for v in values collect (list v (probs:in p))))))))

(defmethod generate-states ((graph feature-graph) vertices model-locations
			    &key predict? 
			      (initial-states (list (list (make-hash-table)
							  (probability graph)))))
  (let* ((states
	  (if (null (cdr vertices))
	      initial-states
	      (generate-states graph (cdr vertices) model-locations :predict? predict?)))
	 (vertex (car vertices))
	 (possible-values
	  (lambda (elements)
	    (possible-values-distribution graph vertex elements model-locations
					  :predict? predict?)))
	 (new-states))
    (dolist (state states new-states)
      (let* ((elements (car state)) (probability (cadr state)))
	(dolist (param (funcall possible-values elements))
	  (let ((new-elements (copy-hash-table elements)))
	    (setf (gethash vertex new-elements) (car param))
	    (push (list new-elements (probs:mul probability (cadr param)))
		  new-states)))))))

(defmethod possible-states ((graph feature-graph) model-locations
			    &key predict?)
  "Call GENERATE-STATES on topologically sorted list of variables, from which
inactive variables have been pruned."
  (generate-states graph (remove-if-not (lambda (v)
					  (member v (f:identifiers (active graph))))
					(graphs:topological-sort graph))
		   model-locations :predict? predict?))
	  
(defmethod model-dataset ((graph feature-graph)
			  dataset &key construct? predict? writers)
  (loop for s in dataset collect
       (model-sequence graph s :construct? construct?
		       :predict? predict?
		       :writers writers)))

(defmethod model-sequence ((graph feature-graph) events
			   &key construct? predict? writers
			     skip-reset?)
  (let ((plausible-states (list (root-state graph)))
	(model-locations (make-hash-table :test #'equal)))
    (labels
	((update-states (events &optional (plausible-states plausible-states)
				(model-locations model-locations))
	   (multiple-value-bind (plausible-states evidence)
	       (model-event graph (car events)
			    :model-locations model-locations
			    :plausible-states plausible-states
			    :construct? construct? :predict? predict?
			    :writers writers)
	     (when (and (> (length plausible-states) 1) construct?)
	       (warn "Training on more than one plausible state; behaviour unknown."))
	     (multiple-value-bind (plausible-states model-locations)
		 (higher-order-states graph plausible-states model-locations
				      :construct? construct?)
	       ;;(when predict? (format t "~A~%" (length plausible-states)))
	       (next graph)
	       (if (null (cdr events))
		   (values plausible-states model-locations evidence)
		   (update-states (cdr events) plausible-states model-locations))))))
      (multiple-value-bind (plausible-states model-locations evidence)
	  (update-states events)
	(dolist (v (f:identifiers (modeled graph)))
	  ;; Call next sequence on all models
	  ;;(dolist (m (f:models f))
	  ;;(models:next-sequence m :construct? construct?)))
	  ;; Call next sequence only on plausible models
	  (dolist (s plausible-states)
	    (let* ((elements (cadr s))
		   (model (model graph v elements))
		   (location
		    (location graph v model elements model-locations)))
	      (models:last-event model (cadr location) :construct? construct?)
	      ;; TODO: Shouldn't this be called only once per model?
	      ;; By iterating over plausible states it may be the same model
	      ;; is used by multiple plausible states.
	      (models:next-sequence model :construct? construct?))))
	(unless (null writers)
	  (dolist (writer writers)
	    (next-sequence writer)))
	(unless skip-reset? (reset graph))
	(values plausible-states model-locations evidence)))))

(defmethod higher-order-states ((graph feature-graph) plausible-states model-locations
				&key construct?)
  "Update MODEL-LOCATIONS with PLAUSIBLE-STATES and tag higher-order states by
replacing branch-ids encoded in each plausible-state (representing branch-id of
source location) with a new branch-id pointing to the new model locations table.
Assumes that HORIZONTAL-DEPTH has not increased."
  (let ((new-model-locations (make-hash-table :test #'equal))
	;; Keep track of unique state ids
	(unique-branches (make-hash-table :test #'equal))
	;; A counter of unique branches per feature
	(branch-counters (make-hash-table))
	(modeled (f:identifiers (modeled graph))))
    (flet ((update-branch-ids (state)
	     (multiple-value-bind (branch-ids elements probability)
		 (values-list state)
	       (setf (branch-ids graph) branch-ids)
	       (let ((new-branch-ids (make-hash-table)))
		 (dolist (v modeled)
		   (let* ((feature (feature graph v))
			  (arguments (vertical-arguments graph v elements))
			  (conditioning (model-conditioning graph feature arguments))
			  (symbol (gethash v elements))
			  (branch-id (gethash v branch-ids))
			  (branch-key (list branch-id v conditioning
					    (gethash v elements))))
		     (multiple-value-bind (new-branch-id exists?)
			 (get-or-create-and-set
			  branch-key unique-branches
			  (incf (gethash v branch-counters -1)))
		       ;;(format t "~a ~a ~a~%" v new-branch-id (gethash v elements))
		       (unless exists?
			 (update-location graph v symbol elements model-locations
					  new-model-locations new-branch-id
					  :construct? construct?))
		       (setf (gethash v new-branch-ids) new-branch-id))))
		 (list new-branch-ids elements probability)))))
      (values (mapcar #'update-branch-ids plausible-states) new-model-locations))))

;; To make it more general:
;; Split out possible state generation
;; First generate possible states
;; Marginalise to observed state
;; Marginalise out stuff that can be marginalised

;; Hypothetical implementation:
;;(defmethod evidence ((graph feature-graph) observation
;;		     &key model-locations plausible-states predict?)
;;  (let ((marginal (marginals:make))
;;	(modeled-vertices (f:identifiers (modeled graph)))
;;	(marginal-vertices (f:identifiers (marginal graph))))
;;    (dolist (horizontal-state plausible-states)
;;      (set-horizontal-state graph horizontal-state)
;;      (let ((active-branch-ids
;;	     (mapcar (lambda (f) (gethash f (car horizontal-state) 0)) modeled-vertices)))
;;	(dolist (state (plausible-states graph observation model-locations
;;					 :predict? predict?))
;;	  (let ((elements (car state))
;;		(probability (cadr state)))
;;	    (marginals:update marginal elements marginal-vertices probability
;;			      (list active-branch-ids))))))
;;    (loop for parameter being the hash-keys of marginal collect
;;	 (let ((branch-ids (dictionary modeled-vertices (car parameter)))
;;	       (probability (marginals:probability marginal parameter))
;;	       (elements (dictionary marginal-vertices (cdr parameter))))
;;	   (list branch-ids elements probability)))))

;; TODO: there may still be a bug where it is possible to merge incompatible model
;; states when marginalising out zeroth- and first-order features.
(defmethod model-event ((graph feature-graph) event
			&key model-locations plausible-states predict?
			  writers)
  (let* ((marginal (marginals:make))
	 (modeled (f:identifiers (modeled graph)))
	 (joint (f:identifiers (marginal graph)))
	 (new-plausible-states) (evidence))
    (dolist (horizontal-state plausible-states)
      (set-horizontal-state graph horizontal-state)
      (unless (null writers)
	(dolist (writer writers)
	  (set-writer-horizontal-state writer horizontal-state)))
      (dolist (state (possible-states graph model-locations :predict? predict?))
	(let ((elements (car state))
	      (probability (cadr state)))
	  (multiple-value-bind (parameter exists?)
	      (marginals:update marginal elements joint probability
				(list (active-branch-ids graph)))
	    ;; Wasteful.
	    ;; Plausible need only called for the possible values of states that
	    ;; "observations" are a function of.
	    (let ((plausible? (plausible? graph elements event)))
	      (infom "state: ~{~A ~}plausible?: ~A exists?: ~A probability: ~A"
		     (element-list graph elements)
		     plausible? exists? (* probability 20))
	      (unless (null writers)
		(dolist (writer writers)
		  (add-possible-state writer elements event plausible? probability)))
	      (when plausible?
		(unless exists? (push parameter new-plausible-states))
		(setf evidence (if (null evidence) probability
				   (probabilities:add evidence probability)))))))))
    (unless (null writers)
      (dolist (writer writers)
	(write-event writer)
	(next-event writer)))
    (let* ((hidden-states
	    (loop for parameter in new-plausible-states collect
		 (let ((branch-ids (dictionary modeled (car parameter)))
		       (probability (marginals:probability marginal parameter))
		       (elements (dictionary joint (cdr parameter))))
		   (list branch-ids elements probability)))))
      (when (null evidence)
	(warn "Model cannot generate event ~A" event) (break))
      (values hidden-states evidence))))

(defun state-elements (state)
  (cadr state))

(defmethod element-list ((graph feature-graph) elements)
  (loop for f in (active graph) collect
       (gethash (f:identifier f) elements)))

(defmethod state-element-list ((graph feature-graph) elements)
  (let ((joint (f:identifiers (marginal graph))))
    (loop for f in joint collect
	 (gethash f elements))))

(defmethod printable ((graph feature-graph))
  "Construct a LIST that can be PRINTed containing representations of each
feature. Does not store domains!"
  (let ((result))
    (dolist (f (modeled graph :active? nil) result)
      (setf result
	    (append (list (intern (symbol-name (f:identifier f)) :keyword)
			  (f::printable f))
		    result)))))

(defmethod from-printable (printable (graph feature-graph))
  "Given a READed PRINTable representation, load the stored parameters into 
GRAPH."
  (loop for (feature-kw printable) on printable by #'cddr while printable do
       (let ((feature (feature graph feature-kw)))
	 (f::from-printable printable feature))))
