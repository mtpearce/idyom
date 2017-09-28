(cl:in-package #:mvs)

;;;; Test the abstract mvs; does it load the models correctly etc.
;;;; Test the generative mvs; does model-sequence produce the right results (compare to results produced by previous implementation

;;;; Test the models on toy domains:
;;;; - two metres with two or three phases
;;;; - a generative viewpoint that decodes alphabet substitution codes and a language model

(5am:def-suite abstract-mvs)
(5am:in-suite abstract-mvs)

(defun random-event-sequences (&key (amount 5) (duration 8) (mode 0)
				 (keysig 0) description barlength pulses)
  (let ((dataset))
    (dotimes (i amount dataset)
      (push (random-event-sequence :duration duration :keysig keysig
				   :pulses pulses :mode mode
				   :description description :barlength barlength)
	    dataset))))

(defun random-event-sequence (&key (duration 8) description (mode 0)
				(keysig 0) barlength pulses)
  (let ((iois)
	(cpitches))
    (dotimes (j duration)
      (push (random 5) iois)
      (push (random 88) cpitches))
    (viewpoints::iois->onset-events iois
				    :mode mode :cpitches cpitches
				    :description description :keysig keysig
				    :barlength barlength :pulses pulses)))

(defun models-equal (a b)
   (let ((a-branches (utils:hash-table->alist (ppm::ppm-branches a)))
	 (b-branches (utils:hash-table->alist (ppm::ppm-branches b)))
	 (a-leaves (utils:hash-table->alist (ppm::ppm-leaves a)))
	 (b-leaves (utils:hash-table->alist (ppm::ppm-leaves b))))
    (and (utils:set-equal a-branches b-branches :test #'equalp)
	 (utils:set-equal a-leaves b-leaves :test #'equalp))))
	
(defun mvs-model (rhythms viewpoint)
  (ppm:build-model (viewpoints:viewpoint-sequences viewpoint rhythms) nil))
    
(defmacro with-adjusted-phase ((event-sequence period phase) &body body)
  `(progn
     (unless (or (null ,period) (null ,phase))
       (dolist (e ,event-sequence)
	 (setf (md:onset e) (+ (mod (- ,period ,phase) ,period)
			       (md:onset e)))))
     (let ((form-result (progn ,@body)))
       (unless (or (null ,period) (null ,phase))
	 (dolist (e ,event-sequence)
	   (setf (md:onset e) (- (md:onset e) (mod (- ,period ,phase) ,period)))))
       form-result)))
    
(defmacro with-different-configurations (&body body)
  `(dolist (models '(:ltm :ltm+ :stm :both :both+))
     (dolist (ltm-order-bound '(4 nil))
       (dolist (stm-order-bound '(4 nil))
	 (let ((*ltm-order-bound* ltm-order-bound)
	       (*stm-order-bound* stm-order-bound)
	       (*models* models))
	   ,@body)))))

(defmacro with-all-interpretations (latent-variable &body body)
  `(loop for category in (lv:categories ,latent-variable)
	(dolist (latent-state (lv:get-latent-states category ,latent-variable))
	  (lv:with-latent-variable-state (latent-state ,latent-variable)
	    ,@body))))

(5am:def-fixture metre-categories ()
  (let* ((metre (lv:get-latent-variable 'metre))
	 (2-metre (lv::create-category metre :barlength 2 :pulses 2))
	 (3-metre (lv::create-category metre :barlength 3 :pulses 1)))
    (&body)))

(5am:def-fixture metre-style-key-categories ()
  (let* ((metre (lv:get-latent-variable 'metre))
	 (style (lv:get-latent-variable 'style))
	 (metre-style-key (lv:get-latent-variable '(style key metre)))
	 (afrocuban (lv::create-category style :style 'afrocuban))
	 (ragtime (lv::create-category style :style 'ragtime))
	 (2-metre (lv::create-category metre :barlength 2 :pulses 2))
	 (3-metre (lv::create-category metre :barlength 3 :pulses 1))
	 (2-ragtime (lv::create-category metre-style-key
					 :barlength 2 :pulses 2 :style 'ragtime))
	 (2-afrocuban (lv::create-category metre-style-key
					   :barlength 2 :pulses 2 :style 'afrocuban))
	 (3-afrocuban (lv::create-category metre-style-key
					   :barlength 3 :pulses 1 :style 'afrocuban)))
    (&body)))

(5am:test make-mvs-normal
  (let* ((dataset (random-event-sequences :barlength 3 :pulses 1))
	 (targets (viewpoints:get-basic-viewpoints '(onset) dataset))
	 (sources (viewpoints:get-viewpoints '(ioi posinbar)))
	 (ltms (mapcar (lambda (viewpoint) (mvs-model dataset viewpoint))
		       sources))
	 (mvs (make-mvs targets sources ltms)))
    (5am:is (equal (map 'list #'viewpoints:viewpoint-name (mvs-basic mvs))
		   (mapcar #'viewpoints:viewpoint-name targets)))
    (5am:is (equal (map 'list #'viewpoints:viewpoint-name (mvs-viewpoints mvs))
		   (mapcar #'viewpoints:viewpoint-name sources)))
    (5am:is (typep (slot-value mvs 'ltm) 'vector))
    (5am:is (typep (slot-value mvs 'stm) 'vector))
    (5am:is (typep (mvs-basic mvs) 'list))
    (5am:is (typep (mvs-viewpoints mvs) 'vector))
    ;; Using basic features not predicted by any of the viewpoints raises a warning.
    (let* ((sources (viewpoints:get-viewpoints '(cpint))))
      (5am:signals warning (make-mvs targets sources nil)))
    ;; Using a viewpoint that does not predict any basic features raises a warning.
    (let* ((sources (viewpoints:get-viewpoints '(ioi cpint))))
      (5am:signals warning (make-mvs targets sources nil)))))

(5am:test (make-mvs-abstract-single-latent-variable :depends-on fail)
  (let* ((2-rhythms (random-event-sequences :barlength 2 :pulses 2 :amount 8))
	 (3-rhythms (random-event-sequences :barlength 3 :pulses 1 :amount 6))
	 (targets (viewpoints:get-basic-viewpoints '(onset)
						   (append 2-rhythms 3-rhythms))))
    (5am:with-fixture metre-categories ()
      ;; Adding non-abstract viewpoints to an abstract mvs raises an error
      (let* ((sources (viewpoints:get-viewpoints '(cpitch abs-sdeg-west))))
	(5am:signals error (make-mvs targets sources nil :class 'abstract-mvs)))
      ;; Model hashtable initialisation (key adds no attributes to the
      ;; category representation).
      (let* ((metre (lv:get-latent-variable 'metre))
	     (abs-posinbar-bardist (viewpoints:get-viewpoint '(abs-posinbar bardist)))
	     (abs-posinbar-bardist-train (viewpoints:training-viewpoint abs-posinbar-bardist))
	     (generative-models (list (list (cons 2-metre
						  (mvs-model 2-rhythms
							     abs-posinbar-bardist-train))
					    (cons 3-metre
						  (mvs-model 3-rhythms
							     abs-posinbar-bardist-train)))))
	     (mvs::*ltm-order-bound* 42)
	     (mvs::*stm-order-bound* 77))
	(setf (lv:categories metre) (list 2-metre 3-metre))
	(setf (latent-variable abs-posinbar-bardist) metre)
	(let ((mvs (make-mvs targets (list abs-posinbar-bardist) generative-models
			     :class 'abstract-mvs
			     :latent-variable metre
			     :viewpoint-latent-variables (list metre))))
	  (5am:is (eq (type-of (slot-value mvs 'ltm)) 'hash-table))
	  (5am:is (eq (type-of (slot-value mvs 'stm)) 'hash-table))
	  (5am:is (typep (mvs-basic mvs) 'list))
	  (5am:is (typep (mvs-viewpoints mvs) 'vector))
	  (dolist (cat (lv:categories metre))
	    (let ((lt-models (gethash cat (slot-value mvs 'ltm)))
		  (st-models (gethash cat (slot-value mvs 'stm))))
	      (5am:is (not (null lt-models)))
	      (5am:is (not (null st-models)))
	      (5am:is (eq (array-dimension lt-models 0) 1))
	      (5am:is (eq (array-dimension st-models 0) 1))
	      (5am:is (eq (type-of (aref lt-models 0)) 'ppm:ppm))
	      (5am:is (eq (type-of (aref st-models 0)) 'ppm:ppm))
	      (5am:is (eq (ppm::ppm-order-bound (aref lt-models 0)) 42))
	      (5am:is (eq (ppm::ppm-order-bound (aref st-models 0)) 77)))))))))

(5am:test (make-mvs-abstract-linked-latent-variable :depends-on fail)
  (let* ((2-rt-rhythms (random-event-sequences :barlength 2 :pulses 2
					       :amount 5 :description "ragtime"))
	 (2-rhythms (random-event-sequences :barlength 2 :pulses 2
					    :amount 8 :description "afrocuban"))
	 (3-rhythms (random-event-sequences :barlength 3 :pulses 1
					    :amount 6 :description "afrocuban"))
	 (targets (viewpoints:get-basic-viewpoints '(onset cpitch)
						  (append 2-rt-rhythms 2-rhythms 3-rhythms))))
  (5am:with-fixture metre-style-key-categories ()
      (let* ((abs-posinbar-bardist (viewpoints:get-viewpoint '(abs-posinbar bardist)))
	     (abs-sdeg-west (viewpoints:get-viewpoint 'abs-sdeg-west))
	     (abs-ioi (viewpoints:get-viewpoint 'abs-ioi))
	     (sources (list abs-posinbar-bardist abs-ioi abs-sdeg-west))
	     (key (lv:get-latent-variable 'key))
	     (2-abs-posinbar-bardist-model (mvs-model (append 2-rt-rhythms 2-rhythms)
						      (viewpoints:training-viewpoint
						       abs-posinbar-bardist)))
	     (3-abs-posinbar-bardist-model (mvs-model 3-rhythms
							 (viewpoints:training-viewpoint
							  abs-posinbar-bardist)))
	     (abs-sdeg-west-model (mvs-model (append 2-rt-rhythms 2-rhythms 3-rhythms)
						  (viewpoints:training-viewpoint
						   abs-sdeg-west)))
	     (rt-style-model (mvs-model 2-rt-rhythms
					  (viewpoints:training-viewpoint
					   abs-ioi)))
	     (ac-style-model (mvs-model (append 2-rhythms 3-rhythms)
					  (viewpoints:training-viewpoint
						abs-ioi)))
	     (generative-models (list (list (cons 2-metre
						  2-abs-posinbar-bardist-model)
					    (cons 3-metre
						  3-abs-posinbar-bardist-model))
				      (list (cons ragtime
						  rt-style-model)
					    (cons afrocuban
						  ac-style-model))
				      (list (cons nil
						  abs-sdeg-west-model)))))
	(setf (lv:categories metre-style-key) (list 2-afrocuban 2-ragtime 3-afrocuban))
	(lv:set-link-categories metre-style-key)
	(setf (latent-variable abs-posinbar-bardist) metre)
	(setf (latent-variable abs-sdeg-west) key)
	(setf (latent-variable abs-ioi) style)
	;; Hypothetical abstract-mvs in which metre, style and key are modelled jointly but
	;; their models are all independent (IRL this that means that metre, style and
	;; key would be inferred independently).
	(let ((mvs (make-mvs targets sources generative-models :class 'abstract-mvs
			     :latent-variable metre-style-key
			     :viewpoint-latent-variables (list metre style key))))
	  (5am:is (eq (array-dimension (%all-stm mvs) 0) 5))
	  (5am:is (eq (array-dimension (%all-ltm mvs) 0) 5))
	  (5am:is (every #'models-equal
			 (coerce (%all-ltm mvs) 'list)
			 (list 2-abs-posinbar-bardist-model
			       3-abs-posinbar-bardist-model
			       rt-style-model
			       ac-style-model
			       abs-sdeg-west-model)))
	  (lv:with-latent-category (2-afrocuban metre-style-key)
	    (multiple-value-bind (metre-model style-model key-model)
		(apply #'values (coerce (mvs-ltm mvs) 'list))
	      (5am:is (models-equal key-model abs-sdeg-west-model))
	      (5am:is (models-equal metre-model 2-abs-posinbar-bardist-model))
	      (5am:is (models-equal style-model ac-style-model))))
	  (lv:with-latent-category (2-ragtime metre-style-key)
	    (multiple-value-bind (metre-model style-model key-model)
		(apply #'values (coerce (mvs-ltm mvs) 'list))
	      (5am:is (models-equal key-model abs-sdeg-west-model))
	      (5am:is (models-equal metre-model 2-abs-posinbar-bardist-model))
	      (5am:is (models-equal style-model rt-style-model))))
	  (lv:with-latent-category (3-afrocuban metre-style-key)
	    (multiple-value-bind (metre-model style-model key-model)
		(apply #'values (coerce (mvs-ltm mvs) 'list))
	      (5am:is (models-equal key-model abs-sdeg-west-model))
	      (5am:is (models-equal metre-model 3-abs-posinbar-bardist-model))
	      (5am:is (models-equal style-model ac-style-model)))))))))

(5am:test (make-mvs-abstract-linked-latent-variable-2 :depends-on fail)
  (let* ((2-rt-rhythms (random-event-sequences :barlength 2 :pulses 2
					       :amount 5 :description "ragtime"))
	 (2-rhythms (random-event-sequences :barlength 2 :pulses 2
					    :amount 8 :description "afrocuban"))
	 (3-rhythms (random-event-sequences :barlength 3 :pulses 1
					    :amount 6 :description "afrocuban"))
	 (targets (viewpoints:get-basic-viewpoints '(onset cpitch)
						   (append 2-rt-rhythms 2-rhythms 3-rhythms))))
    (5am:with-fixture metre-style-key-categories ()
      (let* ((metre-key (lv:get-latent-variable '(metre key)))
	     (key-style (lv:get-latent-variable '(key style)))
	     (metre-key-vp (viewpoints:get-viewpoint '(abs-posinbar bardist
							       abs-sdeg-west)))
	     (key-style-vp (viewpoints:get-viewpoint '(abs-ioi abs-sdeg-west)))
	     (sources (list metre-key-vp key-style-vp))
	     (2-metre-model (mvs-model (append 2-rt-rhythms 2-rhythms)
				       (viewpoints:training-viewpoint metre-key-vp)))
	     (3-metre-model (mvs-model 3-rhythms
				       (viewpoints:training-viewpoint metre-key-vp)))
	     (rt-style-model (mvs-model 2-rt-rhythms
					(viewpoints:training-viewpoint
					 key-style-vp)))
	     (ac-style-model (mvs-model (append 2-rhythms 3-rhythms)
					(viewpoints:training-viewpoint
					 key-style-vp)))
	     (generative-models (list (list (cons 2-metre
						  2-metre-model)
					    (cons 3-metre
						  3-metre-model))
				      (list (cons ragtime
						  rt-style-model)
					    (cons afrocuban
						  ac-style-model)))))
	(setf (lv:categories metre-style-key) (list 2-afrocuban 2-ragtime 3-afrocuban))
	(setf (latent-variable metre-key-vp) metre-key)
	(setf (latent-variable key-style-vp) key-style)
	(let ((mvs (make-mvs targets sources generative-models :class 'abstract-mvs
			     :latent-variable metre-style-key
			     :viewpoint-latent-variables (list metre-key key-style))))
	  (lv:with-latent-category (2-afrocuban metre-style-key)
	    (multiple-value-bind (metre-key-model key-style-model)
		(apply #'values (coerce (mvs-ltm mvs) 'list))
	      (5am:is (models-equal metre-key-model 2-metre-model))
	      (5am:is (models-equal key-style-model ac-style-model))))
	  (lv:with-latent-category (2-ragtime metre-style-key)
	    (multiple-value-bind (metre-key-model key-style-model)
		(apply #'values (coerce (mvs-ltm mvs) 'list))
	      (5am:is (models-equal metre-key-model 2-metre-model))
	      (5am:is (models-equal key-style-model rt-style-model))))
	  (lv:with-latent-category (3-afrocuban metre-style-key)
	    (multiple-value-bind (metre-key-model key-style-model)
		(apply #'values (coerce (mvs-ltm mvs) 'list))
	      (5am:is (models-equal metre-key-model 3-metre-model))
	      (5am:is (models-equal key-style-model ac-style-model)))))))))

(5am:test fail
  (5am:is (eq (+ 1 1) 3)))

;;; Compare an abstract-mvs to individual normal mvs models and ensure that
;;; their results match. The normal mvs uses a posinbar viewpoint.
;;; To test the normal mvs for different phases, the event sequence is locally
;;; phase-shifted by the macro WITH-ADJUSTED-PHASE.
;;; Use a toy dataset with two categories: (3 1) and (2 1) (barlength pulses).
(5am:test abstract-mvs-model-sequence
  (5am:with-fixture metre-categories ()
    (let* ((2-rhythms (random-event-sequences :barlength 2 :pulses 2 :amount 8))
	   (3-rhythms (random-event-sequences :barlength 3 :pulses 1 :amount 6))
	   (event-sequence (viewpoints::iois->onset-events '(0 3 3 2 1 3 3)))
	   (targets (viewpoints:get-basic-viewpoints '(onset) (append (list event-sequence)
								      2-rhythms 3-rhythms)))
	   (latent-variable (lv:get-latent-variable 'metre))
	   (source (viewpoints:get-viewpoint '(bardist abs-posinbar)))
	   (training-viewpoint (viewpoints:training-viewpoint source))
	   (mvs-models (list (list (mvs-model 2-rhythms training-viewpoint))
			     (list (mvs-model 3-rhythms training-viewpoint))))
	   (generative-models (list (list (cons 2-metre (mvs-model 2-rhythms
								   training-viewpoint))
					  (cons 3-metre (mvs-model 3-rhythms
								   training-viewpoint)))))
	   (categories (list 2-metre 3-metre)))
      (setf (viewpoints:latent-variable source) latent-variable
	    (lv:categories latent-variable) categories)
      (let ((abstract-mvs (make-mvs targets (list source) generative-models
				    :class 'abstract-mvs
				    :latent-variable latent-variable
				    :viewpoint-latent-variables
				    (list latent-variable)))
	    (mvss (mapcar (lambda (ltms)
			    (make-mvs targets (list training-viewpoint) ltms)) mvs-models)))
	(with-different-configurations 
	  (loop for category in categories
	     for mvs in mvss do
	       (dolist (latent-state (lv:get-latent-states category latent-variable))
		 (lv:with-latent-variable-state (latent-state latent-variable)
		   (let ((abstract-results (model-sequence abstract-mvs event-sequence
							   :predict? t :construct? t))
			 (mvs-results
			  (model-interpretation-with-mvs
			   mvs event-sequence latent-state latent-variable)))
		     (dolist (mvs (list abstract-mvs mvs))
		       (operate-on-models mvs #'increment-sequence-front)
		       (operate-on-models mvs #'reinitialise-ppm :models 'stm))
		     ;; The first prediction may diverge due to differences
		     ;; in the alphabet derived from BIOI which is
		     ;; transformed with the abstract viewpoint in the
		     ;; abstract-mvs but remains unaltered in the normal
		     ;; case.
		     (5am:is (every #'equal
				    (cdr (first (sequence-predictions->event-likelihoods
					    abstract-results)))
				    (cdr (first (sequence-predictions->event-likelihoods
						 mvs-results))))))))))))))

(5am:test (generative-mvs-model-sequence :depends-on ())
  (let* ((model-a (ppm:build-model '((a b a c a d a b r a) (b a b a)) nil))
	 (model-b (ppm:build-model '((b b a a d d c c r r) (d d b b d d)) nil))
	 (model-a-copy (ppm:build-model '((a b a c a d a b r a) (b a b a)) nil))
	 (model-b-copy (ppm:build-model '((b b a a d d c c r r) (d d b b d d)) nil))
	 (events (mapcar (lambda (s) (make-instance 'md:music-event
						    :onset 0
						    :id (make-instance 'md:event-identifier
								       :event-index 0
								       :composition-index 0
								       :dataset-index 0)
						    :description s))
				'(b b r a a c c)))
	 (generative-models (list (list (cons '(a) model-a)
					(cons '(b) model-b))))
	 (latent-variable (lv:get-latent-variable 'latent3))
	 (targets (viewpoints:get-viewpoints '(description)))
	 (sources (viewpoints:get-viewpoints '(abstract3)))
	 (training-sources (mapcar #'training-viewpoint sources)))
    (setf (viewpoints:viewpoint-alphabet (first targets))
	  '(a b c d r))
    (setf (viewpoints:viewpoint-alphabet (viewpoints:get-viewpoint 'bioi)) nil)
    (setf (viewpoints:latent-variable (first sources)) latent-variable)
    (lv:initialise-prior-distribution (list (cons '(a) '(whatever1 whatever2))
					    (cons '(b) '(whatever1)))
				      latent-variable)
    (with-different-configurations
      (let* ((mvs-models
	     (list (cons '(a) (make-mvs targets training-sources (list model-a-copy)))
		   (cons '(b) (make-mvs targets training-sources (list model-b-copy)))))
	    (abstract-mvs (make-mvs targets sources generative-models
				    :class 'abstract-mvs
				    :latent-variable latent-variable
				    :viewpoint-latent-variables
				    (list latent-variable)))
	    (generative-mvs (make-instance 'generative-mvs :mvs abstract-mvs))
	    (generative-predictions (model-sequence generative-mvs events
						    :predict? t :construct? t))
	    (result-priors (mapcar (lambda (sequence-prediction)
				     (mapcar #'prediction-sets::prediction-prior
					     (prediction-set sequence-prediction)))
				   generative-predictions))
	    (prior-distribution (mapcar #'cdr (lv:prior-distribution latent-variable)))
	    (latent-states (mapcar #'car (lv:prior-distribution latent-variable)))
	    (joint-likelihood-sequences))
	(dolist (latent-state latent-states)
	  (let* ((mvs (cdr (assoc latent-state mvs-models :test #'equal)))
		 (predictions
		  (model-interpretation-with-mvs
		   mvs events latent-state latent-variable))
		 (likelihoods (sequence-predictions->event-likelihoods
			       predictions))
		 (joint-likelihoods (first likelihoods)))
	    (operate-on-models mvs #'increment-sequence-front)
	    (operate-on-models mvs #'reinitialise-ppm :models 'stm)
	    (push joint-likelihoods joint-likelihood-sequences)))
	(let* ((per-event-likelihoods (transpose-lists
				       (reverse joint-likelihood-sequences)))
	       (posteriors (list prior-distribution)))
	  (loop for likelihoods in per-event-likelihoods
	     for result-prior in (first result-priors) do
	       (let* ((prior (car posteriors))
		      (evidence (marginal-likelihood prior
						     likelihoods)))
		 (5am:is (equal result-prior prior))
		 (push (infer-posterior-distribution evidence prior likelihoods)
		       posteriors))))))))

(5am:test (generative-mvs-model-onset-sequence :depends-on ())
  (let* ((model-a (ppm:build-model '((0 1 3 5 6 8)) nil))
	 (model-b (ppm:build-model '((3 4 7 9 11)) nil))
	 (model-a-copy (ppm:build-model '((0 1 3 5 6 8)) nil))
	 (model-b-copy (ppm:build-model '((3 4 7 9 11)) nil))
	 (events (mapcar (lambda (s) (make-instance 'md:music-event
						    :onset s
						    :id (make-instance 'md:event-identifier
								       :event-index 0
								       :composition-index 0
								       :dataset-index 0)))
			 '(0 1 4)))
	 (generative-models (list (list (cons '(2 0) model-a)
					(cons '(3 0) model-b))))
	 (latent-variable (lv:get-latent-variable 'metre))
	 (targets (viewpoints:get-viewpoints '(onset)))
	 (sources (viewpoints:get-viewpoints '(abs-posinbar)))
	 (training-sources (mapcar #'training-viewpoint sources)))
    (setf (viewpoints:viewpoint-alphabet (viewpoints:get-viewpoint 'bioi)) '(0 1 2 3))
    (setf (viewpoints:latent-variable (first sources)) latent-variable)
    (lv:initialise-prior-distribution (list (cons '(2 0) '(whatever1 whatever2))
					    (cons '(3 0) '(whatever1)))
				      latent-variable)
    (with-different-configurations
      (let* ((mvs-models
	      (list (cons '(2 0) (make-mvs targets training-sources (list model-a-copy)))
		    (cons '(3 0) (make-mvs targets training-sources (list model-b-copy)))))
	     (abstract-mvs (make-mvs targets sources generative-models
				     :class 'abstract-mvs
				     :latent-variable latent-variable
				     :viewpoint-latent-variables
				     (list latent-variable)))
	     (generative-mvs (make-instance 'generative-mvs :mvs abstract-mvs))
	     (generative-predictions (model-sequence generative-mvs events
						    :predict? t :construct? t))
	     (result-priors (mapcar (lambda (sequence-prediction)
				      (mapcar #'prediction-sets::prediction-prior
					      (prediction-set sequence-prediction)))
				    generative-predictions))
	     (prior-distribution (mapcar #'cdr (lv:prior-distribution latent-variable)))
	     (latent-states (mapcar #'car (lv:prior-distribution latent-variable)))
	     (joint-likelihood-sequences))
	;; Surgically replace viewpoints with abstract viewpoints
	(mapcar (lambda (c)
		  (setf (slot-value (cdr c) 'viewpoints) (apply #'vector sources)))
		mvs-models)
	(dolist (latent-state latent-states)
	  (lv:with-latent-variable-state (latent-state latent-variable)
	    (let* ((mvs (cdr (assoc (lv:get-category latent-state latent-variable)
				    mvs-models :test #'equal)))
		   (predictions (model-sequence mvs events :predict? t :construct? t))
		   (likelihoods (sequence-predictions->event-likelihoods
				 predictions))
		   (joint-likelihoods (first likelihoods)))
	      (operate-on-models mvs #'increment-sequence-front)
	      (operate-on-models mvs #'reinitialise-ppm :models 'stm)
	      (push joint-likelihoods joint-likelihood-sequences))))
	(let* ((per-event-likelihoods (transpose-lists
				       (reverse joint-likelihood-sequences)))
	       (posteriors (list prior-distribution)))
	  (loop for likelihoods in per-event-likelihoods
	     for result-prior in (first result-priors) do
	       (let* ((prior (car posteriors))
		      (evidence (marginal-likelihood prior
						     likelihoods)))
		 (5am:is (equal result-prior prior))
		 (push (infer-posterior-distribution evidence prior likelihoods)
		       posteriors))))))))

(5am:test (generative-mvs-model-sequence-2 :depends-on ())
  (let* ((2-rt-rhythms (random-event-sequences :barlength 2 :pulses 2 :keysig 1 :mode 0
					       :amount 5 :description "ragtime"))
	 (2-rhythms (random-event-sequences :barlength 2 :pulses 2 :keysig 2 :mode 0
					    :amount 8 :description "afrocuban"))
	 (3-rhythms (random-event-sequences :barlength 3 :pulses 1 :keysig 2 :mode 1
					    :amount 6 :description "afrocuban"))
	 (event-sequence (viewpoints::iois->onset-events '(0 3 2 1 3 3 3)
							 :cpitches
							 '(80 75 75 76 75 79)))
    	 (targets (viewpoints:get-basic-viewpoints '(onset cpitch)
						   (append (list event-sequence)
							   2-rt-rhythms 2-rhythms 3-rhythms))))
    (5am:with-fixture metre-style-key-categories ()
      (let* ((metre-key (lv:get-latent-variable '(metre key)))
	     (key-style (lv:get-latent-variable '(key style)))
	     (metre-key-vp (viewpoints:get-viewpoint '(abs-posinbar bardist
							       abs-sdeg-west)))
	     (key-style-vp (viewpoints:get-viewpoint '(abs-ioi abs-sdeg-west)))
	     (sources (list metre-key-vp key-style-vp))
	     (2-metre-model (mvs-model (append 2-rt-rhythms 2-rhythms)
				       (viewpoints:training-viewpoint metre-key-vp)))
	     (3-metre-model (mvs-model 3-rhythms
				       (viewpoints:training-viewpoint metre-key-vp)))
	     (rt-style-model (mvs-model 2-rt-rhythms
					(viewpoints:training-viewpoint
					 key-style-vp)))
	     (ac-style-model (mvs-model (append 2-rhythms 3-rhythms)
					(viewpoints:training-viewpoint
					 key-style-vp)))
	     (2-metre-model-1 (mvs-model (append 2-rt-rhythms 2-rhythms)
				       (viewpoints:training-viewpoint metre-key-vp)))
	     (3-metre-model-1 (mvs-model 3-rhythms
				       (viewpoints:training-viewpoint metre-key-vp)))
	     (rt-style-model-1 (mvs-model 2-rt-rhythms
					(viewpoints:training-viewpoint
					 key-style-vp)))
	     (ac-style-model-1 (mvs-model (append 2-rhythms 3-rhythms)
					(viewpoints:training-viewpoint
					 key-style-vp)))
	     (generative-models (list (list (cons 2-metre
						  2-metre-model)
					    (cons 3-metre
						  3-metre-model))
				      (list (cons ragtime
						  rt-style-model)
					    (cons afrocuban
						  ac-style-model)))))
	(lv:initialise-prior-distribution (list (cons 2-ragtime 2-rt-rhythms)
						(cons 2-afrocuban 2-rhythms)
						(cons 3-afrocuban 3-rhythms))
					  metre-style-key)
	(setf (latent-variable metre-key-vp) metre-key)
	(setf (latent-variable key-style-vp) key-style)
	(with-different-configurations
	  (print (list models stm-order-bound ltm-order-bound))
	  (let* ((abstract-mvs (make-mvs targets sources generative-models :class 'abstract-mvs
					 :latent-variable metre-style-key
					 :viewpoint-latent-variables (list metre-key key-style)))
		 (generative-mvs (make-instance 'generative-mvs :mvs abstract-mvs))
		 (mvs-models
		  (list (cons 2-afrocuban (make-mvs targets
						    (list (training-viewpoint metre-key-vp)
							  (training-viewpoint key-style-vp))
						    (list 2-metre-model-1 ac-style-model-1)))
			(cons 2-ragtime
			      (make-mvs targets (list (training-viewpoint metre-key-vp)
						      (training-viewpoint key-style-vp))
					(list 2-metre-model-1 rt-style-model-1)))
			(cons 3-afrocuban
			      (make-mvs targets	(list (training-viewpoint metre-key-vp)
						      (training-viewpoint key-style-vp))
					(list 3-metre-model-1 ac-style-model-1)))))
		 (generative-predictions (model-sequence generative-mvs event-sequence
							 :predict? t :construct? nil))
		 (result-priors (mapcar (lambda (sequence-prediction)
					  (mapcar #'prediction-sets::prediction-prior
						  (prediction-set sequence-prediction)))
					generative-predictions))
		 (prior-distribution (mapcar #'cdr (lv:prior-distribution metre-style-key)))
		 (latent-states (mapcar #'car (lv:prior-distribution metre-style-key)))
		 (joint-likelihood-sequences))
	    (mapcar (lambda (c)
		      (setf (slot-value (cdr c) 'viewpoints) (apply #'vector sources)))
		    mvs-models)
	    (dolist (latent-state latent-states)
	      (lv:with-latent-variable-state (latent-state metre-style-key)
		(let* ((mvs (cdr (assoc (lv:get-category latent-state metre-style-key)
					mvs-models :test #'equal)))
		       (predictions (model-sequence mvs event-sequence
						    :predict? t :construct? t))
		       (likelihoods (sequence-predictions->event-likelihoods
				     predictions))
		       (joint-likelihoods (mapcar #'* (first likelihoods)
						  (second likelihoods))))
		  (operate-on-models mvs #'increment-sequence-front)
		  (operate-on-models mvs #'reinitialise-ppm :models 'stm)
		     ;(lv:with-latent-variable-state (latent-state metre-style-key)
		     ;  (5am:is (every #'numbers-approximately-equal 
			;	      (mapcar #'cdr likelihoods)
			;	      (mapcar #'cdr (sequence-predictions->event-likelihoods
			;			     (model-sequence abstract-mvs event-sequence
			;					     :predict? t :construct? nil))))))
		  (push joint-likelihoods joint-likelihood-sequences))))
	    (let ((per-event-likelihoods (transpose-lists
					  (reverse joint-likelihood-sequences)))
		  (posteriors (list prior-distribution)))
	      (loop for likelihoods in per-event-likelihoods
		 for result-prior in (first result-priors) do
		   (let* ((prior (car posteriors))
			  (evidence (marginal-likelihood prior
							 likelihoods)))
		     (5am:is (numbers-approximately-equal result-prior prior))
		     (push (infer-posterior-distribution evidence prior likelihoods)
			   posteriors))))))))))

(defun numbers-approximately-equal (a b &key (tolerance 1.e-5))
  (flet ((approximately-equal (a b)
	   (< (abs (- a b)) tolerance)))
    (and (eq (length a) (length b))
	 (every #'approximately-equal a b))))

(5am:test (infer-posterior-distribution :depends-on marginal-likelihood)
  (let* ((prior '(0.7 0.2 0.1))
	 (likelihoods '(0.5 0.5 0.8))
	 (evidence (marginal-likelihood prior likelihoods)))
    (5am:is (every (lambda (a b) (< (abs (- a b)) 1.e-7))
			   (infer-posterior-distribution evidence prior likelihoods)
			   (list (/ 0.35 evidence) (/ 0.1 evidence) (/ 0.08 evidence))))))

(5am:test marginal-likelihood
  (5am:is (equal (marginal-likelihood '(0.25 0.25 0.5) '(4 8 6))
		 (+ 1 2 3.0))))

(5am:test transpose-lists
  (5am:is (equal (transpose-lists '((a b c) (0 1 2)))
		 '((a 0) (b 1) (c 2))))
  (5am:is (equal (transpose-lists '((a b c) (p q r) (0 1 2)))
		 '((a p 0) (b q 1) (c r 2)))))

(5am:test joint-interpretation-likelihoods 
    (let ((event-interpretation-predictions
	   (list
	    ;; Basic viewpoint 1.
	    (mapcar (lambda (event set) (make-instance 'event-prediction :event event
						       :element 0 :set set))
		    '((0 0 0) (1 0 0) (2 0 0))
		    '(((0 0.3) (1 0.7)) ((0 0.5) (1 0.5)) ((0 0.8) (1 0.2))))
	    ;; Basic viewpoint 2
	    (mapcar (lambda (event set) (make-instance 'event-prediction :event event
						       :element 0 :set set))
		    '((0 0 0) (1 0 0) (2 0 0))
		    '(((0 0.2) (1 0.8)) ((0 0.3) (1 0.7)) ((0 0.9) (1 0.1)))))))
      (let ((likelihoods (joint-interpretation-likelihoods
				event-interpretation-predictions)))
	(5am:is (typep likelihoods 'list))
	(5am:is (eq (length likelihoods) 3))
	(5am:is (equal likelihoods
		       (list (* 0.3 0.2) (* 0.5 0.3) (* 0.8 0.9)))))))

(5am:test get-event-predictions
  (let ((mvs (make-instance 'generative-mvs :mvs
			    (make-instance 'abstract-mvs :basic '(0 1))))
	(sequence-interpretation-predictions
	 (list
	  ;; Interpretation 1
	  (list
	   ;; Basic viewpoint 1
	   (make-instance 'sequence-prediction :set
			  (list (make-instance 'event-prediction :event '(1 1 1))
				(make-instance 'event-prediction :event '(1 1 2))))
	   ;; Basic viewpoint 2
	   (make-instance 'sequence-prediction :set
			  (list (make-instance 'event-prediction :event '(1 2 1))
				(make-instance 'event-prediction :event '(1 2 2)))))
	  (list (make-instance 'sequence-prediction :set
			       (list (make-instance 'event-prediction :event '(2 1 1))
				     (make-instance 'event-prediction :event '(2 1 2))))
		(make-instance 'sequence-prediction :set
			       (list (make-instance 'event-prediction :event '(2 2 1))
				     (make-instance 'event-prediction :event '(2 2 2)))))
	  (list (make-instance 'sequence-prediction :set
			       (list (make-instance 'event-prediction :event '(3 1 1))
				     (make-instance 'event-prediction :event '(3 1 2))))
		(make-instance 'sequence-prediction :set
			       (list (make-instance 'event-prediction :event '(3 2 1))
				     (make-instance 'event-prediction :event '(3 2 2))))))))
    (let ((event-interpretation-predictions
	   (get-event-predictions sequence-interpretation-predictions 0 mvs)))
      (5am:is (typep event-interpretation-predictions 'list))
      (5am:is (eq (length event-interpretation-predictions) 2))
      (5am:is (typep (first event-interpretation-predictions) 'list))
      (5am:is (eq (length (first event-interpretation-predictions)) 3))
      (let ((events (mapcar (lambda (p) (mapcar #'prediction-event p))
			    event-interpretation-predictions)))
	;; event: (interpretation basic-viewpoint event-index)
	(5am:is (equal events
		       '(;; Basic viewpoint 1
			 ((1 1 1) (2 1 1) (3 1 1))
			 ;; Basic viewpoint 2
			 ((1 2 1) (2 2 1) (3 2 1)))))))
    (let* ((event-interpretation-predictions
	    (get-event-predictions sequence-interpretation-predictions 1 mvs))
	   (events (mapcar (lambda (p) (mapcar #'prediction-event p))
			   event-interpretation-predictions)))
      (5am:is (equal events
		     '(;; Basic viewpoint 1
		       ((1 1 2) (2 1 2) (3 1 2))
		       ;; Basic viewpoint 2
		       ((1 2 2) (2 2 2) (3 2 2))))))))
		      
	    					     

(defun sequence-predictions->event-likelihoods (sequence-predictions)
  (let ((event-likelihoods))
    (dolist (prediction sequence-predictions)
      (push (mapcar #'cadr (event-predictions prediction))
	    event-likelihoods))
    (reverse event-likelihoods)))

(defun model-interpretation-with-mvs (mvs sequence latent-state latent-variable)
  ;; Adjust the sequence to be correctly interpreted by posinbar.
    (dolist (e sequence)
      (dolist (parameter (lv::latent-state-parameters latent-variable))
	(let ((symbol (intern (symbol-name parameter) (find-package :md))))
	  (unless (not (member symbol md:*md-music-slots*))
	    (lv:get-latent-state-parameter latent-state parameter latent-variable)
	    (setf (slot-value e symbol)
		  (lv:get-latent-state-parameter latent-state parameter latent-variable))))))
    ;; Further adjust the sequence to be interpreted in the correct phase
    ;; by posinbar.
    
    (with-adjusted-phase (sequence (lv::get-latent-state-parameter latent-state :barlength
								   latent-variable)
				   (lv:get-latent-state-parameter  latent-state :phase
								   latent-variable))
      (model-sequence mvs sequence :predict? t :construct? t)))
