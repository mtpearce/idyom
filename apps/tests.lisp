(cl:in-package #:resampling)

(defun dataset->music-dataset (dataset dataset-index)
  (let ((description (first dataset))
	(timebase (second dataset))
	(midc (third dataset))
	(compositions (nthcdr 3 dataset))
	(identifier (md:make-dataset-id dataset-index)))
    (let ((dataset (make-instance 'md:music-dataset
				  :id identifier
				  :midc midc :timebase timebase
				  :description description))
	  (compositions (loop for composition in compositions
			   for composition-index below (length compositions) collect
			     (composition->music-composition composition midc timebase
							     dataset-index
							     composition-index))))
      (sequence:adjust-sequence dataset (length compositions)
				:initial-contents (nreverse compositions))
      dataset)))

(defun composition->music-composition (composition midc timebase
				       dataset-index composition-index)
  (let ((description (car composition))
	(events (cdr composition))
	(music-events)
	(identifier (md:make-composition-id dataset-index composition-index)))
    (loop for event in events for index below (length events) do
	 (push (event->music-event event midc timebase
				   dataset-index composition-index index)
	       music-events))
    (let ((composition (make-instance 'md:music-composition
				      :id identifier
				      :duration (md::end-time (car music-events))
				      :onset 0
				      :midc midc :timebase timebase
				      :description description)))
      (sequence:adjust-sequence composition (length events)
				:initial-contents (nreverse music-events))
      composition)))

(defun event->music-event (event midc timebase dataset-index composition-index event-index)
  (let ((attributes (mapcar #'car event))
	(values (mapcar #'cadr event))
	(identifier (md:make-event-id dataset-index composition-index event-index)))
    (let ((music-event (make-instance 'md:music-event
				      :id identifier
				      :description ""
				      :midc midc
				      :timebase timebase)))
      (loop for attrib in attributes
	 for value in values do
	   (setf (slot-value music-event (intern (symbol-name attrib)
						 (find-package :md))) value))
      music-event)))

(defun distributions-equal (a b &key (tolerance 1.e-5))
  (flet ((distribution-symbols (d)
	   (mapcar #'car d))
	 (approximately-equal (a b)
	   (< (abs (- a b)) tolerance)))
    (and (utils:set-equal (distribution-symbols a)
			  (distribution-symbols b)
			  :test #'equal)
	 (every #'approximately-equal
		(mapcar #'cdr a)
		(mapcar (lambda (s) (cdr (assoc s b :test #'equal)))
			(distribution-symbols a))))))

(defun numbers-approximately-equal (a b &key (tolerance 1.e-5))
  (flet ((approximately-equal (a b)
	   (< (abs (- a b)) tolerance)))
    (and (eq (length a) (length b))
	 (every #'approximately-equal a b))))


(defun legacy-metre-string->latent-state (metre-string latent-variable)
  (multiple-value-bind (barlength pulses timebase phase)
      (apply #'values (read-from-string metre-string))
    (lv::create-latent-state latent-variable
			     (lv::create-category latent-variable
						  :barlength
						  (md::convert-time-slot barlength
									 timebase)
						  :pulses pulses)
			     :phase (md::convert-time-slot phase timebase))))

;;;=====================
;;; *Generative systems*
;;;=====================

(5am:def-suite generative-systems :in resampling)
(5am:in-suite generative-systems)

;;; Load a dataset fixture from a file-fixture and convert to melodic sequences.
(5am:def-fixture dataset-fixture (path)
  (let* ((dataset (utils:read-object-from-file path))
	 (dataset (dataset->music-dataset dataset 0))
	 (compositions))
    (sequence:dosequence (c dataset)
      (push (md::composition->monody c)
	    compositions))
    (&body)))

;;; Calculate the event likelihood based on results stored in a validation
;;; fixture.
(defun event-likelihoods (likelihoods posteriors)
  (flet ((event-likelihood (position likelihoods posteriors)
	   (let ((interpretations (mapcar #'car posteriors)))
	     (apply '+ (mapcar #'(lambda (i) 
				   (* (nth position (cdr (assoc i likelihoods :test #'equal)))
				      (nth position (cdr (assoc i posteriors :test #'equal)))))
			       interpretations)))))
    (loop for p below (length (cdr (car likelihoods))) collecting
	 (event-likelihood p likelihoods posteriors))))

;;; Convert the distributions as stored in the validation fixture to
;;; something more similar to the native representation of results.
(5am:def-fixture legacy->native (likelihoods posteriors latent-variable)
  (flet ((convert (s)
	   (legacy-metre-string->latent-state s latent-variable)))
    (let ((likelihoods (let ((likelihood-params (mapcar #'convert (mapcar #'car likelihoods))))
			 (mapcar #'cons likelihood-params
				 (mapcar #'cdr likelihoods))))
	  (posteriors (let ((posterior-params (mapcar #'convert (mapcar #'car posteriors))))
			(loop for n below (length (cdr (car likelihoods))) collect
			     (mapcar #'cons
				     posterior-params
				     (mapcar (lambda (series)
					       (elt (cdr series) n)) posteriors))))))
      (&body))))

;;; Load results from validation fixture.
(5am:def-fixture results-fixture (path)
  (let* ((results (utils:read-object-from-file path))
	 (likelihoods (car results))
	 (posteriors (cdr results)))
    (&body)))

;;; Configure mvs to produce same results as in validation fixture.
(5am:def-fixture comparison-environment ()
  (let ((mvs::*ltm-order-bound* nil)
	(mvs::*models* :ltm)
	(md::*md-timebase* 16))
    (&body)))
		       
(5am:def-fixture abstract-mvs (targets sources training-set latent-variable)
    (let ((ltms (get-long-term-generative-models sources latent-variable (reverse training-set)
						 nil nil nil nil nil nil nil)))
      (let ((abstract-mvs (make-mvs targets sources ltms
				    :class 'mvs::abstract-mvs
				    :latent-variable latent-variable)))
	(&body))))

(5am:test partition-dataset)
(5am:test partition-composition)
(5am:test make-viewpoint-model)
(5am:test (make-abstract-viewpoint-model :depends-on make-viewpoint-model))
(5am:test (get-long-term-models :depends-on make-viewpoint-model))
(5am:test (get-long-term-generative-models :depends-on make-abstract-viewpoint-model))

(5am:test (prior-calculation-matches-fixture :depends-on (and . (partition-dataset)))
  (5am:with-fixture comparison-environment () 
    (5am:with-fixture dataset-fixture
	("/home/bastiaan/projects/idyom/apps/fixtures/test-dataset-training.lisp")
      (let ((training-set compositions)
	    (latent-variable (lv:get-latent-variable 'metre)))
	(let ((training-data (lv::partition-dataset training-set latent-variable)))
	  (lv:initialise-prior-distribution training-data latent-variable))
	(5am:with-fixture results-fixture
	    ("/home/bastiaan/projects/idyom/apps/fixtures/test-results/simulations/0")
	  (5am:with-fixture legacy->native (likelihoods posteriors latent-variable)
	    likelihoods
	    (5am:is (distributions-equal (lv:prior-distribution latent-variable)
					 (first posteriors)))))))))

(5am:test fail (5am:is (eq (+ 1 1) 3)))

(5am:test (abstract-mvs-against-fixture-2 :depends-on (and . (fail
							      partition-dataset 
							    get-long-term-generative-models)))
  (5am:with-fixture comparison-environment ()
    (5am:with-fixture dataset-fixture
	("/home/bastiaan/projects/idyom/apps/fixtures/test-dataset-training.lisp")
      (let* ((training-set compositions)
	     (latent-variable (lv:get-latent-variable 'metre))
	     (sources (viewpoints:get-viewpoints '((abs-posinbar bardist-legacy))))
	     (targets (viewpoints:get-basic-viewpoints '(onset) training-set)))
	(5am:with-fixture abstract-mvs (targets sources training-set latent-variable)
	  (5am:with-fixture dataset-fixture
	      ("/home/bastiaan/projects/idyom/apps/fixtures/test-dataset-test.lisp")
	    (let ((composition (second compositions))
		  (interpretations (utils:read-object-from-file "/home/bastiaan/projects/idyom/apps/fixtures/interpretations-1"))
		  (interpretation-predictions (utils:read-object-from-file "/home/bastiaan/projects/idyom/apps/fixtures/interpretation-predictions-1")))
	      (dolist (category (lv:categories latent-variable))
		(dolist (latent-state (reverse (lv:get-latent-states category latent-variable)))
		  (lv:with-latent-variable-state (latent-state latent-variable)
		    (let* ((predictions (model-sequence abstract-mvs
							(coerce composition 'list)
							:construct? t :predict? t)))
		      ;;(operate-on-models abstract-mvs #'increment-sequence-front)
		      (flet ((event-probabilities (prediction-set)
			       (let* ((event-predictions (event-predictions prediction-set)))
				 (mapcar #'cadr event-predictions))))
			(let* ((abstract-mvs-likelihoods (event-probabilities (first predictions)))
			       (reference-likelihoods (cdr (assoc latent-state
								   interpretation-predictions
								   :key (lambda (s) (legacy-metre-string->latent-state s latent-variable))
								   :test #'equal)))
			       (reference-interpretation (cadr (assoc latent-state
								      interpretations
								      :key (lambda (s) (legacy-metre-string->latent-state s latent-variable))
								      :test #'equal))))
			  (when (not (numbers-approximately-equal abstract-mvs-likelihoods
								  reference-likelihoods :tolerance 1.e-5))
			    (format t "~A~%" latent-state))
			  (5am:is (equal reference-interpretation (mapcar #'reverse (viewpoints:viewpoint-sequence (first sources) composition))))
			  (5am:is (numbers-approximately-equal abstract-mvs-likelihoods
							       reference-likelihoods :tolerance 1.e-5)))))))))))))))

(5am:test (test-specifics :depends-on (and . (partition-dataset 
					      get-long-term-generative-models)))
  (5am:with-fixture comparison-environment ()
    (5am:with-fixture dataset-fixture
	("/home/bastiaan/projects/idyom/apps/fixtures/test-dataset-training.lisp")
      (let* ((training-set compositions)
	     (latent-variable (lv:get-latent-variable 'metre))
	     (partitioned-training-set (lv::partition-dataset training-set latent-variable))
	     (training-set (cdr (assoc '(16 4) partitioned-training-set :test #'equal)))
	     (sources (viewpoints:get-viewpoints '((abs-posinbar bardist-legacy))))
	     (targets (viewpoints:get-basic-viewpoints '(onset) training-set)))
	(5am:with-fixture abstract-mvs (targets sources training-set latent-variable)
	  (5am:with-fixture dataset-fixture
	      ("/home/bastiaan/projects/idyom/apps/fixtures/test-dataset-test.lisp")
	    (let* ((composition (second compositions))
		   (latent-state '(16 8 4))
					;(mvs::*debug* t)
		   (targets (viewpoints:get-basic-viewpoints '(onset) training-set))
		   (mvs (make-mvs targets sources
				  (resampling:get-long-term-models
				   (mapcar #'viewpoints:training-viewpoint sources)
				   training-set nil nil nil nil nil nil nil)))
		   (training-sequences (viewpoints:viewpoint-sequences
					(training-viewpoint (first sources))
					training-set))
		   (reference-training-set 
		    (utils:read-object-from-file
		     "/home/bastiaan/projects/idyom/apps/fixtures/4-4-training-set"))
		   (interpretations
		    (utils:read-object-from-file
		     "/home/bastiaan/projects/idyom/apps/fixtures/interpretations-1"))
		   (interpretation-predictions
		    (utils:read-object-from-file
		     "/home/bastiaan/projects/idyom/apps/fixtures/interpretation-predictions-1")))
	      ;(print (mapcar (lambda (s) (mapcar #'reverse s)) training-set))
	      (5am:is (set-equal (mapcar (lambda (s) (mapcar #'reverse s)) training-sequences)
				 reference-training-set :test #'equal))
	      ;(print (set-difference training-set reference-training-set :test #'equal))
	      (lv:with-latent-variable-state (latent-state latent-variable)
		(let ((ltm (aref (mvs::mvs-ltm abstract-mvs) 0)))
		  (with-open-file (s "/home/bastiaan/new-model"
				     :direction :output :if-exists :overwrite
				     :if-does-not-exist :create)
		    (format s "~S~%" (sort (utils:hash-table->alist (ppm::ppm-leaves ltm)) #'> :key #'car))
		    (format s "~S~%" (sort (utils:hash-table->alist (ppm::ppm-branches ltm)) #'> :key #'car))))
		(let* ((normal-mvs-predictions (model-sequence mvs (coerce composition 'list)
							       :construct? nil :predict? t))
		       (predictions (model-sequence abstract-mvs
						    (coerce composition 'list)
						    :construct? nil :predict? t)))
		  ;;(operate-on-models abstract-mvs #'increment-sequence-front)
		  (flet ((event-probabilities (prediction-set)
			   (let* ((event-predictions (event-predictions prediction-set)))
			     (mapcar #'cadr event-predictions))))
		    (let* ((abstract-mvs-likelihoods (event-probabilities (first predictions)))
			   (normal-mvs-likelihoods (event-probabilities (first normal-mvs-predictions)))
			   (reference-likelihoods (cdr (assoc latent-state
							      interpretation-predictions
							      :key (lambda (s) (legacy-metre-string->latent-state s latent-variable)
							      :test #'equal))))
			   (reference-interpretation (cadr (assoc latent-state
								  interpretations
								  :key (lambda (s) (legacy-metre-string->latent-state s latent-variable))
								  :test #'equal))))
		      (5am:is (equal reference-interpretation (mapcar #'reverse (viewpoints:viewpoint-sequence (first sources) composition))))
		      ;;(print reference-likelihoods)
		      (print normal-mvs-likelihoods)
		      (print (mapcar #'- normal-mvs-likelihoods abstract-mvs-likelihoods))
		      ;;(print (mapcar #'- reference-likelihoods abstract-mvs-likelihoods))
		      (5am:is (numbers-approximately-equal abstract-mvs-likelihoods
							   normal-mvs-likelihoods :tolerance 1.e-5))
		      (5am:is (numbers-approximately-equal reference-likelihoods
							   normal-mvs-likelihoods :tolerance 1.e-5))
		      (5am:is (numbers-approximately-equal abstract-mvs-likelihoods
							   reference-likelihoods :tolerance 1.e-5)))))))))))))


(5am:test (abstract-mvs-against-fixture :depends-on (and . (fail
							    partition-dataset 
							    get-long-term-generative-models)))
  (5am:with-fixture comparison-environment ()
    (5am:with-fixture dataset-fixture
	("/home/bastiaan/projects/idyom/apps/fixtures/test-dataset-training.lisp")
      (let* ((training-set compositions)
	     (latent-variable (lv:get-latent-variable 'metre))
	     (sources (viewpoints:get-viewpoints '((abs-posinbar bardist-legacy))))
	     (targets (viewpoints:get-basic-viewpoints '(onset) training-set)))
	(5am:with-fixture abstract-mvs (targets sources training-set latent-variable)
	  (5am:with-fixture dataset-fixture
	      ("/home/bastiaan/projects/idyom/apps/fixtures/test-dataset-test.lisp")
	    (let ((test-set compositions))
	      (dotimes (composition-index (min (length test-set) 2))
		(5am:with-fixture results-fixture
		    ((format nil
			     "/home/bastiaan/projects/idyom/apps/fixtures/test-results/simulations/~a"
			     composition-index))
		  (5am:with-fixture legacy->native (likelihoods posteriors
								latent-variable)
		    posteriors
		    (dolist (category (lv:categories latent-variable))
		      (dolist (latent-state (reverse (lv:get-latent-states category latent-variable)))
			(lv:with-latent-variable-state (latent-state latent-variable)
			  (let* ((predictions (model-sequence abstract-mvs (coerce
									    (elt test-set
										 composition-index)
									    'list)
							      :construct? t :predict? t)))
			    ;;(operate-on-models abstract-mvs #'increment-sequence-front)
			    (operate-on-models abstract-mvs #'reinitialise-ppm :models 'mvs::stm)
			    (flet ((event-probabilities (prediction-set)
				     (let* ((event-predictions (event-predictions prediction-set)))
				       (mapcar #'cadr event-predictions))))
			      (let* ((abstract-mvs-likelihoods (event-probabilities (first predictions)))
				     (fixture-likelihoods (cdr (assoc latent-state
								      likelihoods
								      :test #'equal))))
				;; (when (not (numbers-approximately-equal abstract-mvs-likelihoods
				;;					  fixture-likelihoods :tolerance 1.e-9))
				;; (print (cons latent-state composition-index)))
				;; (print (mapcar (lambda (a b) (- a b)) abstract-mvs-likelihoods fixture-likelihoods))
				(5am:is (numbers-approximately-equal abstract-mvs-likelihoods
								     fixture-likelihoods :tolerance 1.e-5))))))))))))))))))

(5am:test (generative-mvs-against-fixture :depends-on (and . (fail
							      partition-dataset 
							      get-long-term-generative-models)))
  (5am:with-fixture dataset-fixture
      ("/home/bastiaan/projects/idyom/apps/fixtures/test-dataset-training.lisp")
    (let* ((training-set compositions)
	   (latent-variable (lv:get-latent-variable 'metre))
	   (sources (viewpoints:get-viewpoints '((abs-posinbar bardist-legacy))))
	   (targets (viewpoints:get-basic-viewpoints '(onset) training-set)))
      (5am:with-fixture abstract-mvs (targets sources training-set latent-variable)
	(let ((generative-mvs (make-instance 'mvs::generative-mvs :mvs abstract-mvs)))
	  (5am:with-fixture dataset-fixture
	      ("/home/bastiaan/projects/idyom/apps/fixtures/test-dataset-test.lisp")
	    (5am:with-fixture comparison-environment ()
	      (let ((test-set compositions))
		(dotimes (composition-index (length test-set))
		  (let* ((predictions (model-sequence generative-mvs (coerce
								      (elt test-set
									   composition-index)
								      'list)
						      :construct? t :predict? t)))
		    (operate-on-models generative-mvs #'increment-sequence-front)
		    (operate-on-models generative-mvs #'reinitialise-ppm :models 'mvs::stm)
		    (5am:with-fixture results-fixture
			((format nil "/home/bastiaan/projects/idyom/apps/fixtures/test-results/simulations/~A"
				 composition-index))
			(flet ((event-probabilities (prediction-set)
				 (mapcar #'cadr (event-predictions prediction-set))))
			  (let* ((event-likelihoods (event-likelihoods likelihoods posteriors))
				 (generative-likelihoods
				  (event-probabilities (first predictions))))
;			    (print (length generative-likelihoods))
;			    (print (length event-likelihoods))
			    (5am:is (numbers-approximately-equal generative-likelihoods
								 event-likelihoods)))))))))))))))

      
