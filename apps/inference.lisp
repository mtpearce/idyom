(in-package #:idyom)

(defmethod model-sequence (training-set-id target-viewpoints source-viewpoints
			   event-sequence &key (voices nil) (texture :melody))
  (let* ((training-set (md:get-music-objects (if (listp training-set-id)
						 training-set-id (list training-set-id))
					     nil :voices voices :texture texture))
	 (sources (viewpoints:get-viewpoints source-viewpoints))
	 (targets (viewpoints:get-basic-viewpoints target-viewpoints training-set)))
    (multiple-value-bind (categories segmented-training-set)
	(segment-dataset training-set)
      (let* ((category-counts (mapcar #'length segmented-training-set))
	     (models (category-models segmented-training-set
				      categories targets sources))
	     (likelihood-distributions (category-predictions categories models event-sequence))
	     (prior-distribution (prior-distribution categories category-counts))
	     (posteriors (list prior-distribution))
	     (evidences))
	(loop for likelihood-distribution in likelihood-distributions collecting
	     (let ((evidence (marginal-likelihood likelihood-distribution
							prior-distribution))
		   (prior-distribution (cdr posteriors)))
	       (push (infer-posterior-distribution evidence prior-distribution
						   likelihood-distribution)
		     posteriors)
	       (push evidence evidences)))
	(values evidences likelihood-distributions posteriors)))))

(defun infer-posterior-distribution (evidence prior-distribution likelihood-distribution)
  (loop
     for likelihood in likelihood-distribution
     for prior in prior-distribution collecting
       (/ (* likelihood prior) evidence)))

;; Latent-variable specific
(defun prior-distribution (categories category-counts)
  (let ((total-observations (apply #'+ category-counts)))
    (let ((unnormalised-prior
	   (apply #'append
		  (loop for category in categories for count in category-counts collecting
		       (loop for phase in (getf category :barlength) collecting
			    (/ count total-observations))))))
      (prediction-sets:normalise-distribution unnormalised-prior))))

(defun likelihood-distributions (categories models sequence)
  (let* ((sequence-predictions
	  (category-predictions categories models sequence))
	 (sequence-likelihoods (prediction-sets->likelihoods sequence-predictions)))
    (loop for i below (length sequence) collecting
	 (mapcar #'(lambda (likelihoods) (elt likelihoods i)) sequence-likelihoods))))

(defun marginal-likelihood (prior-distribution likelihood-distribution)
  (apply #'+
	 (mapcar (lambda (prior likelihood) (* prior likelihood))
		 prior-distribution likelihood-distribution)))		      

(defun category-models (segmented-training-set targets sources categories)
  (mapcar #'(lambda (category training-data) (category-model training-data
							     targets sources category))
	  categories segmented-training-set))

(defun category-model (segmented-training-set targets sources category)
  (let ((ltms (get-long-term-category-models category sources segmented-training-set)))
    (mvs:make-mvs targets sources ltms)))

(defun get-long-term-category-models (category sources training-data)
  (viewpoints:with-latent-state category
    ;;; Fix caching
    (resampling:get-long-term-models sources training-data
				     nil nil nil nil nil nil nil)))

;;; Model and latent variable specific
(defun category-index (categories category)
  (loop
     for i below (length categories)
     for cat in categories do
       (when (and (eql (getf cat :barlength) (getf category :barlength))
		  (eql (getf cat :pulses) (getf category :pulses)))
	   (return i))))
       
(defun segment-dataset (dataset &optional categories segmented-dataset)
  (let ((composition (car dataset))
	(remaining-compositions (cdr dataset)))
    (multiple-value-bind (categories segmented-dataset)
	(segment-composition categories segmented-dataset (coerce composition 'list))
      (if (null remaining-compositions)
	  (values categories segmented-dataset)
	  (segment-dataset remaining-compositions categories segmented-dataset)))))

;; Model and latent-variable specific
(defun segment-composition (categories segmented-dataset composition
			    &optional subsequence barlength pulses)
  "<categories> is a list of categories encountered so far. <segmented-dataset> is
a list of lists of excertps associated with each category.
Both <categories> and <segmented-dataset> may be empty. 
Split <composition> into continuous excerpts during which the category does 
not change. Return <categories>, updated with any new categories encountered, and
<segmented-dataset> updated with the new excerpts inserted the appropriate place."
  (let* ((event (car composition))
	 (remaining-events (cdr composition))
	 (barlength (if (null barlength) (md:barlength event) barlength))
	 (pulses (if (null pulses) (md:pulses event) pulses))
	 (category (list :barlength barlength :pulses pulses)))
    (if (and (and (eql (md:barlength event) barlength)
		  (eql (md:pulses event) pulses))
	     (not (null remaining-events)))
	(segment-composition categories segmented-dataset remaining-events
			     (cons event subsequence) barlength pulses)
	(let* ((index (category-index categories category))
	       (categories (if (null index)
			       (cons category categories)
			       categories))
	       (segmented-dataset (if (null index)
				      (cons (list (reverse subsequence)) segmented-dataset)
				      (push (reverse subsequence)
					    (elt segmented-dataset index)))))
	  (if (null remaining-events)
	      (values categories segmented-dataset)
	      (segment-composition categories segmented-dataset remaining-events))))))

(defun category-predictions (categories models sequence)
  (apply #'append
	 (loop for category in categories
	    for model in models collecting
	      (interpretation-predictions category model sequence))))

;; Model and latent-variable specific
(defun interpretation-predictions (category model sequence)
  (viewpoints:with-latent-state category
    (loop for phase below (getf category :barlength) collecting
	 (viewpoints:with-latent-state '(:phase phase)
	   (mvs:model-sequence model sequence)))))

(defun prediction-sets->likelihoods (prediction-sets)
  (mapcar #'prediction-set->likelihoods prediction-sets))

(defun prediction-set->likelihoods (prediction-set)
  (prediction-sets::distribution-probabilities
   (prediction-sets:event-predictions prediction-set)))
