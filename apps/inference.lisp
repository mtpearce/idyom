;;; Code for modelling perceptual inference using IDyOM

(cl:in-package #:inference)

(defparameter *verbose* nil)

; Directory for caching meter counts
(defparameter *counts-dir* 
  (ensure-directories-exist
   (merge-pathnames "data/counts/" (utils:ensure-directory utils:*root-dir*))))

(defgeneric infer-meter (training-set target-viewpoints source-viewpoints test-sequence
			&key &allow-other-keys))

(defmethod infer-meter ((training-set list) target-viewpoints source-viewpoints test-sequence
			&rest kwargs &key &allow-other-keys)
  (let ((training-set-promise 
	 (promises:make-promise :function training-set 
				:id (composition-list-signature training-set))))
    (apply #'infer-meter (append (list training-set-promise target-viewpoints 
					 source-viewpoints test-sequence)
				   kwargs))))

(defmethod infer-meter ((training-set promises:promise) 
			target-viewpoints source-viewpoints test-sequence
			&key (voices nil) (texture :grid) 
			  (resolution 16) (use-cache? t) &allow-other-keys)
    (let* ((sources (viewpoints:get-viewpoints source-viewpoints))
	   (targets (viewpoints:get-basic-viewpoints target-viewpoints training-set texture))
	   ;; Obtain event counts per meter
	   (meter-counts (count-meters training-set texture resolution :use-cache? nil))
	   ;; Extract a list of metrical interpretations
	   (meters (mapcar #'(lambda (meter-count) 
			       (md:meter-string->metrical-interpretation 
				(car meter-count) resolution)) 
			   meter-counts))
	   ;; Generate a Nparams x Ntarget-viewpoints x Nevents matrix for event-likelihoods
	   (likelihoods 
	    (generate-meter-predictions training-set meters targets sources
					test-sequence
					:resolution resolution :voices voices
					:texture texture :use-cache? use-cache?))
	   ;; Initialize the prior distribution
	   (prior-distribution (initialise-prior-distribution meter-counts resolution))
	   ;; Convert to a Nparams x Nevents data structure where each column is 
	   ;; a probability distribution over params
	   (posteriors (generate-meter-posteriors prior-distribution likelihoods 
					      (length test-sequence)))
	   (information-contents 
	    (loop for p below (length test-sequence) collecting
		 (- (log (event-likelihood p likelihoods posteriors)
			 2)))))
      (values prior-distribution likelihoods posteriors information-contents)))

(defun event-likelihood (position likelihoods posteriors)
  "Return the predictive likelihood of an event at position <position>"
  (let ((interpretations (prediction-sets:distribution-symbols posteriors)))
    (apply '+ (mapcar #'(lambda (i) 
			  (* (nth position (lookup-key i likelihoods))
			     (nth position (lookup-key i posteriors))))
		      interpretations))))

(defun generate-meter-posteriors (prior-distribution likelihoods n) 
  (when *verbose* (format t "Performing Bayesian inference using predictions and the prior~%"))
  (let* ((params (prediction-sets:distribution-symbols prior-distribution))
	 ;; Initialise results with the prior
	 (results (mapcar #'(lambda (param) 
			      (cons param
				    (copy-list 
				     (lookup-key param prior-distribution)))) params)))
    ;; Iterate over positions in the test sequence to infer meter
    (dotimes (position n)
      (let ((evidence (apply #'+ 
			     (mapcar #'(lambda (m) 
					 (* (get-event-likelihood m position likelihoods)
					    (first (lookup-key m results))))
				     params))))
	(dolist (meter params)
	  (let* ((posteriors (lookup-key meter results))
		 (likelihood (get-event-likelihood meter position likelihoods))
		 (prior (first posteriors))
		 (posterior (/ (* likelihood prior) evidence)))
	    ; Store the result
	    (push posterior (cdr (assoc meter results :test #'string-equal)))))))
    ;; Reverse the list of probabilities for each meter
    (dolist (param params)
      (let ((result (assoc param results :test #'string-equal)))
	(rplacd result (nreverse (cdr result)))))
    results))


(defun generate-meter-predictions (training-set-promise categories targets sources test-sequence
				   &key (resolution 16) voices texture use-cache?)
  (when *verbose* 
    (format t "Generating predictions for the test sequence in all interpretations~%"))
  (flet ((model-sequence (model interpretation)
	   (mvs:model-sequence model (coerce test-sequence 'list) texture :construct? nil 
				:predict? t :interpretation interpretation))
	 (make-mvs (category)
	   (let ((ltms (resampling:get-long-term-models sources training-set-promise
							nil (promises:get-identifier 
							     training-set-promise) nil nil
							:voices voices :texture texture
							:interpretation category
							:resolution resolution
							:use-cache? use-cache?)))
	     (mvs:make-mvs targets sources ltms))))
    ;; Create a list of lists containing each category in each possible phase
    (let ((interpretations 
	   (apply 'append (mapcar #'(lambda (c) (md:create-interpretations c resolution))
				   categories))))
      (let ((multiple-viewpoint-systems (mapcar #'make-mvs interpretations)))
	(mapcar #'(lambda (interpretation model) 
		    ;; FIXME: Only the predictions of the *first* target viewpoint are 
		    ;; taken into account here! They should be combined.
		    (cons (md:meter-string interpretation)
			  (prediction-sets:distribution-probabilities
			   (prediction-sets:event-predictions 
			    (first (model-sequence model interpretation))))))
		interpretations multiple-viewpoint-systems)))))

(defgeneric count-meters (training-set texture resolution 
			  &key &allow-other-keys))

(defmethod count-meters ((training-set promises:promise) texture resolution
			 &key (use-cache? t) (per-composition? nil))
  (let ((filename (format nil "~A~A-~A" *counts-dir* 
			  (promises:get-identifier training-set) resolution)))
    (unless (and (utils:file-exists filename) use-cache?)
      (let ((training-set (promises:retrieve training-set)))
	(utils:write-object-to-file (count-meters training-set texture resolution    
						  :per-composition? per-composition?)
				    filename))
      (when *verbose* (format t "Written meter counts to ~A.~%" filename)))
    (utils:read-object-from-file filename)))

(defmethod count-meters ((training-set list) texture resolution
			 &key (per-composition? nil))
  "Find every occurring meter in a list of composition and count the number of bars in 
each meter (unless <per-composition?> is true and the texture is not :grid, in that case
the number of compositions per meter are counted and compositions are assumed not to 
contain metrical changes). Return an ALIST with counts indexed by meter-strings."
  (let ((meter-counts))
    (dolist (composition training-set meter-counts)
      (cond ((eq texture :grid)
	     (sequence:dosequence (event composition)
	       (when (not (or (null (md:barlength event)) (null (md:pulses event))))
		 (let* ((meter (md:make-metrical-interpretation event resolution))
			(meter-string (md:meter-string meter))
			(mcount (cdr (lookup-meter meter-string meter-counts)))
			(increment (/ 1 (md:meter-period meter)))) 
		   (if mcount ; use mcount as a check whether the key already exists
		       (rplacd (lookup-meter meter-string meter-counts) (+ mcount increment))
		       (setf meter-counts (acons meter-string increment meter-counts)))))))
	    ((member texture (list :melody :harmony))
	     (let ((last-event (utils:last-element composition)))
	       (let ((duration (+ (md:onset last-event)
				  (md:duration last-event)))
		     (meter (md:make-metrical-interpretation last-event nil)))
		 (let* ((meter-string (md:meter-string meter))
			(mcount (cdr (lookup-meter meter-string meter-counts)))
			(increment (if per-composition? 
				       1 (ceiling (/ duration (md:barlength meter))))))
		   (if mcount ; use mcount as a check whether the key already exists
		       (rplacd (lookup-meter meter-string meter-counts) (+ mcount increment))
		       (setf meter-counts (acons meter-string increment meter-counts)))))))))))

(defun initialise-prior-distribution (category-counts resolution)
  "Initialise a prior distribution over categories based on <category-counts>.
Rescale the distribution so the sum of prior category likelihoods in each possible
phase equals one. Return the rescaled distribution."
  (when *verbose* (format t "Generating prior distribution~%"))
  (let ((counts (mapcar #'cdr category-counts))
	(categories (mapcar #'(lambda (category-count) 
				(md:meter-string->metrical-interpretation
				 (car category-count) resolution))
			    category-counts)))
    (let ((observation-count (apply '+ counts)))
      (let ((category-prior (mapcar #'(lambda (count) (/ count observation-count)) counts))
	    (interpretations-per-category
	     (mapcar (lambda (c) (md:create-interpretations c resolution))
				     categories)))
	(let ((interpretation-prior 
	       (apply #'append (mapcar #'(lambda (interpretations p) 
			   (mapcar #'(lambda (interpretation) 
				       (cons (md:meter-string interpretation) (list p)))
				   interpretations))
		       interpretations-per-category category-prior))))
	  ;; Flatten and re-normalise the distribution
	  (prediction-sets:normalise-distribution interpretation-prior))))))

(defun composition-list-signature (composition-list)
  "Return the hexdigest of the MD5SUM of the list of all composition indices in the list."
  (let* ((identifiers (mapcar #'md:get-identifier composition-list))
	 (composition-indices (mapcar #'md:get-composition-index identifiers)))
    (utils:md5-sum-of-list composition-indices)))

(defun sum-over-time (distributions)
  "Take an alist where each key is a distribution parameter and the
corresponding value is a list of probabilities at a specific time. Sum 
the probabilities over time and divide by the list length."
  (let ((params (prediction-sets:distribution-symbols distributions))
	(results))
    (dolist (param params)
      (let* ((probabilities (lookup-key param distributions))
	     (result (/ (apply #'+ probabilities) (length probabilities))))
	(setf results (acons param result results))))
    results))

(defun phase-metre->metre (distribution)
  "Convert a distribution over metre and phase to a distribution
over metre."
  (flet ((remove-phase (param)
	   (multiple-value-bind (values) 
	       (read-from-string param)
	     (format nil "(~A ~A)" (first values) (second values)))))
    (let* ((params (prediction-sets:distribution-symbols distribution))
	   (metres 
	    (remove-duplicates (mapcar #'remove-phase params) 
			       :test #'equal))
	   (params-per-metre (loop for metre in metres collect 
				  (cons metre (loop for param in params 
						 when (equal (remove-phase param) metre)
						 collect param)))))
      (loop for metre-params in params-per-metre collect
	   (let ((metre (car metre-params))
		 (params (cdr metre-params)))
	     (let ((probability (apply #'+ (loop for param in params collect 
						(lookup-key param distribution)))))
	       (cons metre (/ probability (length params)))))))))
  
(defun lookup-key (key alist)
  (cdr (assoc key alist :test #'string-equal)))

(defun get-sequence-likelihoods (meter-likelihoods meter)
  (first (lookup-key meter meter-likelihoods)))

(defun get-event-likelihood (meter position likelihoods)
  (nth position (lookup-key meter likelihoods)))

(defun get-prior-likelihood (meter likelihoods)
  (lookup-key meter likelihoods))

(defun meter->time-signature (metrical-interpretation)
  (let ((phase (md:meter-phase metrical-interpretation))
	(pulses (md:pulses metrical-interpretation))
	(beat-division (md:beat-division metrical-interpretation)))
    (values pulses beat-division phase)))

(defun lookup-meter (meter counts)
  (assoc meter counts :test #'string-equal))
