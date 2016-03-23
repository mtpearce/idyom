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
	   (meter-counts (cond ((eq texture :grid)
				(count-meters-grid training-set resolution))
			       ((member texture (list :melody :harmony)) 
				(count-meters training-set :use-cache? nil))))
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
	   (posteriors (generate-meter-posterior prior-distribution likelihoods 
					      (length test-sequence)))
	   (information-contents 
	    (loop for p below (length test-sequence) collecting
		 (- (log (event-likelihood p likelihoods posteriors)
			 2)))))
      (values prior-distribution likelihoods posteriors information-contents)))

(defun event-likelihood (position likelihoods posteriors)
  (let ((interpretations (prediction-sets:distribution-symbols posteriors)))
    (apply '+ (loop for interpretation in interpretations collecting
		   (* (nth position (first (lookup-key interpretation likelihoods)))
		      (nth position (lookup-key interpretation posteriors)))))))

(defun generate-meter-posterior (prior-distribution likelihoods n) 
  (when *verbose* (format t "Performing Bayesian inference using predictions and the prior~%"))
  (let ((params (prediction-sets:distribution-symbols prior-distribution))
	(results))
    ;; Iterate over positions in the test sequence to infer meter
    (dotimes (position n)
      (let ((evidence (apply #'+ 
			     (mapcar #'(lambda (m) 
					 (* (get-event-likelihood m position likelihoods)
					    (get-prior-likelihood m prior-distribution))) 
				     params))))
	(dolist (meter params)
	  (let* ((likelihood (get-event-likelihood meter position likelihoods))
		 (prior (get-prior-likelihood meter prior-distribution))
		 (posterior (/ (* likelihood prior) evidence))
		 (probabilities (assoc meter results :test #'string-equal)))
	    ; Update the prior
	    (rplacd (assoc meter prior-distribution :test #'string-equal) posterior)
	    ; Store the result
	    (if probabilities 
		(push posterior (cdr probabilities))
		(setf results (acons meter (list posterior) results)))))))
    (dolist (param params)
      (let ((result (assoc param results :test #'string-equal)))
	(rplacd result (nreverse (cdr result)))))
    results))

(defun generate-meter-predictions (training-set-promise meters targets sources test-sequence
		    &key (resolution 16) voices texture use-cache?)
  (when *verbose* (format t "Generating predictions for the test sequence in all interpretations~%"))
  (let* ((meter-predictions))
    ;; Train models for each meter
    (dolist (meter meters meter-predictions)
      (multiple-value-bind (beats division) (meter->time-signature meter)
	(when *verbose* (format t "Training interpretation model for ~D ~D~%" beats division)))
      (let* ((interpretation-models ; the viewpoint models
	      (resampling:get-long-term-models sources training-set-promise
					       nil
					       (promises:get-identifier training-set-promise) 
					       nil nil
					       :voices voices
					       :texture texture
					       :interpretation meter
					       :resolution resolution
					       :use-cache? use-cache?))
	     (mvs (mvs:make-mvs targets sources interpretation-models)) 
	     (period (md:meter-period meter)))
	 ; Generate predictions for each phase
	(when *verbose* (format t "Modelling test sequence for all phases~%"))
	(dotimes (phase period)
	  (let* ((m (md:make-metrical-interpretation meter resolution :phase phase))
		 (predictions
		  (mvs:model-sequence mvs (coerce test-sequence 'list) texture :construct? nil :predict? t :interpretation m))
		 (likelihoods (prediction-sets:event-predictions (first predictions))))
	    (setf meter-predictions 
		  (acons (md:meter-string m) 
			 (list (prediction-sets:distribution-probabilities likelihoods))
			 meter-predictions))))))
    meter-predictions))

(defun count-meters-grid (training-set-promise resolution)
  "Count occurrences of each meter in a list of grid representations of compositions.
Return an ALIST with counts indexed by meter-strings."
  (let ((filename (format nil "~A~A-~A" *counts-dir* (promises:get-identifier training-set-promise) resolution)))
    (unless (utils:file-exists filename)
      (let ((training-set (promises:retrieve training-set-promise))
	    (meter-counts)) ; The meter counts
	(dolist (composition training-set)
	  (sequence:dosequence (event composition)
	    (when (not (or (null (md:barlength event)) (null (md:pulses event))))
	      (let* ((meter (md:make-metrical-interpretation event resolution))
		     (meter-string (md:meter-string meter))
		     (mcount (cdr (lookup-meter meter-string meter-counts)))
		     (increment (/ 1 (md:meter-period meter)))) 
		(if mcount ; use mcount as a check whether the key already exists
		    (rplacd (lookup-meter meter-string meter-counts) (+ mcount increment))
		    (setf meter-counts (acons meter-string increment meter-counts)))))))
	(utils:write-object-to-file meter-counts filename)
	(when *verbose* (format t "Written meter counts to ~A.~%" filename))))
    (utils:read-object-from-file filename)))

(defun count-meters (training-set-promise &key (use-cache? t) (per-composition? nil))
  "This function assumes the meter does not change during event sequences. Go over 
eac sequence in the training set, calculate the number meter-observations by taking the 
ceiling of the onset time of the last event over the meter's period."
    (let ((filename (format nil "~A~A" *counts-dir* (promises:get-identifier training-set-promise))))
    (unless (and (utils:file-exists filename) use-cache?)
      (let ((training-set (promises:retrieve training-set-promise))
	    (meter-counts))
	(dolist (composition training-set)
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
		    (setf meter-counts (acons meter-string increment meter-counts)))))))
	(utils:write-object-to-file meter-counts filename)
	(when *verbose* (format t "Written meter counts to ~A.~%" filename))))
    (utils:read-object-from-file filename)))

(defun initialise-prior-distribution (meter-counts resolution)
  "Initialise a prior distribution over meter based on <meter-counts>.
Rescale the distribution so the sum of prior meter likelihoods in each possible
phase equals one. Return the rescaled distribution."
  (when *verbose* (format t "Generating prior distribution~%"))
  (let* ((prior)
	 (unnormalised-prior)
	 (meter-strings (prediction-sets:distribution-symbols meter-counts))
	 (meters (mapcar #'(lambda (key) 
			     (md:meter-string->metrical-interpretation key resolution)) 
			 meter-strings))
	 ;; Obtain probability by dividing the count of each meter by the total count
	 (probabilities (mapcar #'(lambda (meter-and-count) 
				      (/ (cdr meter-and-count)
					 (apply '+ (mapcar #'cdr meter-counts)))) 
				meter-counts)))
    (dotimes (meter-index (length meter-strings))
      (let* ((meter (nth meter-index meters))
	     (probability (nth meter-index probabilities))
	     (period (md:meter-period meter)))
	(dotimes (phase period)
	  (setf (md:meter-phase meter) phase)
	  (setf unnormalised-prior
		(acons (md:meter-string meter) 
		       probability 
		       unnormalised-prior)))))
    ;; Normalise
    (let ((normalisation-factor (/ 1 (apply '+ (mapcar #'cdr unnormalised-prior)))))
      (dolist (meter (prediction-sets:distribution-symbols unnormalised-prior))
	(let ((unnormalised-probability (cdr (assoc meter unnormalised-prior))))
	  (setf prior 
		(acons meter
		       (* unnormalised-probability normalisation-factor)
		       prior))
	  prior))
      prior)))

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
  (nth position (first (lookup-key meter likelihoods))))

(defun get-prior-likelihood (meter likelihoods)
  (lookup-key meter likelihoods))

(defun meter->time-signature (metrical-interpretation)
  (let ((phase (md:meter-phase metrical-interpretation))
	(pulses (md:pulses metrical-interpretation))
	(beat-division (md:beat-division metrical-interpretation)))
    (values pulses beat-division phase)))

(defun lookup-meter (meter counts)
  (assoc meter counts :test #'string-equal))
