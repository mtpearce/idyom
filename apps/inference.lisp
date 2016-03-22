;;; Code for modelling perceptual inference using IDyOM

(cl:in-package #:inference)

(defparameter *verbose* nil)

; Directory for caching meter counts
(defparameter *counts-dir* 
  (ensure-directories-exist
   (merge-pathnames "data/counts/" (utils:ensure-directory utils:*root-dir*))))

(defun infer-meter (dataset-id target-viewpoints source-viewpoints test-sequence
		    &key (voices nil) (texture :grid) 
		      (resolution 16) (repetitions 1)
		      (python-results-file nil)
		      (use-cache? t))
  (let* ((training-set-promise
	  (promises:make-promise :function (md:get-music-objects 
				   (if (listp dataset-id) dataset-id (list dataset-id))	
				   nil 
				   :voices voices 
				   :texture texture 
				   :resolution resolution)
			:id dataset-id))
	 (sources (viewpoints:get-viewpoints source-viewpoints))
	 (targets (viewpoints:get-basic-viewpoints target-viewpoints 
						   training-set-promise
						   texture))
	 ;; Obtain event counts per meter
	 (meter-counts (count-meters training-set-promise resolution))
	 ;; Extract a list of meters
	 (meters (mapcar #'(lambda (meter-count) (md:meter-key->metrical-interpretation (car meter-count) resolution)) meter-counts))
	 ; Repeat the test sequence a specified number of times
	 (test-sequence (loop for x in (range repetitions) collecting (copy-list test-sequence)))
	 ; Concatenate sublists
	 (test-sequence (reduce #'append test-sequence))
	 ;; Generate a Nparams x Ntarget-viewpoints x Nevents matrix
	 ;; containing event-likelihoods
	 (likelihoods (generate-meter-predictions training-set-promise
							meters
							targets
							sources
							test-sequence
							:resolution resolution
							:voices voices
							:texture texture
							:use-cache? use-cache?))
	 ;; Initialize the prior distribution
	 (prior-distribution (initialise-prior-distribution meter-counts resolution))
	 ;; Convert to a Nparams x Nevents data structure where each column is 
	 ;; a probability distribution over params
	 (results (generate-meter-posterior prior-distribution likelihoods (length test-sequence))))
    (when python-results-file 
      (write-python-output prior-distribution 
			   likelihoods 
			   results 
			   resolution
			   python-results-file))
    results))


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
		  (acons (md:meter-key m) 
			 (list (prediction-sets:distribution-probabilities likelihoods))
			 meter-predictions))))))
    meter-predictions))

(defun count-meters-grid (training-set-promise resolution)
  "Count occurrences of each meter in a list of grid representations of compositions."
  (let ((filename (format nil "~A~A-~A" *counts-dir* (promises:get-identifier training-set-promise) resolution)))
    (unless (utils:file-exists filename)
      (let ((training-set (promises:retrieve training-set-promise))
	    (meter-counts)) ; The meter counts
	(dolist (composition training-set)
	  (sequence:dosequence (event composition)
	    (when (not (or (null (md:barlength event)) (null (md:pulses event))))
	      (let* ((meter (md:make-metrical-interpretation event resolution))
		     (meter-key (md:meter-key meter))
		     (mcount (cdr (lookup-meter meter-key meter-counts)))
		     (increment (/ 1 (md:meter-period meter)))) 
		(if mcount ; use mcount as a check whether the key already exists
		    (rplacd (lookup-meter meter-key meter-counts) (+ mcount increment))
		    (setf meter-counts (acons meter-key increment meter-counts)))))))
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
	      (let* ((meter-key (md:meter-key meter))
		     (mcount (cdr (lookup-meter meter-key meter-counts)))
		     (increment (if per-composition? 
				    1 (ceiling (/ duration (md:barlength meter))))))
		(if mcount ; use mcount as a check whether the key already exists
		    (rplacd (lookup-meter meter-key meter-counts) (+ mcount increment))
		    (setf meter-counts (acons meter-key increment meter-counts)))))))
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
	 (meter-keys (prediction-sets:distribution-symbols meter-counts))
	 (meters (mapcar #'(lambda (key) 
			     (md:meter-key->metrical-interpretation key resolution)) 
			 meter-keys))
	 ;; Obtain probability by dividing the count of each meter by the total count
	 (probabilities (mapcar #'(lambda (meter-and-count) 
				      (/ (cdr meter-and-count)
					 (apply '+ (mapcar #'cdr meter-counts)))) 
				meter-counts)))
    (dotimes (meter-index (length meter-keys))
      (let* ((meter (nth meter-index meters))
	     (probability (nth meter-index probabilities))
	     (period (md:meter-period meter)))
	(dotimes (phase period)
	  (setf (md:meter-phase meter) phase)
	  (setf unnormalised-prior
		(acons (md:meter-key meter) 
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

(defun key-label->python (key &optional (resolution 16))
    "Translate the keys used here into python tuples"
    (multiple-value-bind 
	  (beat division phase)
	(meter->time-signature (md:meter-key->metrical-interpretation key resolution))
      (format nil "((~D,~D), ~D)" beat division phase)))
	
(defun write-python-output (prior likelihoods results resolution path)
  (let ((prior-dict
	 (python:alist->dict prior
			:dict-name "prior"
			:key-format-fn #'key-label->python))
	(likelihoods-dict
	 (python:alist->dict likelihoods
			:dict-name "likelihoods"
			:key-format-fn #'key-label->python
			:value-format-fn (lambda (prediction-sets) (python:list->list (first prediction-sets)))))
	(results-dict 
	 (python:alist->dict results 
			:dict-name "posterior" 
			:key-format-fn (lambda (key) 
					 (multiple-value-bind 
					       (beat division phase)
					     (meter->time-signature (md:meter-key->metrical-interpretation key resolution))
					   (format nil "'~D ~D (phase ~D)'" beat division phase)))
			:value-format-fn (lambda (value)
					   (format nil "[~{~D, ~}]" value)))))
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (format stream "~A~%~A~%~A~%" prior-dict likelihoods-dict results-dict))))
