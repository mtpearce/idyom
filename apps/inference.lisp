;;; Code for modelling perceptual inference using IDyOM

(cl:in-package #:inference)

(defparameter *counts-dir* 
  (ensure-directories-exist
   (merge-pathnames "data/counts/" (utils:ensure-directory apps:*root-dir*))))

(defun infer-meter (dataset-id target-viewpoints source-viewpoints test-sequence
		    &key (voices nil) (texture :grid) 
		      (resolution 16) (repetitions 1)
		      (write-to-python? nil)
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
	 (targets (viewpoints:get-basic-viewpoints target-viewpoints training-set-promise))
	 ;; Obtain event counts per meter
	 (meter-counts (count-meters training-set-promise resolution))
	 ;; Extract a list of meters
	 (meters (mapcar #'(lambda (meter-count) (md:meter-key->metrical-interpretation (car meter-count) resolution)) meter-counts))
	 (test-sequence (loop for x in (range repetitions) collecting (copy-list test-sequence)))
	 (test-sequence (reduce #'append test-sequence))
	 ;; Generate a Nparams x Ntarget-viewpoints x Nevents datastructure
	 ;; containing event-predictions
	 (meter-predictions (generate-meter-predictions training-set-promise
							meters
							targets
							sources
							test-sequence
							:resolution resolution
							:voices voices
							:texture texture
							:use-cache? use-cache?))
	 ;; Extract the likelihoods
	 (likelihoods (meter-predictions->probabilities meter-predictions))
	 ;; Initialize the prior distribution
	 (prior-distribution (initialise-prior-distribution meter-counts resolution))
	 ;; Convert to a Nparams x Nevents data structure where each column is 
	 ;; a probability distribution over params
	 (posterior-per-position (generate-meter-posterior prior-distribution likelihoods (length test-sequence)))
	 (posterior (sum-over-time posterior-per-position)))
    (when write-to-python? (write-python-output posterior-per-position resolution))
    (sort posterior #'> :key (lambda (x) (cdr x)))))

(defun generate-meter-posterior (prior-distribution likelihoods n) 
  (format t "Performing Bayesian inference using predictions and the prior~%")
  (let ((params (mapcar #'car prior-distribution))
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
  (format t "Generating predictions for the test sequence in all interpretations~%")
  (let* ((meter-predictions))
    ;; Train models for each meter
    (dolist (meter meters meter-predictions)
      (multiple-value-bind (beats division) (meter->time-signature meter)
	(format t "Training interpretation model for ~D ~D~%" beats division))
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
	(format t "Modelling test sequence for all phases~%")
	(dotimes (phase period)
	  (setf (md:meter-phase meter) phase)
	  (let ((predictions
		 (mvs:model-sequence mvs (coerce test-sequence 'list) :construct? nil :predict? t :interpretation meter)))
	    (setf meter-predictions 
		  (acons (md:meter-key meter) predictions meter-predictions))))))
    meter-predictions))

(defun count-meters (training-set-promise resolution)
  "Take a training set consisting of music-sequences. Go over each event,
note it's meter and add it to an alist
 of counts."
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
		     (increment (/ (or mcount 1) (md:meter-period meter))))
		(if mcount ; use mcount as a check whether the key already exists
		(rplacd (lookup-meter meter-key meter-counts) (+ mcount increment))
		(setf meter-counts (acons meter-key increment meter-counts)))))))
	(utils:write-object-to-file meter-counts filename)
	(format t "Written meter counts to ~A.~%" filename)))
    (utils:read-object-from-file filename)))

(defun initialise-prior-distribution (meter-counts resolution)
  "Generate a probability distribution using the counts in <meter-counts>. 
An extra variable, phase, is added to the resulting distribution, which has a 
flat distribution. Different metres have different amounts of phases."
  (format t "Generating prior distribution~%")
  (let* ((prior)
	 (unnormalised-prior)
	 (meter-keys (mapcar #'car meter-counts))
	 (meters (mapcar #'(lambda (key) (md:meter-key->metrical-interpretation key resolution)) meter-keys))
	 (probabilities (mapcar #'(lambda (meter-and-count) 
				      (/ (cdr meter-and-count)
					 (apply '+ (mapcar #'cdr meter-counts)))) meter-counts))) ; Divide the count of each meter by the sum of the counts
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
      (dolist (meter (mapcar #'car unnormalised-prior))
	(let ((unnormalised-probability (cdr (assoc meter unnormalised-prior))))
	  (setf prior 
		(acons meter
		       (* unnormalised-probability normalisation-factor)
		       prior))
	  prior))
      prior)))

(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))
       
(defun cumsum (l)
  (let ((cs))
    (dolist (item l)
      (push (+ item (or (first cs) 0)) cs))
    (nreverse cs)))

(defun sum-over-time (distributions)
  "Take an alist where each key is a distribution parameter and the
corresponding value is a list of probabilities at a specific time. Sum 
the probabilities over time and divide by the list length."
  (let ((params (mapcar #'car distributions))
	(results))
    (dolist (param params)
      (let* ((probabilities (lookup-key param distributions))
	     (result (/ (apply #'+ probabilities) (length probabilities))))
	(setf results (acons param result results))))
    results))
  
(defun lookup-key (key alist)
  (cdr (assoc key alist :test #'string-equal)))

(defun get-sequence-likelihoods (meter-likelihoods meter)
  (first (lookup-key meter meter-likelihoods)))

(defun get-event-likelihood (meter position likelihoods)
  (nth position (first (lookup-key meter likelihoods))))

(defun get-prior-likelihood (meter likelihoods)
  (lookup-key meter likelihoods))
	  
(defun meter-predictions->information-contents (meter-predictions)
  (let ((interpretations (mapcar #'car meter-predictions))
	(output))
    (dolist (meter interpretations) ; FOR EACH: meter
      (let ((sequence-predictions (lookup-key meter meter-predictions))
	    (sequence-ics))
	(dolist (predictions sequence-predictions) ; FOR EACH: viewpoint prediction
	  (let* ((event-predictions (prediction-sets:prediction-set predictions))
		 (probabilities (mapcar #'probability event-predictions))
		 (viewpoint-ics (mapcar #'(lambda (p) (- (log p 2))) probabilities)))
	    (push viewpoint-ics sequence-ics)))
	(setf output (acons meter (nreverse sequence-ics) output))))
    output))

(defun meter-predictions->probabilities (meter-predictions)
  (let ((interpretations (mapcar #'car meter-predictions))
	(output))
    (dolist (meter interpretations) ; FOR EACH: meter
      (let ((sequence-predictions (cdr (assoc meter meter-predictions :test #'string-equal)))
	    (sequence-probabilities))
	(dolist (predictions sequence-predictions) ; FOR EACH: viewpoint prediction
	  (let* ((event-predictions (prediction-sets:prediction-set predictions))
		 (probabilities (mapcar #'probability event-predictions)))
	    (push probabilities sequence-probabilities)))
	(setf output (acons meter (nreverse sequence-probabilities) output))))
    output))

(defun ioi-list->grid-events (ioi-list &key 
					 (source-resolution 8) 
					 (target-resolution 16) 
					 (timebase 96))
  (setf ioi-list (mapcar #'(lambda (ioi) (md:rescale ioi target-resolution source-resolution)) ioi-list))
  (let ((onset-times (cons 0 (cumsum ioi-list))))
    (loop for position in (range (car (last onset-times)))
	 collecting (make-instance 'md::grid-event
			    :isonset (if (member position onset-times) t nil)
			    :pos position
			    :cpitch nil
			    :onset (md:rescale position timebase target-resolution)
			    :duration (/ timebase target-resolution)
			    :barlength nil
			    :pulses nil
			    :id (md:make-event-id 0 0 0)
			    :timebase timebase))))

(defun grid-events->latex-solution-array (grid viewpoint-list &key (interpretation nil) (highlight 0))
  (let* ((viewpoints (viewpoints:get-viewpoints viewpoint-list))
	 (m (mvs:make-mvs nil viewpoints nil))
	 (isonset-vp (viewpoints:get-viewpoint 'isonset))
	 (pos-vp (viewpoints:get-viewpoint 'pos)))
    (format t "\\begin{tabular}{~{~D~}}~%" (mapcar (lambda (x) "l") (range (length grid))))
    (dolist (event grid)
      (let ((position (funcall (type-of pos-vp) (list event)))
	    (isonset (funcall (type-of isonset-vp) (list event))))
	(when (< position highlight) (format t "\\textcolor{red}{"))
	(if isonset (format t "$\\bullet$") (format t "$\\circ$"))
	(when (< position highlight) (format t "}"))
	(when (< position (- (length grid) 1)) (format t " & "))))
    (format t "\\\\~%")
    (dotimes (vp-index (length viewpoints))
      (dolist (event grid)
	(let ((position (funcall (type-of pos-vp) (list event))))
	  (when (< position highlight) (format t "\\textcolor{red}{"))
	  (format t "~D" (aref (mvs:get-event-array m (list event) :interpretation interpretation)
			       vp-index))
	  (when (< position highlight) (format t "}"))
	  (when (< position (- (length grid) 1)) (format t " & "))))
      (format t "\\\\~%"))
    (format t "\\end{tabular}")))

(defun meter->time-signature (metrical-interpretation)
  (let* ((barlength (md:barlength metrical-interpretation))
	 (pulses (md:pulses metrical-interpretation))
	 (timebase (md:timebase metrical-interpretation))
	 (phase (md:meter-phase metrical-interpretation))
	 (beat-duration (/ barlength pulses))
	 (beat-divisor (/ timebase beat-duration)))
    (values pulses beat-divisor phase)))

(defun lookup-meter (meter counts)
  (assoc meter counts :test #'string-equal))
	
(defun probability (event-prediction) 
  (cadr (prediction-sets:event-prediction event-prediction)))

(defun write-python-output (distributions resolution)
  (with-open-file (stream "/home/bastiaan/Projects/exposure/exposure/idyom_output.py" :direction :output :if-exists :supersede)
    (format stream "posterior = {")
    (let ((params (mapcar #'car distributions)))
      (dolist (param params)
	(multiple-value-bind (beat division phase)
	    (meter->time-signature (md:meter-key->metrical-interpretation param resolution))
	  (format stream "'~D ~D (phase ~D)':[" beat division phase))
	(format stream "~{~D,~}" (cdr (assoc param distributions :test #'string-equal)))
	(format stream "],")))
    (format stream "}")))

;; Some rhythms convenient for testing
(defvar agbekor (ioi-list->grid-events '(2 2 1 2 2 2 1) :target-resolution 16))
(defvar shave-and-a-haircut (ioi-list->grid-events '(2 1 1 2 4 2 4) :target-resolution 16))
