;;; Code for modelling perceptual inference using IDyOM

(cl:in-package #:inference)

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

(defun key-label->python (key &optional (resolution 16))
    "Translate the keys used here to the keys used in the python implementation"
    (multiple-value-bind 
	  (beat division phase)
	(meter->time-signature (md:meter-key->metrical-interpretation key resolution))
      (format nil "((~D,~D), ~D)" beat division phase)))

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
	  (let* ((m (md:make-metrical-interpretation meter resolution :phase phase))
		 (predictions
		  (mvs:model-sequence mvs (coerce test-sequence 'list) texture :construct? nil :predict? t :interpretation m))
		 (likelihoods (prediction-sets:event-predictions (first predictions))))
	    (setf meter-predictions 
		  (acons (md:meter-key m) 
			 (list (mapcar 'cadr likelihoods))
			 meter-predictions))))))
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
		     (increment (/ 1 (md:meter-period meter))))
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
	  
(defun grid->grid-events (grid &key (resolution 16)
				 (interpretation nil)
				 (timebase 96))
  (loop for is-onset in grid for position to (length grid)
     collecting (make-grid-event (if (eql is-onset 1) t nil)
				 position 
				 :interpretation interpretation
				 :resolution resolution
				 :timebase timebase)))

(defun ioi-list->grid-events (ioi-list &key 
					 (source-resolution 8) 
					 (target-resolution 16) 
					 (timebase 96)
					 (interpretation nil)
					 (phase 0))
  (setf ioi-list (mapcar #'(lambda (ioi) (md:rescale ioi target-resolution source-resolution)) ioi-list))
  (let ((onset-times (cons 0 (cumsum ioi-list))))
    (loop for position in (range (+ phase (car (last onset-times))))
	 collecting (make-grid-event (if (member (- position phase) onset-times) t nil) 
				     position 
				     :interpretation interpretation
				     :resolution target-resolution
				     :timebase timebase))))
    
(defun make-grid-event (is-onset position &key
					   (timebase 96)
					   (resolution 16) 
					   (interpretation nil))
  (make-instance 'md::grid-event
		 :is-onset is-onset
		 :pos position
		 :cpitch nil
		 :onset (md:rescale position timebase resolution)
		 :duration (/ timebase resolution)
		 :barlength (when interpretation
			      (md:barlength interpretation))
		 :pulses (when interpretation
			   (md:pulses interpretation))
		 :id (md:make-event-id 0 0 0)
		 :timebase timebase))


(defun grid-events->latex-solution-array (grid viewpoint-list &key (interpretation nil) (highlight 0))
  (let* ((viewpoints (viewpoints:get-viewpoints viewpoint-list))
	 (m (mvs:make-mvs nil viewpoints nil))
	 (is-onset-vp (viewpoints:get-viewpoint 'is-onset))
	 (pos-vp (viewpoints:get-viewpoint 'pos)))
    (format t "\\begin{tabular}{~{~D~}}~%" (mapcar (lambda (x) "l") (range (length grid))))
    (dolist (event grid)
      (let ((position (funcall (type-of pos-vp) (list event)))
	    (is-onset (funcall (type-of is-onset-vp) (list event))))
	(when (< position highlight) (format t "\\textcolor{red}{"))
	(if is-onset (format t "$\\bullet$") (format t "$\\circ$"))
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
  (let ((phase (md:meter-phase metrical-interpretation))
	(pulses (md:pulses metrical-interpretation))
	(beat-division (md:beat-division metrical-interpretation)))
    (values pulses beat-division phase)))

(defun lookup-meter (meter counts)
  (assoc meter counts :test #'string-equal))
	
(defun write-python-output (prior likelihoods results resolution path)
  (let ((prior-dict
	 (alist->pydict prior
			:dict-name "prior"
			:key-format-fn #'key-label->python))
	(likelihoods-dict
	 (alist->pydict likelihoods
			:dict-name "likelihoods"
			:key-format-fn #'key-label->python
			:value-format-fn (lambda (predictions-sets) (list->pylist (first predictions-sets)))))
	(results-dict 
	 (alist->pydict results 
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

(defun alist->pydict (alist &key (file nil) (dict-name "d") (key-format-fn (lambda (x) x))
			      (value-format-fn (lambda (x) x)))
  (let ((pydict (format nil "~A = {~A}" dict-name
			(alist->pydict-items alist key-format-fn value-format-fn))))
    (when file (with-open-file (stream file :direction :output :if-exists :supersede)
		 (format stream pydict)))
    pydict))

(defun alist->pydict-items (alist key-format-fn value-format-fn)
  (let* ((item (pop alist))
	 (other-items 
	  (when alist (alist->pydict-items alist key-format-fn value-format-fn)))
	 (key (car item))
	 (value (cdr item)))
    (format nil "~A:~A, ~A" 
	    (funcall key-format-fn key) 
	    (funcall value-format-fn value)
	    other-items)))
			  

(defun alist-value-map (alist fn)
  (let* ((item (pop alist))
	 (key (car item))
	 (value (cdr item)))
    (acons (funcall fn key) value (alist-value-map alist fn))))

(defun list->pylist (list)
  (format nil "[~{~D, ~}]" list))

;; Some rhythms convenient for testing
(defparameter agbekor (ioi-list->grid-events '(2 2 1 2 2 2 1) :target-resolution 16))
(defparameter shave-and-a-haircut (ioi-list->grid-events '(2 1 1 2 4 2 4) :target-resolution 16))
(defparameter son-clave (ioi-list->grid-events '(3 3 4 2 2) :target-resolution 16))

