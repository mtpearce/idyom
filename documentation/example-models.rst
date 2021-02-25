Generative music models
=======================


  (let* ((multiplier (/ timebase resolution))
	 (meter-viewpoint (viewpoints:get-viewpoint '(barlength pulses)))
	 (iois (remove-if (lambda (x) (eq 0 x))
			  (viewpoints::unique-elements (viewpoints:get-viewpoint 'bioi)
						       dataset)))
	 (meters (viewpoints::unique-elements meter-viewpoint dataset)))
    (labels ((phases (previous-phase period &optional (iois iois))
	       (loop for ioi in iois collect
		    (multiple-value-bind (bardist phase)
			(truncate (+ previous-phase ioi) period)
		      (list bardist phase))))
	     (get-phase (onset phase-0 barlength)
	       (mod (- onset phase-0) barlength))
	     (onset (previous-onset phase-0 bardist period phase)
	       (let ((previous-phase (get-phase previous-onset phase-0 period)))
		 (+ previous-onset
		    (- period previous-phase)
		    (* (1- bardist) period)
		    phase))))
      (f:featurelet 
	  ((phase-0 (recursive () () (list previous-phase-0)
			       () (meter)
			       (print (loop for i below (/ (car meter) multiplier) collect
					   (* i multiplier)))))
	   (meter (recursive () () (list previous-meter)
			     () () meters))
	   (phase (recursive () (meter) (phases previous-phase (car meter))
			     () (meter) (phases 0 (car meter) (cons 0 iois))))
	   (onset (recursive () (phase-0 phase meter)
			     (list (onset previous-onset phase-0
					  (car phase) (car meter) (cadr phase)))
			     () (phase-0 phase meter)
			     (list (onset 0 phase-0
					  (car phase) (car meter) (cadr phase))))))

Clone of old model (only phase prior is calculated differently, can be solved with custom model)

  (let* ((multiplier (/ timebase resolution))
	 (meter-viewpoint (viewpoints:get-viewpoint '(barlength pulses)))
	 (iois (remove-if (lambda (x) (eq 0 x))
			  (viewpoints::unique-elements (viewpoints:get-viewpoint 'bioi)
						       dataset)))
	 (meters (viewpoints::unique-elements meter-viewpoint dataset)))
    (flet ((phases (barlength)
	     (loop for phase below (/ barlength multiplier)
		collect (* phase multiplier)))
	   (next-onsets (onset phase-0 phase meter &optional (iois iois))
	     (let* ((barlength (car meter)))
	       (loop for ioi in iois
		  if (eq (* (mod (+ onset ioi phase-0) barlength)) phase)
		  collect (+ onset ioi)))))
      (f:featurelet 
	  ((phase-0 (recursive () () (list previous-phase-0)
			       () (meter) (phases (car meter))))
	   (meter (recursive () () (list previous-meter)
			     () () meters))
	   (phase (normal () (meter) (print (phases (car meter)))))
	   (onset (recursive () (phase-0 phase meter) 
			     (next-onsets previous-onset phase-0 phase meter)
			     () (phase-0 phase meter)
			     (next-onsets 0 phase-0 phase meter (cons 0 iois)))))

IOI model with phase space

  (let* ((meter-viewpoint (viewpoints:get-viewpoint 'pulses))
	 (iois (remove-if (lambda (x) (eq 0 x))
			  (viewpoints::unique-elements (viewpoints:get-viewpoint 'bioi)
						       dataset)))
	 (periods '(2 3))
	 (phases (apply #'lcm periods))
	 (meters (viewpoints::unique-elements meter-viewpoint dataset)))
    (f:featurelet 
	((period (recursive () () (list previous-period)
			    () () periods))
	 (meter (recursive () () (list previous-meter)
			   () () meters))
	 (phase (normal () (meter)
			phases))
	 (ioi (normal (phase) (phase period)
		      (loop for ioi in iois
			 if (eq (mod (+ previous-phase
					(* (/ ioi period) phases))
				     period)
				phase)
			 collect ioi))))

Super duper fancy model

  (let* ((meter-viewpoint (viewpoints:get-viewpoint 'pulses))
	 (period-viewpoint (viewpoints:get-viewpoint 'barlength))
	 (iois (remove-if (lambda (x) (eq 0 x))
			  (viewpoints::unique-elements (viewpoints:get-viewpoint 'bioi)
						       dataset)))
	 (periods (print (viewpoints::unique-elements period-viewpoint dataset)))
	 (phases (print (apply #'lcm periods)))
	 (meters (viewpoints::unique-elements meter-viewpoint dataset))
	 (pickup-observation
	  (lambda (e) (if (and (eq (md:description e) 'has-pickup)
			       (< (md:onset e) (md:barlength e)))
			  t nil))))
    (f:featurelet 
	((period (recursive () () (list previous-period)
			    () () periods))
	 (meter (recursive () () (list previous-meter)
			   () () meters))
	 (pickup? (recursive () () (if previous-pickup?
				       '(t nil)
				       '(nil))
			     () () '(t nil)))
	 (phase (recursive () (meter pickup?)
			   (if pickup?
			       (loop for phase below phases
				  if (> phase previous-phase)
				  collect phase)
			       (loop for phase below phases collect phase))
			   () (meter pickup?) '(0)))
	 (ioi (normal (pickup? phase) (pickup? phase period)
		      (if previous-pickup?
			  (if pickup?
			      (list (* (/ (- phase previous-phase) phases) period))
			      (when (eq phase 0)
				(loop for ioi in iois
				   if (<= (+ previous-phase
					     (* (/ ioi period) phases))
					  phases)
				   collect ioi)))
			  (loop for ioi in iois
			     if (eq (mod (+ previous-phase (* (/ ioi period) phases))
					 period)
				    phase)
			     collect ioi)))))
