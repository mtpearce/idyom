;; Attempt 1 (forgot what was wrong here)
(f:featurelet event
    ((pickup?
      (recursive () () (if previous-pickup? '(t nil) '(nil))
		 () () '(t nil)
		 (funcall (observe-pickup model) event)))
     (phase
      (recursive (pickup?) (numerator period pickup?)
		 (if pickup?
		     (cons 0 (remove-if (lambda (ph) (<= ph previous-phase))
					(phase-domain model)))
		     (phase-domain model))
		 () (numerator pickup?) '(0) ; always begin at zero
		 (let ((ioi (funcall (observe-ioi model) event))
		       (pickup? (funcall (observe-pickup model) event)))
		   (if (and previous-pickup? (not pickup?))
		       0
		       (phase-delta model (+ previous-phase ioi) period)))
		 (let ((ioi (funcall (observe-ioi model) event))
		       (pickup? (funcall (observe-pickup model) event)))
		   (if pickup?
		       (phase-delta model ioi period)
		       0))))))

;; Attempt 2

(f:featurelet event
    ((pickup-phase
	  (recursive () (period)
		     (if (or (null previous-pickup-phase)
			     (= 0 previous-pickup-phase))
			 (list nil)
			 (remove-if (lambda (p) (>= p previous-pickup-phase))
				    (phase-domain model)))
		     () (period)
		     (phase-domain model)
		     (if (or (null previous-pickup-phase)
			     (= 0 previous-pickup-phase))
			 nil
			 (let ((bioi (funcall (observe-ioi model) event)))
			   (- previous-pickup-phase (phase-delta model bioi period))))
		     (let ((pickup (funcall (observe-pickup model) event)))
		       (phase-to-pickup-phase model (phase-delta model pickup period)))))
	 (phase
	  (recursive () (period pickup-phase numerator)
		     (if (null pickup-phase)
			 (phase-domain model)
			 (list (- (phase-positions model) pickup-phase)))
		     () (period pickup-phase numerator)
		     (list (pickup-phase-to-phase model pickup-phase))
		     (if (null pickup-phase)
			 (let* ((ioi (funcall (observe-ioi model) event))
				(delta (phase-delta model ioi period)))
			   ;;(format t "IOI: ~A. Delta: ~A. Period ~A~%" ioi delta period)
			   (mod (+ previous-phase delta) period))
			 (let* ((ioi (funcall (observe-ioi model) event))
				(delta (phase-delta model ioi period)))
			   (+ previous-phase delta)))
		     (pickup-phase-to-phase model pickup-phase))))
      (values period numerator pickup-phase phase))))
