;(cl:in-package #:idyom)

(5am:def-suite inference)
(5am:in-suite inference)

(defparameter timebase 16)
(defparameter midc 60)
(defparameter test-iois '(4 2 2 4 8 4 8))
(defparameter test-iois-compound '(6 2 2 2 6 12 6 12))

(defun iois->biois (iois &optional (offset 0))
  (append (list offset) (subseq iois 0 (1- (length iois)))))

(defun onset-event (onset bioi duration &optional (barlength nil) (pulses nil))
  (make-instance 'md:music-event
		 :voice 0 :vertint12 0 :articulation nil
		 :comma nil :ornament nil :dyn nil
		 :accidental 0 :mpitch 40 :cpitch 80
		 :deltast 0 :bioi bioi :midc midc
		 :timebase timebase :onset onset
		 :duration duration :mode 0 :keysig 0
		 :pulses pulses :barlength barlength
		 :tempo nil :phrase nil))

(defun iois->onset-events (iois &optional barlength pulses)
  (let* ((biois (iois->biois iois))
	 (onsets (apply #'utils:cumsum biois))
	 (durations iois))
    (mapcar #'(lambda (onset bioi duration)
		(onset-event onset bioi duration barlength pulses))
	    onsets biois durations)))

(defmacro mock-mel-seq (events &body body)
  `(let ((mel-seq (make-instance 'md:melodic-sequence
				 :%data ,events)))
     ,@body))

(defun crotchets-dur (crotchets)
  (* (/ 1 4) crotchets timebase))

;(5am:test marginal-likelihood
;  (let ((prior '(0.0001 0.9999))  ; (have-disease not-have-disease)
;	(likelihood '(0.99 0.1))) ; (test-positive|disease test-positive|not-have-disease)
;    (5am:is (eql (idyom::marginal-likelihood prior likelihood)
;		 (+ (* 0.0001 0.99) (* 0.9999 0.1))))))
	  
;(5am:test (infer-posterior :depends-on marginal-likelihood)
;  (let ((prior '(0.0001 0.9999)) 
;	(likelihood '(0.99 0.1)))
;    (let ((evidence (idyom::marginal-likelihood prior likelihood)))
;      (5am:is (equal (infer-posterior evidence prior likelihood)
;		     (list (/ (* 0.0001 0.99) evidence)
;			   (/ (* 0.9999 0.1) evidence)))))))

(5am:test partition-composition
  (mock-mel-seq (append (iois->onset-events test-iois 16 4)
			(iois->onset-events test-iois-compound 12 2)
			(iois->onset-events test-iois-compound 16 4))
    (multiple-value-bind (categories partitioned-dataset)
	(partition-composition () (coerce mel-seq 'list) (make-instance 'lv::metre))
      ;; All categories appear in categories
      ;; All excerpts in each segment are associated with one category only
      ;; All excerpts in the dataset appear in segmented-composition
      (print categories)
      (print partitioned-dataset))))
