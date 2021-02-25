(cl:in-package #:generative-models)
(5am:def-suite models)
(5am:in-suite models)

(defmacro groups-test ((features parents order previous-state
				 &optional ppm-locations) result)
  `(5am:is (set-equal (mapcar #'elements
			      (generate-states (list ,@features) (list ,@parents)
					       ,order ,previous-state ,ppm-locations
					       model))
		      (mapcar #'vector ,result)
		      :test #'equalp)))

(defmacro elements-test ((features parents order previous-state
				   &optional ppm-locations) result)
  `(5am:is (set-equal (mapcar (lambda (gs) (state-elements gs model ,order))
			      (generate-states (list ,@features) (list ,@parents)
					       ,order ,previous-state ,ppm-locations
					       model))
		      (mapcar (lambda (l) (apply #'vector l))
			      ,result)
		      :test #'equalp)))

(5am:test fail (5am:is nil))

(5am:test generate-group-states
  (5am:with-fixture simple-delta-model ()
    (5am:is (equal (generate-group-states '(0) nil nil 0 model)
		   (list f-domain)))
    (5am:is (every #'utils:set-equal
		   (generate-group-states '(1) nil (vector 2) 1 model)
		   '((0 -1 -2))))
    (let ((df-state (make-instance 'group-state
				   :features '(1)
				   :elements '(1))))
      (5am:is (every #'utils:set-equal
		     (generate-group-states '(0) (list df-state) (vector 2) 1 model)
		     '((3))))))
  (5am:with-fixture simple-branch-model ()
    (5am:is (every #'utils:set-equal
		   (generate-group-states '(0) nil nil 0 model)
		   '((x y))))
    (let ((g-state (make-instance 'group-state
				  :features '(0)
				   :elements '(x))))
      (5am:is (every #'utils:set-equal
		     (generate-group-states '(1 2) (list g-state) nil 0 model)
		     '((a b) (a)))))
    (let ((g-state (make-instance 'group-state
				  :features '(0)
				  :elements '(y))))
      (5am:is (every #'utils:set-equal
		     (generate-group-states '(1 2) (list g-state) nil 0 model)
		     '((a) (a b)))))))

(5am:test generate-states
  (5am:with-fixture simple-delta-model ()
    ;; D = {0, 1, 2}
    ;; 0: f = f(f', df) = f' + df
    ;; 1: df = f(f') = {df in R | f' + df in D}
    ;; GENERATE-STATES
    (groups-test ((0) nil 0 nil) '(0 1 2))
    (groups-test ((1) nil 1 (vector 2)) '(0 -1 -2))
    (groups-test ((0) (1) 1 (vector 2)) '(0 1 2))
    (set-ppm-features model 'df)
    (setf (svref (ppm-models model) 1)
	  (ppm:build-model '((0 1 -1 2 0 -2)) '(0 1 2 -1 -2)))
    (let* ((ppm-locations (vector nil (ppm::get-root)))
	   (states (generate-states '(1) nil 1 (vector 0) ppm-locations model)))
;      asd
      (print (mapcar #'elements states))
      (print (mapcar #'probabilities states))))
  (5am:with-fixture simple-branch-model ()
    (groups-test ((0) nil 0 nil) '(x y))
    (elements-test ((1 2) (0) 0 nil)
		   '((x b a) (x a a) (y a a) (y a b)))
    (elements-test (() (1 2) 0 nil)
		   '((x a a) (x b a) (y a a) (y a b))))
  (5am:with-fixture simple-confluence-model ()
    (groups-test ((0) nil 0 nil) '(0 1 2))
    (groups-test ((1) nil 0 nil) '(10 100 1000))
    (groups-test ((0 1) (0) 0 nil)
		 '((2 1000) (2 100) (2 10) (1 1000) (1 100)
		   (1 10) (0 1000) (0 100) (0 10)))
    (elements-test ((2) (0 1) 0 nil)
		   '((0 10 10) (0 100 100) (0 1000 1000)
		     (1 10 11) (1 100 101) (1 1000 1001)
		     (2 10 12) (2 100 102) (2 1000 1002)))
    (elements-test (() (2) 0 nil)
		   '((0 10 10) (0 100 100) (0 1000 1000)
		     (1 10 11) (1 100 101) (1 1000 1001)
		     (2 10 12) (2 100 102) (2 1000 1002))))
  (5am:with-fixture simple-branch-model-2 ()
    (groups-test ((0) nil 0 nil) '(0 1 2))
    (elements-test ((1 2) (0) 0 nil)
		   '((0 0 0) (1 1 1) (2 2 2)))
    (elements-test (() (1 2) 0 nil)
		   '((0 0 0) (1 1 1) (2 2 2)))))


(5am:test marginalize )
(5am:test create-marginal-table )
(5am:test update-model-locations )
(5am:test generate-leaf-states )

(5am:test (model-sequence-1 :depends-on fail)
  (5am:with-fixture simple-delta-model ()
    (set-ppm-features model)
    (set-observables model)
    (multiple-value-bind (plausible-states evidence)
	(model-sequence '(0) model)
      ;; Evidence should be 1
      (5am:is (eq evidence 1))
      ;; There should be plausible states
      (5am:is (length plausible-states) 3)
      ;; They should have a joint probability of 1/3 ea.
      (5am:is (every (lambda (s) (equal (probability s) (/ 1 3)))
		     plausible-states)))
    (set-observables model 'f)
    (multiple-value-bind (plausible-states evidence)
	(model-sequence '(0) model)
      ;; Evidence should be 1/3
      (5am:is (equal evidence (/ 1 3)))
      ;; There should be plausible states
      (5am:is (length plausible-states) 1)
      ;; It should have a joint probability of 1/3 ea.
      (5am:is (every (lambda (s) (equal (probability s) (/ 1 3)))
		     plausible-states)))
    (multiple-value-bind (plausible-states evidence)
	(model-sequence '(0 1) model)
      ;; Evidence should be 1/3
      (5am:is (equal evidence (/ 1 9)))
      ;; There should be plausible states
      (5am:is (length plausible-states) 1)
      ;; It should have a joint probability of 1/9 ea.
      (5am:is (every (lambda (s) (equal (probability s) (/ 1 9)))
		     plausible-states)))))

(5am:test (model-sequence :depends-on nil)
  (5am:with-fixture simple-delta-model ()
    (set-ppm-features model 'df)
    (set-observables model 'f)
    (setf (svref (ppm-models model) 1)
	  (ppm:build-model '((0 1 -1 -2)) '(0 1 2 -1 -2)))
    (multiple-value-bind (plausible-states evidence)
	(model-sequence '(0 0 1 0) model)
      (multiple-value-bind (params table)
	  (create-marginal-table plausible-states '(0 1) 1 model)
	(format t "EVIDENCE: ~A~%" evidence)
	(let ((distribution (marginalize params table)))
	  (dolist (p params)
	    (let ((s (gethash p distribution)))
	      (format t "F: ~A~%" (trace-back s 'f model))
	      (format t "DF: ~A~%" (trace-back s 'df model))
	      (format t "probability: ~A~%" (probability s)))))))
    (set-observables model)
    (set-ppm-features model 'df)
    (setf (svref (ppm-models model) 0)
	  (ppm:make-ppm '(0 1 2)))
    (let ((plausible-states (model-sequence '(nil nil nil nil) model)))
      (multiple-value-bind (params table)
	  (create-marginal-table plausible-states '(1) 1 model
				 :extra-key #'ppm-branch)
	(let ((distribution (marginalize params table)))
	  (format t "Number of states ~A~%" (length params))
	  (dolist (p params)
	    (let ((s (gethash p distribution)))
	      (format t "F: ~A~%" (trace-back s 'f model))
	      (format t "DF: ~A~%" (trace-back s 'df model))
	      (format t "probability: ~A~%" (probability s)))))))))
  (5am:with-fixture simple-branch-model ()
    (set-ppm-features model)
    (set-observables model 'f)
    (setf (svref (ppm-models model) 1)
	  (ppm:build-model '((0 1 -1 2 0 -2)) '(0 1 2 -1 -2)))
    (let ((plausible-states (model-sequence '(0 0 1 0) model)))
      (multiple-value-bind (params table)
	  (create-marginal-table plausible-states '(0 1) 1 model)
	(let ((distribution (marginalize params table)))
	  (dolist (p params)
	    (let ((s (gethash p distribution)))
	      (format t "F: ~A~%" (trace-back s 'f model))
	      (format t "DF: ~A~%" (trace-back s 'df model))
	      (format t "probability: ~A~%" (probability s))))))))
