(cl:in-package #:tests)

(defmacro with-mock-dataset (&rest body)
  `(let ((dataset (list (create-composition (loop repeat 16 collect 1)
					    (loop repeat 16 collect 0)
					    :timebase 4 :barlength 2 :pulses 2)
			(create-composition (loop repeat 10 collect 1)
					    (loop repeat 10 collect 0)
					    :timebase 4 :barlength 4 :pulses 2)
			(create-composition (loop repeat 18 collect 1)
					    (loop repeat 18 collect 0)
					    :timebase 4 :barlength 3 :pulses 3)
			(create-composition (loop repeat 15 collect 1)
					    (loop repeat 15 collect 0)
					    :timebase 4 :barlength nil :pulses nil)
			(create-composition (loop repeat 12 collect 1)
					    (loop repeat 12 collect 0)
					    :timebase 4 :barlength 2 :pulses 2))))
     ,@body))

(def-suite inference)
(in-suite inference)

(test lookup-key
  (is-true 
   (let ((key "test")
	 (alist (list (cons "test" 5) (cons 'test 4))))
     (eql (inference::lookup-key key alist) 5))))

(test sum-over-time
  (let* ((distributions '(("1" 0.2 0.4 0.8)
			  ("2" 0.5 0.2 0.05)
			  ("3" 0.3 0.4 0.15)))
	 (summed (inference::sum-over-time distributions)))
    (is (< 0.999 (prediction-sets::sum-distribution summed) 1.001))
    (is (alist-eql summed
		       `(("1" ,(utils:average 0.2 0.4 0.8))
			 ("2" ,(utils:average 0.5 0.2 0.05))
			 ("3" ,(utils:average 0.3 0.4 0.15)))
		       :test #'equal
		       :alist-test #'string-equal))))

(test get-posterior
  "Test a function that obtains the posterior distribution at a particular position"
  (let ((distributions '(("1" 0.2 0.4 0.8)
			 ("2" 0.5 0.2 0.05)
			 ("3" 0.3 0.4 0.15))))
    (is-true
     (alist-eql (inference::get-posterior distributions 0)
		'(("1" 0.2)
		  ("2" 0.5)
		  ("3" 0.3))
		:test #'equal
		:alist-test #'string-equal)
     "Sum over time did not return the exact correct distribution for position 0")
    (is-true
     (alist-eql (inference::get-posterior distributions 1)
		'(("1" 0.4)
		  ("2" 0.2)
		  ("3" 0.4))
		:test #'equal
		:alist-test #'string-equal)
     "Sum over time did not return the exact correct distribution for position 1")))

(test (interpretations->categories
	   :depends-on time-signature->metrical-interpretation)
  "Test a function that converts distributions over interpretations to a distribution
over categories."
  (let* ((interpretation-distribution '(("(2 4 4 0)" 0.1)
					("(2 4 4 1)" 0.2)
					("(3 4 4 0)" 0.3)
					("(3 4 4 1)" 0.1)
					("(3 4 4 2)" 0.2)))
	 (category-distribution
	  (inference::interpretations->categories interpretation-distribution)))
    (is (distributions-eql category-distribution
		       (prediction-sets:normalise-distribution
			(list (list "(2 4 4)" (utils:average 0.1 0.2))
			      (list "(3 4 4)" (utils:average 0.3 0.1 0.2))))
		       :alist-test #'string-equal))))

(test (count-categories :depends-on composition->grid)
  (with-mock-dataset 
      (let* ((grid-sequences (mapcar (lambda (composition)
				       (md::composition->grid composition :resolution 4))
				     dataset))
	     (monodies (mapcar #'md::composition->monody dataset))
	     (grid-metre-counts (inference::count-categories grid-sequences :grid 4))
	     (monodies-metre-counts (inference::count-categories monodies :melody nil))
	     (desired-counts '(("(2 2 4)" . 14)
			       ("(4 2 4)" . 2.5)
			       ("(3 3 4)" . 6))))
	(is (alist-eql grid-metre-counts desired-counts
		       :test #'equalp :alist-test #'string-equal))
	(is (alist-eql monodies-metre-counts desired-counts
		       :test #'equalp :alist-test #'string-equal)))))

(test get-category-training-set
  (with-mock-dataset
      (flet ((ts (barlength pulses)
	       (make-instance 'md:metrical-interpretation
			      :timebase 4
			      :barlength barlength
			      :pulses pulses
			      :phase 0)))
	(let ((data-2-2 (inference::get-category-training-set dataset (ts 2 2)))
	      (data-4-2 (inference::get-category-training-set dataset (ts 4 2)))
	      (data-3-3 (inference::get-category-training-set dataset (ts 3 3)))
	      (data-4-4 (inference::get-category-training-set dataset (ts 4 4))))
	  (format t "~{~A~}" (mapcar (lambda (seq) (format nil "~{~A ~}~%" (mapcar #'md:onset seq))) data-2-2))
	  (format t "~{~A~}" (mapcar (lambda (seq) (format nil "~{~A ~}~%" (mapcar #'md:onset seq))) data-4-2))
	  (format t "~{~A~}" (mapcar (lambda (seq) (format nil "~{~A ~}~%" (mapcar #'md:onset seq))) data-3-3))
	  (is (eql (length data-2-2) 2))
	  (is (eql (length data-4-2) 1))
	  (is (eql (length data-3-3) 1))
	  (is (eql (length data-4-4) 0))))))

(test initialise-prior-distribution
  (let* ((counts '(("(48 2 96)" . 5)
		   ("(72 3 96)" . 2)
		   ("(72 6 96)" . 3)))
	 (prior-distribution-4 (inference::initialise-prior-distribution counts 4))
	 (prior-distribution-8 (inference::initialise-prior-distribution counts 8))
	 (normalisation-4 (+ (* 2 0.5) (* 3 0.2) (* 3 0.3)))
	 (normalisation-8 (+ (* 4 0.5) (* 6 0.2) (* 6 0.3))))
    (is (distributions-eql prior-distribution-4
			   (list (list "(48 2 96 0)" (/ 0.5 normalisation-4))
				 (list "(48 2 96 24)" (/ 0.5 normalisation-4))
				 (list "(72 3 96 0)" (/ 0.2 normalisation-4))
				 (list "(72 3 96 24)" (/ 0.2 normalisation-4))
				 (list "(72 3 96 48)" (/ 0.2 normalisation-4))
				 (list "(72 6 96 0)" (/ 0.3 normalisation-4))
				 (list "(72 6 96 24)" (/ 0.3 normalisation-4))
				 (list "(72 6 96 48)" (/ 0.3 normalisation-4)))
			   :test (make-approx-eql-test 0.000001)
			   :alist-test #'string-equal))
    (is (distributions-eql prior-distribution-8
			   (list (list "(48 2 96 0)" (/ 0.5 normalisation-8))
				 (list "(48 2 96 12)" (/ 0.5 normalisation-8))
				 (list "(48 2 96 24)" (/ 0.5 normalisation-8))
				 (list "(48 2 96 36)" (/ 0.5 normalisation-8))
				 (list "(72 3 96 0)" (/ 0.2 normalisation-8))
				 (list "(72 3 96 12)" (/ 0.2 normalisation-8))
				 (list "(72 3 96 24)" (/ 0.2 normalisation-8))
				 (list "(72 3 96 36)" (/ 0.2 normalisation-8))
				 (list "(72 3 96 48)" (/ 0.2 normalisation-8))
				 (list "(72 3 96 60)" (/ 0.2 normalisation-8))
				 (list "(72 6 96 0)" (/ 0.3 normalisation-8))
				 (list "(72 6 96 12)" (/ 0.3 normalisation-8))
				 (list "(72 6 96 24)" (/ 0.3 normalisation-8))
				 (list "(72 6 96 36)" (/ 0.3 normalisation-8))
				 (list "(72 6 96 48)" (/ 0.3 normalisation-8))
				 (list "(72 6 96 60)" (/ 0.3 normalisation-8)))
			   :test (make-approx-eql-test 0.000001)
			   :alist-test #'string-equal))))
							   
;;; Tests that really need to be written:
;;; count-categories, generate-category-posteriors, initialise-prior-distribution, generate-category-predictions
