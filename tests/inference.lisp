(cl:in-package #:tests)

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
    (is (alist-eql category-distribution
		       (prediction-sets:normalise-distribution
			(list (list "(2 4 4)" (utils:average 0.1 0.2))
			      (list "(3 4 4)" (utils:average 0.3 0.1 0.2))))
		       :test #'equal
		       :alist-test #'string-equal))))

(test initialise-prior-distribution
  (let* ((counts '(("(48 4 96)" 5)
		   ("(72 3 96)" 2)
		   ("(72 6 96)" 3)))
	 (prior-distribution-4 (inference::initialise-prior-distribution counts 4))
	 (prior-distribution-8 (inference::initialise-prior-distribution counts 8))
	 (normalisation-4 (+ (* 2 0.5) (* 3 0.2) (* 3 0.3)))
	 (normalisation-8 (+ (* 4 0.5) (* 6 0.2) (* 6 0.3))))
    (is (alist-eql (prior-distribution-4 '(("(48 4 96 0)" (/ 0.5 normalisation-8))
					   ("(48 4 96 24)" (/ 0.5 normalisation-8))
					   ("(72 3 96 0)" (/ 0.2 normalisation-8))
					   ("(72 3 96 24)" (/ 0.2 normalisation-8))
					   ("(72 3 96 48)" (/ 0.2 normalisation-8))
					   ("(72 6 96 0)" (/ 0.3 normalisation-8))
					   ("(72 6 96 24)" (/ 0.3 normalisation-8))
					   ("(72 6 96 48)" (/ 0.3 normalisation-8))))))
    (is (alist-eql (prior-distribution-8 '(("(48 4 96 0)" (/ 0.5 normalisation-8))
					   ("(48 4 96 12)" (/ 0.5 normalisation-8))
					   ("(48 4 96 24)" (/ 0.5 normalisation-8))
					   ("(48 4 96 36)" (/ 0.5 normalisation-8))
					   ("(72 3 96 0)" (/ 0.2 normalisation-8))
					   ("(72 3 96 12)" (/ 0.2 normalisation-8))
					   ("(72 3 96 24)" (/ 0.2 normalisation-8))
					   ("(72 3 96 36)" (/ 0.2 normalisation-8))
					   ("(72 3 96 48)" (/ 0.2 normalisation-8))
					   ("(72 3 96 50)" (/ 0.2 normalisation-8))
					   ("(72 6 96 0)" (/ 0.3 normalisation-8))
					   ("(72 6 96 12)" (/ 0.3 normalisation-8))
					   ("(72 6 96 24)" (/ 0.3 normalisation-8))
					   ("(72 6 96 36)" (/ 0.3 normalisation-8))
					   ("(72 6 96 48)" (/ 0.3 normalisation-8))
					   ("(72 6 96 50)" (/ 0.3 normalisation-8))))))))
(test generate-category-posteriors
  (let* ((prior-distribution
       
							   
;;; Tests that really need to be written:
;;; count-categories, generate-category-posteriors, initialise-prior-distribution, generate-category-predictions
