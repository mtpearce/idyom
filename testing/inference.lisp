(cl:in-package #:idyom-tests)

(5am:def-suite inference)
(5am:in-suite inference)

(5am:test lookup-key
  (5am:is-true 
   (let ((key "test")
	 (alist (list (cons "test" 5) (cons 'test 4))))
     (eql (inference::lookup-key key alist) 5))))

(5am:test sum-over-time
  (5am:is-true
   (let ((distributions '(("1" . (0.2 0.4 0.8))
			  ("2" . (0.5 0.2 0.05))
			  ("3" . (0.3 0.4 0.15))))
	 (summed-distributions (list (cons "1" (utils:average 0.2 0.4 0.8))
				     (cons "2" (utils:average 0.5 0.2 0.05))
				     (cons "3" (utils:average 0.3 0.4 0.15)))))
     (alist-eql (inference::sum-over-time distributions) summed-distributions
		:alist-test #'string-equal))
   "Sum over time did not return the exact correct distribution"))

(5am:test get-posterior
  "Test a function that obtains the posterior distribution at a particular position"
  (let ((distributions '(("1" . (0.2 0.4 0.8))
			 ("2" . (0.5 0.2 0.05))
			 ("3" . (0.3 0.4 0.15)))))	
    (5am:is-true
     (let ((posterior (list (cons "1" 0.2)
			    (cons "2" 0.5)
			    (cons "3" 0.3))))
       (alist-eql (inference::get-posterior distributions 0) posterior
		  :alist-test #'string-equal))
     "Sum over time did not return the exact correct distribution for position 0")
    (5am:is-true
     (let ((posterior (list (cons "1" 0.4)
			    (cons "2" 0.2)
			    (cons "3" 0.4))))
       (alist-eql (inference::get-posterior distributions 1) posterior
		  :alist-test #'string-equal))
     "Sum over time did not return the exact correct distribution for position 1")))

(defun run ()
  (let* ((interpretation-distribution '(("(2 4 0 4)" 0.1)
					("(2 4 1 4)" 0.2)
					("(3 4 0 4)" 0.3)
					("(3 4 1 4)" 0.1)
					("(3 4 2 4)" 0.2)))
	 (category-distribution
	  (inference::interpretations->categories interpretation-distribution)))
    (print category-distribution)
    (5am:is (alist-eql category-distribution
		       (prediction-sets:normalise-distribution
			(list (list "(2 4)" (utils:average 0.1 0.2))
			      (list "(3 4)" (utils:average 0.3 0.1 0.2))))
		       :alist-test #'string-equal))))

(5am:test (interpretations->categories
	   :depends-on time-signature->metrical-interpretation)
  "Test a function that converts distributions over interpretations to a distribution
over categories."
  (let* ((interpretation-distribution '(("(2 4 0 4)" 0.1)
					("(2 4 1 4)" 0.2)
					("(3 4 0 4)" 0.3)
					("(3 4 1 4)" 0.1)
					("(3 4 2 4)" 0.2)))
	 (category-distribution
	  (inference::interpretations->categories interpretation-distribution)))
    (5am:is (alist-eql category-distribution
		       (prediction-sets:normalise-distribution
			(list (list "(2 4)" (utils:average 0.1 0.2))
			      (list "(3 4)" (utils:average 0.3 0.1 0.2))))
		       :test #'equal
		       :alist-test #'string-equal))))
							   
	
;;; Tests that really need to be written:
;;; count-categories, interpretaitons->categories, generate-category-posteriors, initialise-prior-distribution, generate-category-predictions
