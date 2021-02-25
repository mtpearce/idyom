(defun create-random-partition (count k)
  (let* ((test-sets (make-array k :initial-element nil))
	 (indices (loop for i below count collect i))
	 (shuffled-indices (utils:shuffle indices))
	 (current-test-set 0))
    (dolist (i shuffled-indices test-sets)
      (push i (svref test-sets current-test-set))
      (setf current-test-set (mod (1+ current-test-set) k)))))

(defun collect-indices (list indices)
  (loop for item in list
     for index below (length indices)
     if (member index indices)
     collect item))

(defun training-set (dataset partitions i)
  (let* ((indices (loop for i below (length dataset) collect i))
	 (training-indices
	  (remove-if #'(lambda (x) (member x (svref partitions i))) indices)))
    (collect-indices dataset training-indices)))

(defun testing-set (dataset partitions i)
  (collect-indices dataset (svref partitions i)))

(defmethod validate ((model gm::feature-graph) dataset
		     &key (resampling-folds 10)
		       (resampling-indices (loop for i below resampling-folds collect i))
		       pretraining-set)
  (set-domains model dataset)
  (let ((partitions (create-random-partition (length dataset) resampling-folds)))
    (dolist (i resampling-indices)
      (let ((training-set (training-set dataset partitions i))
	    (testing-set (testing-set dataset partitions i)))
	(gm::parameterise model (append pretraining-set training-set))
	(gm::test model testing-set)))))
