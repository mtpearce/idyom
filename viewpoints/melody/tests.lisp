(cl:in-package #:viewpoints)

(5am:def-suite melodic-viewpoints)
(5am:in-suite melodic-viewpoints)

(5am:test metpos-viewpoint
  (let ((viewpoint (get-viewpoint 'metpos)))
    (with-fixtures (mel-seq1 latent1-states)
      (lv:with-latent-variable-state (category-8 latent1)
	(test-viewpoint mel-seq1 viewpoint '(0 2 3 4 6 2 4)))
      (lv:with-latent-variable-state (category-8-phase-2 latent1)
	(test-viewpoint mel-seq1 viewpoint '(6 0 1 2 4 0 2))))
    (5am:with-fixture mel-seq1 (8 2)
      (let ((training-viewpoint (training-viewpoint viewpoint)))
	(test-viewpoint mel-seq1 training-viewpoint '(0 2 3 4 6 2 4))))))
    
(5am:test bardist-viewpoint
  (let ((viewpoint (get-viewpoint 'bardist)))
    (with-fixtures (mel-seq1 latent1-states)
      (lv:with-latent-interpretation (category-8 latent1)
	(test-viewpoint mel-seq1 viewpoint '(0 0 0 0 0 1 0)))
      (lv:with-latent-interpretation (category-8-phase-2 latent1)
	(test-viewpoint mel-seq1 viewpoint '(0 1 0 0 0 1 0))))
    (5am:with-fixture mel-seq1 (8 2)
      (let ((training-viewpoint (training-viewpoint viewpoint)))
	(test-viewpoint mel-seq1 training-viewpoint '(0 0 0 0 0 1 0))))))

(5am:test bardist-legacy-viewpoint
  (let ((viewpoint (get-viewpoint 'bardist-legacy)))
    (with-fixtures (mel-seq1 latent1-states)
      (lv:with-latent-interpretation (category-8 latent1)
	(test-viewpoint mel-seq1 viewpoint (list +undefined+ 0 0 0 0 1 0) t))
      (lv:with-latent-interpretation (category-8-phase-2 latent1)
	(test-viewpoint mel-seq1 viewpoint (list +undefined+ 1 0 0 0 1 0) t)))
    (5am:with-fixture mel-seq1 (8 2)
      (let ((training-viewpoint (training-viewpoint viewpoint)))
	(test-viewpoint mel-seq1 training-viewpoint (list +undefined+ 0 0 0 0 1 0) t)))))

(5am:test bardist-legacy-metpos-viewpoint
  (let ((viewpoint (get-viewpoint '(metpos bardist-legacy))))
    (with-fixtures (mel-seq1 latent1-states)
      (lv:with-latent-interpretation (category-8 latent1)
	(test-viewpoint mel-seq1 viewpoint
			(append (list (list +undefined+ 0))
				'((0 2) (0 3) (0 4) (0 6) (1 2) (0 4))) t))
      (lv:with-latent-interpretation (category-8-phase-2 latent1)
	(test-viewpoint mel-seq1 viewpoint
			(append (list (list +undefined+ 6))
				'((1 0) (0 1) (0 2) (0 4) (1 0) (0 2))) t)))
    (5am:with-fixture mel-seq1 (8 2)
      (let ((training-viewpoint (training-viewpoint viewpoint)))
	(test-viewpoint mel-seq1 training-viewpoint
			(append (list (list +undefined+ 0))
				'((0 2) (0 3) (0 4) (0 6) (1 2) (0 4))) t)))))

(5am:test (bardist-metpos-viewpoint
	   :depends-on (and . (bardist-viewpoint metpos-viewpoint)))
  (let ((viewpoint (get-viewpoint '(bardist metpos))))
    (with-fixtures (mel-seq1 latent1-states)
      (lv:with-latent-interpretation (category-8 latent1)
	(test-viewpoint mel-seq1 (get-viewpoint '(bardist metpos))
			(mapcar #'list '(0 0 0 0 0 1 0)
				'(0 2 3 4 6 2 4))))
      (lv:with-latent-interpretation (category-8-phase-2 latent1)
	(test-viewpoint mel-seq1 (get-viewpoint '(bardist metpos))
			(mapcar #'list '(0 1 0 0 0 1 0)
				'(6 0 1 2 4 0 2)))))
    (5am:with-fixture mel-seq1 (8 2)
      (let ((training-viewpoint (training-viewpoint viewpoint)))
	(test-viewpoint mel-seq1 training-viewpoint
			(mapcar #'list '(0 0 0 0 0 1 0)
				'(0 2 3 4 6 2 4)))))))
