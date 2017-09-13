(cl:in-package #:viewpoints)

(5am:def-suite melodic-viewpoints)
(5am:in-suite melodic-viewpoints)

(5am:test metpos-viewpoint
  (with-fixtures (mel-seq1 latent1-states)
    (lv:with-latent-variable-state (category-8 latent1)
      (test-viewpoint mel-seq1 (get-viewpoint 'metpos) '(0 2 3 4 6 2 4)))
    (lv:with-latent-variable-state (category-8-phase-2 latent1)
      (test-viewpoint mel-seq1 (get-viewpoint 'metpos) '(6 0 1 2 4 0 2)))))

(5am:test bardist-viewpoint
  (with-fixtures (mel-seq1 latent1-states)
    (lv:with-latent-interpretation (category-8 latent1)
      (test-viewpoint mel-seq1 (get-viewpoint 'bardist) '(0 0 0 0 0 1 0)))
    (lv:with-latent-interpretation (category-8-phase-2 latent1)
      (test-viewpoint mel-seq1 (get-viewpoint 'bardist) '(0 1 0 0 0 1 0)))))

(5am:test (bardist-metpos-viewpoint
	   :depends-on (and . (bardist-viewpoint metpos-viewpoint)))
  (with-fixtures (mel-seq1 latent1-states)
    (lv:with-latent-interpretation (category-8 latent1)
      (test-viewpoint mel-seq1 (get-viewpoint '(bardist metpos))
		      (mapcar #'list '(0 0 0 0 0 1 0)
			      '(0 2 3 4 6 2 4))))
    (lv:with-latent-interpretation (category-8-phase-2 latent1)
      (test-viewpoint mel-seq1 (get-viewpoint '(bardist metpos))
		      (mapcar #'list '(0 1 0 0 0 1 0)
			      '(6 0 1 2 4 0 2))))))
