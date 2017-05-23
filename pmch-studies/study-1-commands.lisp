;;;; Test analyses
(pmch-s1:analyse-viewpoints '(h-cpitch h-cpitch-class-set) 4 nil
			    :reduce-harmony t
			    :output-path "/Users/peter/Temp/test-analysis/"
			    :training-set-size 1
			    :k 2)

;;;; Training and testing on the same dataset
;; Classical
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-1-of-3* 1 nil t)
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-2-of-3* 1 nil t)
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-3-of-3* 1 nil t)
;; Pop
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-1-of-3* 2 nil nil)
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-2-of-3* 2 nil nil)
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-3-of-3* 2 nil nil)
;; Jazz
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-1-of-3* 3 nil nil)
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-2-of-3* 3 nil nil)
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-3-of-3* 3 nil nil)

;;;; Training and testing on different datasets
;; Train on classical, test on jazz
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-1-of-8* 3 '(1)
			    :reduce-harmony nil
			    :reduce-harmony-pretraining t
			    :k 1)
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-2-of-8* 3 '(1)
			    :reduce-harmony nil
			    :reduce-harmony-pretraining t
			    :k 1)
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-3-of-8* 3 '(1)
			    :reduce-harmony nil
			    :reduce-harmony-pretraining t
			    :k 1)
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-4-of-8* 3 '(1)
			    :reduce-harmony nil
			    :reduce-harmony-pretraining t
			    :k 1)
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-5-of-8* 3 '(1)
			    :reduce-harmony nil
			    :reduce-harmony-pretraining t
			    :k 1)
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-6-of-8* 3 '(1)
			    :reduce-harmony nil
			    :reduce-harmony-pretraining t
			    :k 1)
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-7-of-8* 3 '(1)
			    :reduce-harmony nil
			    :reduce-harmony-pretraining t
			    :k 1)
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-8-of-8* 3 '(1)
			    :reduce-harmony nil
			    :reduce-harmony-pretraining t
			    :k 1)
