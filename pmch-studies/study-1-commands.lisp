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
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-1-of-3* 3 '(1) nil)
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-2-of-3* 3 '(1) nil)
(pmch-s1:analyse-viewpoints pmch-s1:*h-vp-3-of-3* 3 '(1) nil)
