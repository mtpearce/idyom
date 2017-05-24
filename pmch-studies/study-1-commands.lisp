;;;; Test analyses
(pmch-s1:analyse-viewpoints '(h-cpitch h-cpitch-class-set) 4 nil
			    :reduce-harmony t
			    :output-path "/Users/peter/Temp/test-analysis/"
			    :training-set-size 1
			    :k 2)

;;;; Training and testing on the same dataset
(start-idyom)
;;;; Paths
(defparameter cl-user::*mac-os-output-dir*
  "/Users/peter/Dropbox/Academic/projects/idyom/studies/HarmonyRepresentations/inst/extdata/data-4/")
(defparameter cl-user::*linux-output-dir*
  "/home/peter/Dropbox/Academic/projects/idyom/studies/HarmonyRepresentations/inst/extdata/data-4/")
;; Pop
(loop for ts-size in '(665 10 330 20 165 40 80 495)
   do (pmch-s1:analyse-all-viewpoints 
       2 nil
       :k 10
       :training-set-size ts-size
       :output-path (if (member :os-macosx cl-user::*features*)
			cl-user::*mac-os-output-dir*
			cl-user::*linux-output-dir*)))
     


;;;; OLD ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;

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
