;;;; Misc. code for profiling

(start-idyom)
(require 'metering)

;; Investigating slow predictions for the GCT viewpoint

(in-package mvs)
(mon:monitor-form
  (let ((viewpoints::*basic-types* (list :h-cpitch)))
    (idyom:idyom 4 '(h-cpitch) '(h-gct)
		 :pretraining-ids '(2 3)
		 :models :ltm
		 :texture :harmony
		 :harmonic-reduction :regular-harmonic-rhythm
		 :pretraining-harmonic-reduction :none
		 :detail 2.5
		 :use-ltms-cache? t
		 :k 1)))
