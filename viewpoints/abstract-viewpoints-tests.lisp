(cl:in-package #:viewpoints)

(5am:def-suite abstract-viewpoints)
(5am:in-suite abstract-viewpoints)

(defparameter timebase 16)
(defparameter midc 60)
(defparameter test-iois '(4 2 2 4 8 4 8))

(defun iois->biois (iois &optional (offset 0))
  (append (list offset) (subseq iois 0 (1- (length iois)))))

(defun onset-event (onset bioi duration &optional (barlength nil) (pulses nil))
  (make-instance 'md:music-event
		 :voice 0 :vertint12 0 :articulation nil
		 :comma nil :ornament nil :dyn nil
		 :accidental 0 :mpitch 40 :cpitch 80
		 :deltast 0 :bioi bioi :midc midc
		 :timebase timebase :onset onset
		 :duration duration :mode 0 :keysig 0
		 :pulses pulses :barlength barlength
		 :tempo nil :phrase nil))

(defun iois->onset-events (iois)
  (let* ((biois (iois->biois iois))
	 (onsets (apply #'utils:cumsum biois))
	 (durations iois))
    (mapcar #'onset-event onsets biois durations)))

(defmacro mock-mel-seq (&rest body)
  `(let ((mel-seq (make-instance 'md:melodic-sequence
				 :%data (iois->onset-events test-iois))))
     ,@body))

(defun crotchets-dur (crotchets)
  (* (/ 1 4) crotchets timebase))

(defun test-viewpoint (events attribute true-elements)
  (let* ((viewpoint (viewpoints:get-viewpoint attribute))
	 (sequence (viewpoints:viewpoint-sequence viewpoint events)))
    (dotimes (i (length events))
      (let ((element (viewpoints:viewpoint-element viewpoint (subseq events 0 (1+ i))))
	    (true-element (elt true-elements i)))
	(5am:is (equal element true-element))
	(when (not (null element))
	  (5am:is (equal element (pop sequence))))))))

(5am:test metpos-viewpoint
  (mock-mel-seq
   (lv:with-latent-state (list :barlength (crotchets-dur 4) :phase (crotchets-dur 0))
     (test-viewpoint mel-seq 'metpos
		     (mapcar #'crotchets-dur (list 0 1 (+ 1 (/ 1 2)) 2 3 1 2))))
   (lv:with-latent-state (list :barlength (crotchets-dur 4) :phase (crotchets-dur 1))
     (test-viewpoint mel-seq 'metpos
		     (mapcar #'crotchets-dur (list 3 0 (+ 0 (/ 1 2)) 1 2 0 1))))))

(5am:test bardist-viewpoint
  (mock-mel-seq
   (lv:with-latent-state (list :barlength (crotchets-dur 4) :phase (crotchets-dur 0))
     (test-viewpoint mel-seq 'bardist '(1 0 0 0 0 1 0)))
   (lv:with-latent-state (list :barlength (crotchets-dur 4) :phase (crotchets-dur 1))
     (test-viewpoint mel-seq 'bardist '(1 1 0 0 0 1 0)))))

(5am:test (bardist-metpos-viewpoint
	   :depends-on (and . (bardist-viewpoint metpos-viewpoint)))
  (mock-mel-seq
   (lv:with-latent-state (list :barlength (crotchets-dur 4) :phase (crotchets-dur 0))
     (test-viewpoint mel-seq '(bardist metpos)
		     (mapcar #'list '(1 0 0 0 0 1 0)
			     (mapcar #'crotchets-dur (list 0 1 (+ 1 (/ 1 2)) 2 3 1 2)))))
   (lv:with-latent-state (list :barlength (crotchets-dur 4) :phase (crotchets-dur 1))
     (test-viewpoint mel-seq '(bardist metpos)
		     (mapcar #'list '(1 1 0 0 0 1 0)
			     (mapcar #'crotchets-dur (list 3 0 (+ 0 (/ 1 2)) 1 2 0 1)))))))

