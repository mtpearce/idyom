(cl:in-package #:viewpoints)

(5am:def-suite abstract-viewpoints)
(5am:in-suite abstract-viewpoints)

;;;=====================
;;; *Utility functions*
;;;=====================

(defparameter timebase 1)
(defparameter midc 60)
(defparameter test-iois '(0 2 1 1 2 4 2 4))

(defun onset-event (onset bioi duration &key barlength pulses
					  (identifier (make-instance 'md:event-identifier
							     :event-index 0
							     :composition-index 0
							     :dataset-index 0)))
  (make-instance 'md:music-event
		 :id identifier
		 :voice 0 :vertint12 0 :articulation nil
		 :comma nil :ornament nil :dyn nil
		 :accidental 0 :mpitch 40 :cpitch 80
		 :deltast 0 :bioi bioi :midc midc
		 :timebase timebase :onset onset
		 :duration duration :mode 0 :keysig 0
		 :pulses pulses :barlength barlength
		 :tempo nil :phrase nil))

(defun iois->onset-events (iois &key barlength pulses
				  (identifier (make-instance 'md:event-identifier
							     :event-index 0
							     :composition-index 0
							     :dataset-index 0)))
  "The first IOI is treated as the offset of the first event.
 The last ioi is treated as the duration of the last note."
  (let* ((durations (cdr iois))
	 (biois (butlast iois))
	 (onsets (apply #'utils:cumsum biois)))
    (mapcar #'(lambda (o bioi ioi)
		(onset-event o bioi ioi :barlength barlength
			     :pulses pulses
			     :identifier identifier))
	    onsets biois durations)))

(defun test-viewpoint (events viewpoint true-elements)
  (let* ((sequence (viewpoints:viewpoint-sequence viewpoint events)))
    (dotimes (i (length events))
      (let ((element (viewpoints:viewpoint-element viewpoint (subseq events 0 (1+ i))))
	    (true-element (elt true-elements i)))
	(5am:is (equal element true-element))
	(when (not (null element))
	  (5am:is (equal element (pop sequence))))))))

;;;=====================
;;; *Fixtures*
;;;=====================

(defun generate-with-fixtures (fixtures body)
  (let ((fixture (car fixtures))
	(remaining (cdr fixtures)))
    `(5am:with-fixture ,fixture ()
       ,@(if (null remaining)
	     body
	     (list (generate-with-fixtures remaining body))))))
  

(defmacro with-fixtures (fixtures &body body)
  (generate-with-fixtures fixtures body))

;; Same as posinbar
(define-viewpoint (normal1 derived (onset))
    ((events md:music-sequence) element) 
  :function (let ((onset (onset events))
                  (barlength (barlength events)))
              (cond ((undefined-p onset barlength) +undefined+)
                    ((zerop barlength) +undefined+)
                    ((zerop onset) 0)
                    ((> onset 0) (mod onset barlength))
                    (t +undefined+))))

(define-viewpoint (normal2 derived (onset))
    ((events md:music-sequence) element) 
  :function (let ((onset (onset events)))
              (cond ((undefined-p onset) +undefined+)
                    ((> onset 0) (+ onset 2))
                    (t +undefined+))))

;; Same as metpos
(define-abstract-viewpoint (abstract1 (onset) (:barlength :phase) normal1)
    ((events md:melodic-sequence) element)
  :function (lambda (barlength phase)
	      (let ((onset (onset events)))
		(mod (- onset phase) barlength))))

;; Same as keysig
(define-abstract-viewpoint (abstract2 (onset) (:keysig) normal1)
    ((events md:melodic-sequence) element)
  :function (lambda (add)
	      (let ((onset (onset events)))
		(+ onset add))))

(5am:def-fixture abstract1 ()
  (let ((abstract1 (get-viewpoint 'abstract1)))
    (&body)))

(5am:def-fixture latent1 ()
  (let ((latent1 (lv:get-latent-variable 'latent1)))
    (&body)))

(5am:def-fixture mel-seq1 ()
  (let ((mel-seq1 (make-instance 'md:melodic-sequence
				:%data (iois->onset-events test-iois))))
    (&body)))

(5am:def-fixture latent1-states ()
  (let* ((latent1 (lv:get-latent-variable 'latent1))
	 (category-8 (lv::create-latent-state latent1 '(8 4) :phase 0))
	 (category-8-phase-2 (lv::create-latent-state latent1 '(8 4) :phase 2)))
      (&body)))

;;;=====================
;;; *Test cases*
;;;=====================

;; Abstract viewpoint definition
(5am:test abstract-viewpoint
  (with-fixtures (abstract1 latent1-states mel-seq1)
    (5am:is (utils:set-equal (latent-parameters abstract1) '(:barlength :phase)))
    (5am:is (eq (training-viewpoint abstract1) 'normal1))
    (5am:is (abstract? abstract1))
    (lv:with-latent-variable-state (category-8 latent1)
      (test-viewpoint mel-seq1 abstract1 '(0 2 3 4 6 2 4)))
    (lv:with-latent-variable-state (category-8-phase-2 latent1)
      (test-viewpoint mel-seq1 abstract1 '(6 0 1 2 4 0 2)))))

(5am:test linked-viewpoints
  (let ((mixed-linked-viewpoint (get-viewpoint '(abstract1 normal1)))
	(abstract-linked-viewpoint (get-viewpoint '(abstract1 abstract2)))
	(normal-linked-viewpoint (get-viewpoint '(normal1 normal2))))
    (5am:is (eq (type-of abstract-linked-viewpoint) 'abstract-linked))
    (5am:is (eq (type-of mixed-linked-viewpoint) 'abstract-linked))
    (5am:is (eq (type-of normal-linked-viewpoint) 'linked))))

;; Abstract viewpoint alphabet
(5am:test abstract-viewpoint-alphabet
  (let ((latent1 (lv:get-latent-variable 'latent1))
	(latent-linked (lv:get-latent-variable '(latent1 latent2)))
	(abstract1 (get-viewpoint 'abstract1))
	(abstract-linked (get-viewpoint '(abstract1 abstract2))))
    ;; Viewpoint needs to be associated with a latent variable
    ;; before the alphabet can be set.
    (setf (latent-variable abstract1) latent1)
    (setf (latent-variable abstract-linked) latent-linked)
    (let ((category-1 '(0 0))
	  (category-2 '(0 1)))
    (lv:with-latent-category (category-1 latent1)
      (setf (viewpoint-alphabet abstract1) '(a b c))
      (setf (viewpoint-alphabet abstract-linked) '(a b c)))
    (lv:with-latent-category (category-2 latent1)
      (setf (viewpoint-alphabet abstract1) '(1 2 3))
      (setf (viewpoint-alphabet abstract-linked) '(1 2 3)))
    (5am:is (equal (viewpoint-alphabet abstract1) nil))
    (5am:is (equal (viewpoint-alphabet abstract-linked) nil))
    (lv:with-latent-category (category-1 latent1)
      (5am:is (equal (viewpoint-alphabet abstract1) '(a b c)))
      (5am:is (equal (viewpoint-alphabet abstract-linked) '(a b c))))
    (lv:with-latent-category (category-2 latent1)
      (5am:is (equal (viewpoint-alphabet abstract1) '(1 2 3)))
      (5am:is (equal (viewpoint-alphabet abstract-linked) '(1 2 3)))))))


