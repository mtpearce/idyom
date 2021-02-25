(cl:in-package #:generative-models)

(defclass idyom-meter-model (phase-model)
  ((observe-period :initform #'md:barlength)
   (observe-meter :initform #'md:pulses)
   (observe-ioi :initform #'md:bioi)
   (observe-pickup :initform #'md:bioi)))

(defclass idyom-temperley-model (temperley-meter)
  ((observe-period :initform #'md:barlength)
   (observe-meter :initform #'md:pulses)
   (observe-ioi :initform #'md:bioi)
   (observe-pickup :initform #'md:bioi)))

(defclass observe-once ()
  ((last-value :accessor last-value :initform nil))
  (:documentation "Mix-in for generative models.

Use to define observation functions that are guaranteed to run only
once per event (useful for non-deterministic observation functions)."))

(defclass bar-position-configurable (bar-position)
  ((period-correction? :initarg :period-correction? :initform t
		      :reader period-correction?)
   (higher-order? :initarg :higher-order? :initform t
		  :reader higher-order?)))

(defclass bar-position-random-input
    (bar-position-configurable observe-once) ())

(defclass random-input (phase-model observe-once)
  ((metres :initarg :metres)))

(defclass phase-model-random-input (random-input) ())

(defclass n-gram-phase-random-input (random-input) ())

(defmethod next :after ((model observe-once))
  (setf (last-value model) nil))

(defmethod generate ((model observe-once) call)
  (if (null (last-value model))
      (let ((value (funcall call)))
	(setf (last-value model) value)
	value)
      (last-value model)))

(defmethod create-features ((model bar-position-configurable))
  (multiple-value-bind (pickup meter-period period-correction
			       bar-position bar-position-observer)
      (values-list (call-next-method))
    (let ((features (list pickup meter-period bar-position bar-position-observer)))
      (if (higher-order? model)
	  (f:set-model bar-position 'models:idyom-ppm
		       :model-feature-args (list meter-period))
	  (f:set-model bar-position 'models:zeroth-order
		       :model-feature-args (list meter-period)))
      (if (period-correction? model)
	  (cons period-correction features)
	  features))))

(defmethod initialize-instance :after ((model bar-position-random-input)
				       &key periods metres)
  (flet ((random-ioi (e &optional (range (apply #'lcm periods)))
	   (declare (ignorable e))
	   (generate model (lambda () (random range)))))
    (setf (observe-ioi model) #'random-ioi
	  (observe-pickup model)
	  (lambda (&rest args) (declare (ignorable args)) 0)))
  (let ((metre-periods (utils:cartesian-product metres periods)))
    (setf (meter-period-domain model) metre-periods
	  (period-domain model) periods
	  (meter-domain model) metres)))

(defmethod initialize-instance :after ((model phase-model-random-input)
				       &key order-bound)
    (f:set-model (feature model 'phase) 'models::ppm-phase-model
		 :model-feature-args (list (feature model 'meter)))
  (setf (f:model-args (feature model 'phase))
	(append (list :phases (phases model)
		      :order-bound order-bound :mixtures t :escape :c)
		;;(append (list :order-bound nil)
		(f:model-args (feature model 'phase)))))

(defmethod initialize-instance :after ((model n-gram-phase-random-input)
				       &key (n 2))
    (f:set-model (feature model 'phase) 'models::n-gram-phase-model
		 :model-feature-args (list (feature model 'meter)))
  (setf (f:model-args (feature model 'phase))
	(append (list :phases (phases model) :n n)
		;;(append (list :order-bound nil)
		  (f:model-args (feature model 'phase)))))


;; Models
;; bar-position
;; bar-dist
;; bar-position-cycles
;; phase-model
;; constrained-phase-model

(defun test-model ()
  (let* ((model (make-instance 
		 'phase-model
		 :observe-ioi #'identity
		 ;;:observe-period (lambda (e) 3)
		 :resolution 1)))
    (setf (period-domain model) '(2 3)
	  (meter-domain model) '(a b)
	  (ioi-domain model) '(1 2 3 4)
	  (phases model) 6
	  (f:model-args (feature model 'phase)) '(:phases 6))
    (apply #'f:hide (latent model))
    (interpretations model '(0 1 0))))
  
(defun validate-model (model train test output-path)
  "Validate a model by parameterising in IDyOM database index TRAIN
and testing on IDYOM database index TEST.

OUTPUT-PATH is base path from which two output files are constructed:
test-set results are stored in <OUTPUT-PATH>.csv, labels are stored in
<OUTPUT-PATH>-labels.csv"
  (let* ((training-data (md:get-music-objects (list train) ()))
	 (testing-data (md:get-music-objects (list test) ()))
	 (to-list (lambda (x) (coerce x 'list)))
	 (training-data (map 'list to-list training-data))
	 (testing-data (map 'list to-list testing-data)))
    (set-domains model (append training-data testing-data))
;    (print (period-domain model))))
;(print (phases model))))
    (parameterise model training-data)
    (with-open-file (s (format nil "~a.csv" output-path)
		       :direction :output :if-exists :supersede
		       :if-does-not-exist :create)
      (test model testing-data :writers


	    (list (make-full-writer model s))))
    (with-open-file (s (format nil "~a-labels.csv" output-path)
		       :direction :output :if-exists :supersede
		       :if-does-not-exist :create)
      (generate-labels model testing-data :writers
		       (list (make-full-writer model s))))) nil)

(defun interpretations (model sequence &rest features)
  (multiple-value-bind (states locations)
      (model-sequence model sequence :predict? t)
    (dolist (s states)
      (multiple-value-bind (branch-ids elements probability)
	  (values-list s)
	(declare (ignorable branch-ids))
	(format t "~{~A~^, ~}: ~A~%~F~%"
		(loop for v in (state-element-list model elements)
		   for f in (f:identifiers (marginal model))
		   collect (format nil "~A ~A" f v))
		(probs:out probability)
		(apply #'trace-back
		       (append (list model s locations)
			     features)))))))

(defun empty-dataset (n-sequences n-events)
  (loop repeat n-sequences collect
       (loop repeat n-events collect nil)))

(defun test-random-input-model (model output-path 
				&key (n-sequences 4000) (n-events 50)
				  (distribution
				   (loop repeat (length (meter-domain model))
				      collect 1)))
  (loop for metre in (meter-domain model)
       for prop in distribution do
       (let ((period (cadr metre))
	     (metre (car metre))
	     (n-sequences (floor (* (/ prop (apply #'+ distribution))
				  n-sequences))))
	 (setf (observe-meter model) (lambda (e) (declare (ignorable e)) metre))
	 (setf (observe-period model) (lambda (e) (declare (ignorable e)) period))
	 (parameterise model (empty-dataset n-sequences n-events))))
  (with-open-file (s (format nil "~a" output-path)
		     :direction :output :if-exists :supersede
		     :if-does-not-exist :create)
    (test model (empty-dataset 100 50) :writers
	  (list (make-full-writer model s)))) 
  nil)









