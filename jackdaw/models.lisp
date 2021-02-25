(cl:in-package #:models)

(defun uniform-distribution (alphabet)
  (loop for s in alphabet collect
       (list s (probs:in (/ 1 (length alphabet))))))

(defclass sequence-model ()
  ((conditioning :initarg :conditioning :reader conditioning)))

(defclass idyom-ppm (sequence-model ppm:ppm)
  ((ppm::alphabet :initform nil)
   (ppm::escape :initform :c)
   (ppm::mixtures :initform t)
   (ppm::update-exclusion :initform nil)
   (ppm::order-bound :initform nil))
  (:documentation "PPM model that's also a jackdaw-native
 sequence model. The overridden fields serve to set some defaults."))

(defclass verbose-ppm (idyom-ppm)
  ((destination :initarg :destination :accessor destination)
   (args :initarg :args :accessor args)))

(defclass categorical (sequence-model)
  (;(laplace-smoothing? :initarg :laplace-smoothing? :accessor laplace-smoothing?
;		       :initform nil)
   (observation-count :initarg :observation-count :initform 0 :accessor observation-count)
   (symbol-counts :initarg :symbol-counts :accessor symbol-counts
		  :initform (make-hash-table))
   (init-uniform? :initarg :init-uniform? :accessor init-uniform? :initform t)))

(defclass bernouilli (categorical)
  ((head-count :initform 0 :accessor head-count)
   (evaluation :initarg :evaluation :accessor evaluation
	       :initform (lambda (s) s))))

(defmethod probability ((model bernouilli) location symbol)
  (let ((p (/ (head-count model)
	      (observation-count model))))
    (probs:in
     (if (funcall (evaluation model) symbol)
	 p (- 1 p)))))

(defclass categorical-once (categorical) ())

(defclass dd-meter (categorical-once) ())

(defclass old-phase-metre-prior (categorical-once)
  ((metre-function :initarg :metre-function :accessor metre-function)
   (phases-function :initarg :phases-function :accessor phases-function)))

(defclass period-prior (sequence-model)
  ((period-domain :initarg :period-domain :accessor period-domain)))

(defclass phase-model ()
  ((phases :initarg :phases :accessor phases)
   ;; Temporary param, while features don't distinguish between model arguments
   ;; and conditioning variables.
   ;; Update: not temporary anymore because period is now part of meter
   (meter :initarg :meter :accessor meter))
  (:documentation "Mixin for predictive models. 

Ensure this mixin occurs first in a super-classes list for reasons of
method combination working properly.

Assumes input symbols are expressed on the interval (0, PHASES], where 
PHASES is assumed to always be an integer-multiple of PERIOD. Input 
symbols are converted to the interval (0, PERIOD]. Predictions on the 
same interval are converted back to (0, PHASES] by discounting probabilities 
by a factor Z which is calculated based on the entropy such that the 
expected metric information content is preserved assuming that the the 
observations are distributed in accordance with the predictive distribution 
on (0, PERIOD]."))

(defclass ppm-phase-model (phase-model idyom-ppm) ())

(defclass n-gram-phase-model (phase-model n-gram) ())

(defclass n-gram (sequence-model)
  ((n :initarg :n :accessor n)
   (context-counts :initform (make-hash-table :test #'equal) :accessor context-counts)
   (counts :initform (make-hash-table :test #'equal) :accessor counts)))

(defmethod printable ((model sequence-model))
  (list :type (type-of model)
	:data (printable-slots model)))

(defmethod printable-slots ((model sequence-model))
  (list :conditioning (conditioning model)))

(defmethod printable-slots ((model categorical))
  (append (call-next-method)
	  (list ;:laplace-smoothing? (laplace-smoothing? model)
		:observation-count (observation-count model)
		:symbol-counts (utils:hash-table->alist (symbol-counts model))
		:init-uniform? (init-uniform? model))))

(defmethod printable-slots ((model idyom-ppm))
  (append (call-next-method)
	  (list :leaves (utils:hash-table->alist (ppm::ppm-leaves model))
		:branches (utils:hash-table->alist (ppm::ppm-branches model))
		:dataset (ppm::dataset->alist model)
		:alphabet (ppm::ppm-alphabet model)
		:order-bound (ppm::ppm-order-bound model)
		:mixtures (ppm::ppm-mixtures model)
		:escape (ppm::ppm-escape model)
		:update-exclusion (ppm::ppm-update-exclusion model))))

(defmethod printable-slots ((model ppm-phase-model))
  (append (call-next-method)
	  (list :phases (phases model))))
  
(defun kw (symbol)
  (intern (symbol-name symbol) :keyword))

;;(defun serialize (slots object)
;;  (let ((result))
;;    (dolist (slot slots)
;;      (append (list (kw slot) (printable (slot-value

(defun from-printable (printable)
  (let* ((type (getf printable :type))
	 (printable (getf printable :data))
	 (model (make-instance type)))
    (set-slots-from-printable printable model)
;;    (let ((*package* (find-package :common-lisp-user)))
  ;;    (print (printable model)))
    model))

(defmethod set-slots-from-printable (printable (model sequence-model))
  (setf (slot-value model 'conditioning)
	(getf printable :conditioning)))

(defmethod set-slots-from-printable (printable (model idyom-ppm))
  (call-next-method)
  (setf (slot-value model 'ppm::leaves)
	(utils:alist->hash-table (getf printable :leaves))
	(slot-value model 'ppm::branches)
	(utils:alist->hash-table (getf printable :branches))
	(slot-value model 'ppm::dataset)
	(ppm::alist->dataset (getf printable :dataset))
	(slot-value model 'ppm::alphabet)
	(getf printable :alphabet)
	(slot-value model 'ppm::order-bound)
	(getf printable :order-bound)
	(slot-value model 'ppm::mixtures)
	(getf printable :mixtures)
	(slot-value model 'ppm::escape)
	(getf printable :escape)
	(slot-value model 'ppm::update-exclusion)
	(getf printable :update-exclusion))
  (ppm::initialize model))

(defmethod set-slots-from-printable (printable (model ppm-phase-model))
  (call-next-method)
  (setf (slot-value model 'phases)
	(getf printable :phases)
	(slot-value model 'meter)
	(getf printable :meter)))

(defmethod set-slots-from-printable (printable (model categorical))
  (call-next-method)
  ;(setf (slot-value model 'laplace-smoothing?)
;	(getf printable :laplace-smoothing?))
  (setf (slot-value model 'observation-count)
	(getf printable :observation-count))
  (setf (slot-value model 'symbol-counts)
	(utils:alist->hash-table (getf printable :symbol-counts) :test #'equal))
  (setf (slot-value model 'init-uniform?)
	(getf printable :init-uniform?)))

(defmethod root-location ((model n-gram))
  (loop repeat (1- (n model)) collect 'start))

(defmethod next-location ((model n-gram) symbol context &key construct?)
  (let ((n-gram (append context (list symbol)))
	(counts (counts model))
	(context-counts (context-counts model)))
    ;;(when (not construct?) (format t "~a ~a~%" context symbol))
    (when construct?
      (let ((new-context-count (1+ (gethash context context-counts 0)))
	    (new-count (1+ (gethash n-gram counts 0))))
	(setf (gethash context context-counts) new-context-count
	      (gethash n-gram counts) new-count)))
    (cdr n-gram)))

(defmethod probability ((model n-gram) location symbol)
  (probs:in
   (/ (gethash (append location (list symbol))
	       (counts model))
      (gethash location (context-counts model)))))

(defun normalize (distribution)
  (let ((total (apply #'probs:add (mapcar #'cadr distribution))))
    (loop for (s p) in distribution collect
	 (list s (probs:div p total)))))

(defmethod distribution ((model n-gram) location alphabet)
  (normalize
   (loop for symbol in alphabet collect
	(list symbol (probability model location symbol)))))
  
(defmethod initialize-instance :after ((model idyom-ppm) &key meter)
  "Just add a METER initargs."
  (declare (ignorable meter)))

;(defmethod initialize-instance :after ((model phase-model) &key)
;  "Set the alphabet to integers in the interval (0, PERIOD]."
;  (let ((period (cadr (meter model))))
;    (ppm:set-alphabet model (loop for p below period collect p))))

(defmethod root-location ((model idyom-ppm))
  (ppm:get-root))

(defmethod distribution ((model idyom-ppm) location alphabet)
  ;;(ppm:set-alphabet model (sort alphabet #'<))
  (ppm:set-alphabet model alphabet)
  (loop for p in (ppm::get-distribution model location) collect
       (list (car p) (probabilities:in (cadr p)))))

(defmethod next-location ((model idyom-ppm) symbol location &key construct?)
  (ppm::add-event-to-model-dataset model symbol)
  (let* ((next-location (ppm::ukkstep model nil location symbol construct?)))
    (when construct? (ppm::increment-counts model next-location))
    (ppm:increment-event-front model)
    next-location))

(defmethod next-sequence ((model idyom-ppm) &key construct?)
  (when construct? (ppm:initialise-virtual-nodes model))
  (ppm:increment-sequence-front model))

(defmethod last-event ((model idyom-ppm) location &key construct?)
  (when construct? (ppm:model-sentinel-event model location)))

(defgeneric next-location (sequence-model symbol location &key construct?))
(defgeneric next-sequence (sequence-model &key construct?))
(defgeneric last-event (sequence-model location &key construct?))
(defgeneric distribution (sequence-model location alphabet))
(defgeneric probability (sequence-model location symbol))
(defgeneric root-location (sequence-model))

(defmethod root-location ((model sequence-model)))

(defmethod root-location ((model categorical-once))
  t)

(defmethod root-location ((model period-prior))
  t)  

(defmethod probability ((model categorical) location symbol)
  (probabilities:in (/ (gethash symbol (symbol-counts model) 0)
		       (observation-count model))))

(defmethod probability ((model old-phase-metre-prior) location symbol)
  (call-next-method model location (funcall (metre-function model) symbol)))

(defun phase-distribution (probabilities n period z &optional (position 0))
  ;;(format t "mod ~a, pos ~a. probs: ~a~%"
;;	  (mod position (/ (+ n period) period)) position probabilities)
  (unless (eq position (+ n period))
    (if (eq (mod position (/ (+ n period) period)) 0)
	(let* ((p (car probabilities))
	       (rest (cdr probabilities)))
	  (cons (list position (probs:mul (probs:in z) p))
		(phase-distribution rest n period z (1+ position))))
	(cons (list position (probs:in (/ (- 1 z) n)))
	      (phase-distribution probabilities n period z (1+ position))))))

(defun correction-factor (entropy n period)
  (exp (- (/ (log (expt (+ 1 (/ n period)) entropy)) (log period)))))

(defmethod distribution ((model phase-model) location alphabet)
  "ALPHABET is a subset of PHASES of size PERIOD. Convert alphabet to the
domain of PERIOD."
  (let* ((period (cadr (first (conditioning model))))
	 (phases (phases model))
	 (n (- phases period))
	 (alphabet (loop for i below period collect i)))
    (let* ((distribution (call-next-method model location alphabet))
	   (probabilities (mapcar #'cadr distribution))
	   (entropy (probs:entropy probabilities))
	   (z (correction-factor entropy n period)))
      (phase-distribution probabilities n period z))))

(defmethod distribution ((model bernouilli) location alphabet)
  (if (eq (length alphabet) 1)
      (list (list (car alphabet) (probs:in 1)))
      (progn
	(assert (eq (length alphabet) 2))
	(list (list (car alphabet) (probability model location (car alphabet)))
	      (list (cadr alphabet) (probability model location (cadr alphabet)))))))
  
(defmethod distribution ((model categorical) location alphabet)
  (if (eq 0 (observation-count model))
      (if (init-uniform? model)
	  (let* ((effective-counts
		  (mapcar (lambda (s) (effective-count model s)) alphabet))
		 (total (apply #'+ effective-counts)))
	    (warn "Defaulting to uniform distribution for ~A." (type-of model))
	    (loop for c in effective-counts 
		 for s in alphabet collect
		 (list s (probs:in (/ c total)))))
	  (error "Not enough observations for generating a probability estimate."))
      (let* ((probabilities 
	      (loop for s in alphabet collect (probability model location s)))
	     (sum (apply #'probabilities:add probabilities))
	     (normalised (mapcar (lambda (p) (probabilities:div p sum)) probabilities)))
	(loop for p in normalised for s in alphabet collect (list s p)))))

(defmethod distribution ((model categorical-once) location alphabet)
  (if location
      (call-next-method)
      (uniform-distribution alphabet)))

(defmethod distribution ((model period-prior) location alphabet)
  (let ((sum-periods (apply #'+ (period-domain model))))
    (loop for period in alphabet collect
	 (list period (probs:in (/ period sum-periods))))))

(defmethod next-location ((model phase-model) symbol location &key construct?)
  (let* ((period (cadr (first (conditioning model))))
	 (new-symbol (/ symbol (/ (phases model) period))))
    (call-next-method model new-symbol location :construct? construct?)))

(defmethod effective-count ((model categorical) symbol) 1)

(defmethod effective-count ((model dd-meter) symbol)
  (let ((period (cadr symbol)))
    period))
  
(defmethod next-location ((model categorical) symbol location &key construct?)
  (when construct?
    (let* ((count (gethash symbol (symbol-counts model) 0))
	   (delta (effective-count model symbol)))
      (setf (gethash symbol (symbol-counts model)) (+ count delta))
      (incf (observation-count model) delta)
      nil)))

(defmethod next-location ((model categorical-once) symbol location &key construct?)
  (when (and construct? location) 
    (call-next-method)
    nil))

(defmethod next-location ((model old-phase-metre-prior) symbol location &key construct?)
  (when (and construct? location)
    (let* ((metre (funcall (metre-function model) symbol))
	   (phases (funcall (phases-function model) metre))
	   (next-location (call-next-method model metre location
					    :construct? construct?)))
      (incf (observation-count model) (1- phases))
      next-location)))

(defmethod next-location ((model verbose-ppm) symbol location &key construct?)
  (when (and construct? (equal (args model) '(8 2)))
    (format (destination model) "~A " (format nil "~A" symbol)))
  (call-next-method))

(defmethod next-location ((model period-prior) symbol location &key construct?))

(defmethod next-sequence ((model sequence-model) &key construct?))

(defmethod next-sequence ((model verbose-ppm) &key construct?)
  (when (and construct? (equal (args model) '(8 2)))
    (format (destination model) "~%"))
  (call-next-method))

(defmethod last-event ((model sequence-model) location &key construct?))
