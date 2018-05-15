;;;; ======================================================================
;;;; File:       prediction-sets.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-18 18:54:17 marcusp>                           
;;;; Time-stamp: <2016-04-11 16:04:50 marcusp>                           
;;;; ======================================================================
;;;;
;;;; DESCRIPTION 
;;;;
;;;;   Functions for building and combining predictions sets for
;;;;   events, sequences and datasets and for computing entropy and
;;;;   related measures.
;;;;
;;;; ======================================================================

(cl:in-package #:prediction-sets)

;;;========================================================================
;;; Data Structures
;;;========================================================================

(defclass dataset-prediction ()
  ((target-viewpoint :accessor target-viewpoint :initarg :target-viewpoint :type (or null viewpoint))
   (viewpoint :accessor prediction-viewpoint :initarg :viewpoint
              :type (or null viewpoints:viewpoint))
   (set       :accessor prediction-set :initarg :set :type list)))

(defclass sequence-prediction ()
  ((target-viewpoint :accessor target-viewpoint :initarg :target-viewpoint :type (or null viewpoint))
   (viewpoint :accessor prediction-viewpoint :initarg :viewpoint
              :type (or null viewpoint))
   (index     :accessor prediction-index :initarg :index :type (integer 0 *))
   (set       :accessor prediction-set :initarg :set :type list)))

(defclass event-prediction ()
  ((target-viewpoint :accessor target-viewpoint :initarg :target-viewpoint :type (or null viewpoint))
   (viewpoint :accessor prediction-viewpoint :initarg :viewpoint
              :type (or null viewpoint))
   (order     :accessor prediction-order :initarg :order)
   (event     :accessor prediction-event :initarg :event)
   (weights   :accessor prediction-weights :initarg :weights)
   (element   :accessor prediction-element :initarg :element)
   (set       :accessor prediction-set :initarg :set :type list)))

(defclass marginal-event-prediction (event-prediction)
  ((prior :accessor prediction-prior :initarg :prior)
   (latent-states :accessor prediction-latent-states :initarg :latent-states)
   (latent-variable :accessor prediction-latent-variable :initarg :latent-variable)))

(defun make-dataset-prediction (&key viewpoint set target-viewpoint)
  (make-instance 'dataset-prediction :viewpoint viewpoint :set set 
                 :target-viewpoint target-viewpoint))

(defun make-sequence-prediction (&key viewpoint index set target-viewpoint)
  (make-instance 'sequence-prediction :viewpoint viewpoint :index index
                 :set set :target-viewpoint target-viewpoint))

(defun make-event-prediction (&key viewpoint element set event weights target-viewpoint order)
  ;; (format t "~&make-event-prediction: ~A ~A ~A ~A ~A ~A ~A" viewpoint element event weights basic-viewpoint order set  )
  (make-instance 'event-prediction :viewpoint viewpoint :element element
                 :set set :event event :weights weights 
                 :order order :target-viewpoint target-viewpoint))

(defun marginal-likelihood (prior-distribution likelihood-distribution)
  (apply #'+
	 (mapcar (lambda (prior likelihood) (* prior likelihood))
		 prior-distribution likelihood-distribution)))

(defun make-marginal-event-prediction (prior events event-predictions target-viewpoint)
  "Given a prior distribution, the sequence of events, a set of event predictions, which
is a list of event-predictions in which each item corresponds to an event prediction for
the corresponding  and 
a target viewpoint, calculate the marginal probability the target viewpoint element."
  (when (string-equal (viewpoint-name target-viewpoint) "onset")
    (viewpoints:set-onset-alphabet (butlast events)))
    (unless (viewpoints:basic-p target-viewpoint)
    (set-alphabet-from-context target-viewpoint events
			       (get-viewpoints (viewpoint-typeset target-viewpoint))))
  (let ((distribution-symbols (viewpoint-alphabet target-viewpoint))
	(distribution))
    (loop for symbol in distribution-symbols collect
	 (let ((likelihood 
		(marginal-likelihood
		 prior (mapcar (lambda (ep) (cadr (assoc symbol (prediction-set ep)
							 :test #'equal)))
			       event-predictions))))
	   (push (list symbol likelihood) distribution)))
    (make-instance 'marginal-event-prediction
		   :prior prior
		   :target-viewpoint target-viewpoint
		   :viewpoint (prediction-viewpoint (car event-predictions))
		   :event (car (last events))
		   :weights (mapcar #'prediction-weights event-predictions)
		   :order (mapcar #'prediction-order event-predictions)
		   :element (viewpoint-element target-viewpoint events)
		   :set distribution)))
  
;;;========================================================================
;;; Entropies for dataset and sequence prediction sets
;;;========================================================================

(defmethod average-codelengths ((d dataset-prediction))
  (mapcar #'average-codelength (prediction-set d)))

(defmethod average-codelength ((s sequence-prediction))
  (apply #'utils:average (codelengths s)))

(defmethod codelengths ((d dataset-prediction))
  (mapcar #'codelengths (prediction-set d)))

(defmethod codelengths ((s sequence-prediction))
  (let ((predictions (mapcar #'event-prediction (prediction-set s))))
    (mapcar #'(lambda (p) 
                (let ((p (nth 1 p)))
                  (codelength p)))
            predictions)))

(defmethod shannon-entropies ((d dataset-prediction))
  (mapcar #'shannon-entropies (prediction-set d)))

(defmethod shannon-entropies ((s sequence-prediction))
  (mapcar #'(lambda (e)
              (shannon-entropy (prediction-set e)))
          (prediction-set s)))

(defmethod event-predictions ((d dataset-prediction))
  (mapcar #'event-predictions (prediction-set d)))

(defmethod event-predictions ((s sequence-prediction))
  (mapcar #'event-prediction (prediction-set s)))

(defmethod event-prediction ((e event-prediction))
  (let ((event (prediction-element e))
        (distribution (prediction-set e)))
    (assoc event distribution :test #'equal)))

(defmethod sequence-probability ((s sequence-prediction))
  (let ((predictions (mapcar #'event-prediction (prediction-set s))))
    (reduce #'* predictions :key #'(lambda (x) (nth 1 x)))))

(defmethod prediction-set->likelihoods ((prediction-sets list))
  (mapcar #'prediction-set->likelihoods prediction-sets))

(defmethod prediction-set->likelihoods ((sequence-prediction sequence-prediction))
  (mapcar #'cadr (event-predictions sequence-prediction)))

(defmethod prediction-set->likelihoods ((dataset-prediction dataset-prediction))
  (mapcar #'prediction-set->likelihoods (prediction-set dataset-prediction)))

(defmethod prediction-set->likelihoods ((prediction-set event-prediction))
  (cadr (event-prediction prediction-set)))

(defmethod output-prediction ((prediction-set marginal-event-prediction))
  (let* ((target-viewpoint-name (viewpoint-name (target-viewpoint prediction-set)))
	 (distribution (prediction-set prediction-set))
	 (event-identifier (md:get-identifier (prediction-event prediction-set)))
	 (dataset-id (md:get-dataset-index event-identifier))
	 (composition-id (md:get-composition-index event-identifier))
	 (partition-id (md::get-partition-index event-identifier))
	 (event-id (md:get-event-index event-identifier))
	 (element (prediction-element prediction-set)))
    (loop for pair in distribution do
	 (let ((parameter (car pair))
	       (p (cadr pair)))
	   (format t "~A,~A,~A,~A,event-prediction,~A,~A,~A,~F~%"
		   dataset-id composition-id partition-id event-id
		   target-viewpoint-name element parameter p)))))

(defmethod output-prediction ((prediction-set event-prediction))
  (let* ((target-viewpoint-name (viewpoint-name (target-viewpoint prediction-set)))
	 (distribution (prediction-set prediction-set))
	 (event-identifier (md:get-identifier (prediction-event prediction-set)))
	 (dataset-id (md:get-dataset-index event-identifier))
	 (composition-id (md:get-composition-index event-identifier))
	 (event-id (md:get-event-index event-identifier))
	 (element (prediction-element prediction-set)))
    (loop for pair in distribution do
	 (let ((parameter (car pair))
	       (p (cadr pair)))
	   (format t "~A,~A,~A,~A,event-prediction,~A,~A,~A,~F~%"
		   dataset-id composition-id NIl event-id
		   target-viewpoint-name element parameter p)))))

;;;========================================================================
;;; Functions for distributions 
;;;========================================================================

(defun distribution-symbols (distribution) 
  (mapcar #'car distribution))

(defun distribution-probabilities (distribution) 
  (mapcar #'cadr distribution))

(defun shannon-entropy (distribution)
  "Calculates the entropy (bpc) for <distribution> which is a probability
   distribution over an alphabet in the form ((a_1 p_1) (a_2 p_2) ...) for
   all symbols a_i in the alphabet."
  (reduce #'+ distribution
          :key #'(lambda (p)
                   (let ((p (nth 1 p)))
                     (* p (codelength p))))))

(defun codelength (probability)
  "Returns the expected codelength of a symbol predicted with probability
   <probability>."
  (- (log probability 2)))

(declaim (inline maximum-entropy))
(defun maximum-entropy (alphabet)
  "Returns the maximum entropy over an alphabet of size <alph-size>."
  (declare (type list alphabet))
  (log (float (length alphabet) 0.0) 2))

;; (declaim (inline perplexity))
;; (defun perplexity (probability-list)
;;   "Calculates the perplexity given a list of event probabilities of the
;;    form ((e_1 p_1) (e_2 p_2) ...) for all symbols e_i in a given sequence." 
;;   (expt 2 (average-codelength probability-list)))

(defun relative-entropy (entropy maximum-entropy)
  "Returns the quotient of the <entropy> to the <maximum-entropy>."
  (if (> maximum-entropy 0.0) (/ entropy maximum-entropy) 1.0))

(defun relative-entropies (entropies maximum-entropy)
  "Returns a list of relative entropies corresponding to <entropies>."
  (mapcar #'(lambda (e) (relative-entropy e maximum-entropy)) entropies))

(defun weight (relative-entropy bias)
  "Returns a weight associated with a prediction with <relative-entropy>." 
  (expt relative-entropy (- bias)))

(defun weights (relative-entropies bias)
  "Returns weights associated with predictions with <relative-entropies>." 
  (mapcar #'(lambda (e) (weight e bias)) relative-entropies))

(defun sum-distribution (distribution)
  "Returns the sum of <distribution> a set of probabilities distributed
   over an alphabet."
  (apply #'+ (distribution-probabilities distribution)))

(declaim (inline sums-to-one-p))
(defun sums-to-one-p (distribution)
  "Returns true if <distribution> sums to one otherwise nil."
  (< 0.999 (sum-distribution distribution) 1.0))

(defun normalise-distribution (distribution)
  "Normalises <distribution> such that it sums to one."
  (if (sums-to-one-p distribution) distribution
      (let* ((sum (sum-distribution distribution))
             (scaling-factor (if (zerop sum) 1.0 (/ 1.0 sum))))
        (mapcar #'(lambda (ep) 
                    (let ((e (nth 0 ep)) (p (nth 1 ep)))
                      (list e (* p scaling-factor))))
                distribution))))

(defun all-values-in-range (distribution)
  "Returns true if all probabilities in <distribution> lie in the range
   (0,1] else nil."
  (flet ((not-in-range (number) 
           (or (<= number 0.0) (> number 1.0))))
    (declare (type list distribution))
    (unless
        (find-if #'not-in-range distribution :key #'(lambda (p) (nth 1 p)))
      t)))

(defun flat-distribution (alphabet)
  (let ((p (/ 1.0 (float (length alphabet) 0.0))))
    (mapcar #'(lambda (x) (list x p)) alphabet)))


;;;========================================================================
;;; Combining Prediction sets 
;;;========================================================================

(defun combine-distributions (event-predictions &optional 
                              (function #'geometric-combination)
                              (bias 0)
                              type)
  "Combines a list of distributionse using the combination function
   <function> with a bias value of <bias>. Type is a symbol reflecting
   the type of combination: STM-LTM, STM and LTM are meaningful
   values."
  (let* ((f (find-symbol (symbol-name function) (find-package :prediction-sets)))
         (full-prediction-sets (remove-if #'null event-predictions :key #'prediction-set))
         (empty-prediction-sets (remove-if-not #'null event-predictions :key #'prediction-set))
         (distributions (mapcar #'prediction-set full-prediction-sets))
         (old-weights (mapcar #'prediction-weights event-predictions))
         (orders (mapcar #'prediction-order event-predictions))
         (element (prediction-element (car event-predictions)))
         (viewpoint (prediction-viewpoint (car event-predictions))))
    ;;(print (list event-predictions distributions empty-prediction-sets))
    (multiple-value-bind (set weights)
        (funcall f distributions bias)
      (let ((orders 
             (cond ((and (eq type :ltm-stm) (null (car old-weights)))
                    (mapcar #'(lambda (ep o type) 
                                (list (utils:string-append (format nil "order.~(~A~)." type) 
                                                           (viewpoints:viewpoint-name (prediction-viewpoint ep))) 
                                      (if (null o) "NA" o)))
                            event-predictions orders '(:ltm :stm)))
                   ((eq type :ltm-stm) 
                    (reduce #'append orders))
                   (t (property-identifiers "order" type event-predictions orders))))
            (weights (if (eq type :ltm-stm)
                        (append (mapcar #'list '("weight.ltm" "weight.stm") (normalise-weights weights)) (reduce #'append old-weights))
                        (property-identifiers "weight" type full-prediction-sets (normalise-weights weights)))))
        (unless (eq type :ltm-stm)
          (setf weights (append weights (property-identifiers "weight" type empty-prediction-sets (make-list (length empty-prediction-sets) :initial-element "NA")))))
        ;; (print (list type "orders" orders "old-weights" old-weights "weights" weights))
        (make-event-prediction
         :target-viewpoint (target-viewpoint (car event-predictions))
         :viewpoint viewpoint ; (if (eq type :ltm-stm) viewpoint (mapcar #'prediction-viewpoint event-predictions))
         :element   element 
         :event     (prediction-event (car event-predictions))
         :order     orders
         :weights   weights
         :set       set)))))
    
(defun property-identifiers (property type event-predictions list)
  (mapcar #'(lambda (ep l) 
              (list (utils:string-append (format nil "~A.~(~A~)." property type) (viewpoints:viewpoint-name (prediction-viewpoint ep))) l))
          event-predictions list))

(defun print-weights (weights)
  (format t "~&~{~,3F ~}~%" (normalise-weights weights)))

(defun normalise-weights (weights)
  (let* ((sum (reduce #'+ weights))
         (scaling-factor (if (zerop sum) 1.0 (/ 1.0 sum))))
    (mapcar #'(lambda (w) (* w scaling-factor)) weights)))

(defun arithmetic-combination (distributions &optional (bias 0))
  "Combines the probability distributions <distribution-1> and
   <distribution-2> over the same ordered alphabet and in the form ((e_1
   p_1) (e_2 p_2) ...), using a weighted arithmetic mean."
  (labels ((weight-distribution (distribution weight sum-of-weights)
             (mapcar #'(lambda (ep) 
                         (let ((e (nth 0 ep)) (p (nth 1 ep)))
                           (list e (/ (* p weight) sum-of-weights))))
                     distribution))
           (weight-distributions (distributions weights)
             (let ((sum-of-weights (reduce #'+ weights)))
               (mapcar #'(lambda (d w)
                           (weight-distribution d w sum-of-weights))
                       distributions weights)))
           (combine-elements (&rest event-probability-pairs)
             (list (caar event-probability-pairs)
                   (sum-distribution event-probability-pairs)))
           (combine-distributions (weighted-distributions)
             (apply #'mapcar
                    (cons #'combine-elements weighted-distributions))))
    (let* ((entropies (mapcar #'shannon-entropy distributions))
           (max-entropy (maximum-entropy (car distributions)))
           (relative-entropies (relative-entropies entropies max-entropy))
           (weights (weights relative-entropies bias))
           (weighted-distributions
            (weight-distributions distributions weights)))
      (values (normalise-distribution
               (combine-distributions weighted-distributions))
              weights))))

(defun geometric-combination (distributions &optional (bias 0))
  "Combines the probability distributions <distribution-1> and
   <distribution-2> over the same ordered alphabet and in the form ((e_1
   p_1) (e_2 p_2) ...) using a weighted geometric mean."
  (labels ((weight-distribution (distribution weight sum-of-weights)
             (mapcar #'(lambda (ep) 
                         (let ((e (nth 0 ep)) (p (nth 1 ep)))
                           (list e (expt p (/ weight sum-of-weights)))))
                     distribution))
           (weight-distributions (distributions weights)
             (let ((sum-of-weights (reduce #'+ weights)))
               (mapcar #'(lambda (d w)
                           (weight-distribution d w sum-of-weights))
                       distributions weights)))
           (combine-elements (&rest event-probability-pairs)
             (list (caar event-probability-pairs)
                   (reduce #'* event-probability-pairs
                           :key #'(lambda (ep) (nth 1 ep)))))
           (combine-distributions (weighted-distributions)
             (apply #'mapcar
                    (cons #'combine-elements weighted-distributions))))
    (let* ((entropies (mapcar #'shannon-entropy distributions))
           (max-entropy (maximum-entropy (car distributions)))
           (relative-entropies (relative-entropies entropies max-entropy))
           (weights (weights relative-entropies bias))
           (weighted-distributions
            (weight-distributions distributions weights)))
      (values (normalise-distribution (combine-distributions weighted-distributions))
              weights))))

(defun bayesian-combination (distributions &optional bias)
  "Combines a set of probability distributions by multiplying the
estimates for each element and then normalising such that the
resulting disribution sums to one."
  (declare (ignore bias))
  (labels ((combine-elements (&rest event-probability-pairs)
             (list (caar event-probability-pairs)
                   (reduce #'* event-probability-pairs
                           :key #'(lambda (ep) (nth 1 ep)))))
           (combine-distributions (distributions)
             (apply #'mapcar (cons #'combine-elements distributions))))
    (normalise-distribution (combine-distributions distributions))))

(defun ranked-combination (distributions &optional bias)
  "Combines a set of probability distributions <distributions> by first
   converting each distribution into a ranked list, summing the ranks
   for each symbol over all distributions and then converting these
   summed ranks back into probabilities."
  (declare (ignore bias))
  (labels ((sort-distribution (distribution)
             (insertion-sort distribution
                             #'(lambda (p1 p2) (< (nth 1 p1) (nth 1 p2)))))
           (unsort-distribution (distribution sorted-distribution)
             (if (null distribution) '()
                 (cons (assoc (caar distribution) sorted-distribution
                              :test #'equal)
                       (unsort-distribution (cdr distribution)
                                            sorted-distribution))))
           (rank-distribution (distribution ranks sum-of-ranks)
             (let ((sorted-distribution (sort-distribution distribution)))
               (unsort-distribution distribution
                                    (mapcar #'(lambda (e r) 
                                                (list (nth 0 e)
                                                      (/ r sum-of-ranks)))
                                            sorted-distribution ranks))))
           (rank-distributions (distributions ranks)
             (let ((sum-of-ranks (* (reduce #'+ ranks)
                                    (length distributions))))
               (mapcar #'(lambda (d) (rank-distribution d ranks sum-of-ranks))
                       distributions)))
           (combine-elements (&rest event-rank-pairs)
             (list (caar event-rank-pairs)
                   (reduce #'+ event-rank-pairs
                           :key #'(lambda (er) (nth 1 er))))))
    (let* ((alphabet-size (length (car distributions)))
           (ranks (generate-integers 1 alphabet-size))
           (ranked-distributions (rank-distributions distributions ranks)))
      (apply #'mapcar (cons #'combine-elements ranked-distributions)))))

