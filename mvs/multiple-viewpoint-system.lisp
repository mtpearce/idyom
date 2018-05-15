;;;; ======================================================================
;;;; File:       multiple-viewpoint-system.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-27 18:54:17 marcusp>                           
;;;; Time-stamp: <2016-04-20 16:54:42 marcusp>                           
;;;; ======================================================================
;;;;
;;;; DESCRIPTION 
;;;;
;;;;   A multiple viewpoint system consists of a vector of viewpoint
;;;;   types and corresponding vectors containing long-term and
;;;;   short-term models models for each of those viewpoint
;;;;   types. This file contains methods for modelling sequences
;;;;   (construction and prediction) using multiple viewpoint systems.
;;;;
;;;; TODO 
;;;;
;;;;   - Should be able to select combination order:
;;;;       o Viewpoints then LTM-STM (as currently) 
;;;;       o LTM-STM then Viewpoints
;;;;       o simultaneously over all models 
;;;;
;;;;   - derived viewpoints predict over alphabet empirically derived
;;;;     from corpus, elements which do not correspond to any basic
;;;;     element are removed, basic elements which receive no prediction
;;;;     are predicted by escaping down to the order -1 model (cf
;;;;     Darrell's implementation).
;;;;
;;;;   - when predicting the value of a basic attribute using a linked
;;;;     viewpoint which has a component viewpoint which doesn't
;;;;     predict that basic attribute (e.g., (cpint dur)), assume that
;;;;     dur has already been predicted and use this to narrow down
;;;;     the elements of the derived alphabet considered.
;;;;
;;;;   - ensure that we work smoothly with predicting/generating more
;;;;     than one event attribute all at the same time. 
;;;;
;;;; ======================================================================

(cl:in-package #:mvs)

(defvar *debug* nil)

;;;========================================================================
;;; Data Structures
;;;========================================================================

(defclass mvs ()
  ((target :reader mvs-target :initarg :target :type list)
   (viewpoints :reader mvs-viewpoints :initarg :viewpoints
               :type (vector viewpoints:viewpoint))
   (ltm :reader mvs-ltm :initarg :ltm :type (vector ppm))
   (stm :reader mvs-stm :initarg :stm :type (vector ppm)))
  (:documentation "A multiple viewpoint system (MVS) consists of a vector
    of viewpoint objects <viewpoints> and two vectors containing the
    long- and short-term models corresponding to each viewpoint."))


;;;========================================================================
;;; Caching of event predictions 
;;;========================================================================

(defvar *ep-cache* nil)

(defun ep-cache-path (filename) 
  (utils:string-append (namestring *ep-cache-dir*) filename))

(defun initialise-ep-cache ()
  (setf *ep-cache* (make-hash-table :test #'equal)))

(defun disable-ep-cache () 
  (setf *ep-cache* nil))

(defun store-ep-cache (filename package) 
  (utils:write-object-to-file (utils:hash-table->alist *ep-cache*)
                              (ep-cache-path filename)
                              package))

(defun load-ep-cache (filename package) 
  (setf *ep-cache* 
        (alist->hash-table 
         (utils:read-object-from-file (ep-cache-path filename) package)
         :test #'equal)))

(defun ep-cache-enabled-p ()
  (hash-table-p *ep-cache*))

(defun ep-cache-key (m v e)
  (let ((e-id (md:get-identifier e)))
    (list m v (md:get-dataset-index e-id)
	  (md:get-composition-index e-id)
	  (md:get-event-index e-id))))

(defun cache-ep (distribution m v e)
  (when (ep-cache-enabled-p)
    (setf (gethash (ep-cache-key m v e) *ep-cache*) distribution)))

(defun cached-ep (m v e)
  (when (ep-cache-enabled-p) 
    (gethash (ep-cache-key m v e) *ep-cache*)))


;;;========================================================================
;;; Various utility methods and functions
;;;========================================================================

(defun set-ltm-stm-combination (comb)   (setf *ltm-stm-combination*   comb))
(defun set-viewpoint-combination (comb) (setf *viewpoint-combination* comb))
(defun set-ltm-stm-bias (bias)          (setf *ltm-stm-bias*          bias))
(defun set-viewpoint-bias (bias)        (setf *viewpoint-bias*        bias))
(defun set-models (models)              (setf *models*                models))

(defmethod count-viewpoints ((m mvs))
  "Returns the number of viewpoints modelled by <m>."
  (array-dimension (mvs-viewpoints m) 0))

(defmethod get-event-array ((m mvs) sequence)
  "Given a list of basic events <sequence> returns a vector of
   of viewpoint elements one for each of the viewpoints modelled by
   multiple-viewpoint-system <m>."
  (let* ((viewpoints (mvs-viewpoints m))
         (viewpoint-count (count-viewpoints m))
         (event-array (make-array viewpoint-count)))
    (dotimes (i viewpoint-count event-array)
      (setf (aref event-array i)
            (viewpoint-element (aref viewpoints i) sequence)))))

(defmethod operate-on-models ((m mvs) operation &key (models 'both)
                              ltm-args stm-args)
  "Calls a function <operation> which accepts a <ppm> object as it
sole argument on all the long- and short-term models in mvs <m>."
  (let ((viewpoint-count (count-viewpoints m)))
    (dotimes (model-index viewpoint-count)
      (let ((ltm (aref (mvs-ltm m) model-index))
            (stm (aref (mvs-stm m) model-index))
            (ltm-args (mapcar #'(lambda (x) (nth model-index x)) ltm-args))
            (stm-args (mapcar #'(lambda (x) (nth model-index x)) stm-args)))
        (unless (eql models 'stm) (apply operation (cons ltm ltm-args)))
        (unless (eql models 'ltm) (apply operation (cons stm stm-args)))))))

;;; needed in apps/generation.lisp (TODO rewrite that to use new
;;; SET-MODEL-ALPHABETS)
(defgeneric old-set-model-alphabets (mvs event-array events unconstrained))
(defmethod old-set-model-alphabets ((m mvs) event-array events unconstrained)
  "Sets the alphabets of all non-basic viewpoints and models in
multiple viewpoint system <m> such that they are capable of predicting
all of the elements of the basic viewpoints in their
typesets. <unconstrainted> is a list of basic viewpoints being
predicted which assume their full alphabets, otherwise the basic
alphabets are singletons determined on the basis of the values of the
final event in <events>. If <unconstrained> is passed a value of T,
all basic viewpoints are considered to be unconstrained."
  (dotimes (model-index (count-viewpoints m))
  ;;(format t "~&Viewpoint: ~A; Alphabet length: ~A~%" 
  ;;        (viewpoint-type (aref (mvs-viewpoints m) model-index))
  ;;        (length (viewpoint-alphabet (aref (mvs-viewpoints m) model-index))))
    (let* ((event (aref event-array model-index))
           (viewpoint (aref (mvs-viewpoints m) model-index))
           (ltm (aref (mvs-ltm m) model-index))
           (stm (aref (mvs-stm m) model-index)))
      (unless (or (viewpoints:basic-p viewpoint) (undefined-p event))
        (viewpoints:set-alphabet-from-context viewpoint events unconstrained))
      ;;(format t "~&Viewpoint: ~A; Event: ~A; Alphabet length: ~A~%" 
      ;;        (viewpoint-type viewpoint) event 
      ;;        (length (viewpoint-alphabet viewpoint)))
      (set-alphabet ltm (viewpoint-alphabet viewpoint))
      (set-alphabet stm (viewpoint-alphabet viewpoint)))))

(defmethod set-model-alphabets ((m mvs) event events viewpoint ltm stm 
                                unconstrained)
  "Sets the alphabets of all non-basic viewpoints and models in
multiple viewpoint system <m> such that they are capable of predicting
all of the elements of the basic viewpoints in their
typesets. <unconstrained> is a list specifying those viewpoints that
assume their full alphabet (rather than just the value of the current
event); it takes the following values: 
-1 = current viewpoint
0  = viewpoints derived from basic viewpoints in typeset of current viewpoint 
1  = viewpoints derived from basic viewpoint in the multiple viewpoint system
2  = viewpoints derived from all basic viewpoints
a list = a specified list of viewpoints 

See also VIEWPOINTS:SET-ALPHABET-FROM-CONTEXT."
  (let ((unconstrained 
         (case unconstrained
           ;; -1 = only the viewpoint currently being predicted
           (-1 viewpoint)
           ;; 0 = basic viewpoints in typeset of current viewpoint (and their derived viewpoints)
           (0 (mapcar #'viewpoints:get-viewpoint (viewpoints:viewpoint-typeset viewpoint)))
           ;; 1 = target viewpoints in MVS (and their derived viewpoints)
           (1 (mvs-target m)) 
           ;; 2 = all basic viewpoints (and their derived viewpoints) 
           (2 (mapcar #'viewpoints:get-viewpoint (viewpoints:get-basic-types event)))
           ;; 3 = a specified list of basic viewpoints         
           (t unconstrained))))
    (viewpoints::set-onset-alphabet (butlast events))
    (unless (or (viewpoints:basic-p viewpoint) (undefined-p event)) 
      (viewpoints:set-alphabet-from-context viewpoint events unconstrained))
    ;(format t "~&Viewpoint: ~A; Event: ~A; Alphabet length: ~A~%" 
    ;        (viewpoint-type viewpoint) event 
    ;        (length (viewpoint-alphabet viewpoint)))
    ;(format t "~&~A: ~A" (viewpoint-name viewpoint) 
    ;        (viewpoint-alphabet viewpoint))
    (set-alphabet ltm (viewpoint-alphabet viewpoint))
    (set-alphabet stm (viewpoint-alphabet viewpoint))))
  
(defmethod get-basic-viewpoint ((m mvs) derived-viewpoint)
  (find-if #'(lambda (b) (viewpoints:in-typeset-p b derived-viewpoint))   
           (mvs-target m)))

(defmethod sequence-prediction-sets ((m mvs) events event-prediction-sets)
  (let ((index (md:get-composition-index (md:get-identifier (car events)))))
    (apply #'mapcar
           (cons #'(lambda (&rest e)
                     (make-sequence-prediction :viewpoint (car e)
                                               :index index
                                               :set (cdr e)))
                 (cons (mvs-target m) event-prediction-sets)))))

(defmethod dataset-prediction-sets ((m mvs) sequence-prediction-sets)
  (apply #'mapcar
         (cons #'(lambda (&rest c)
                   (make-dataset-prediction :viewpoint (car c)
                                            :set (cdr c)))
               (cons (mvs-target m) sequence-prediction-sets))))


;;;========================================================================
;;; Model Initialisation 
;;;========================================================================

(defun make-mvs (target-viewpoints viewpoints ltms
		 &key (class 'mvs) latent-variable)
  "Instantiate a multiple viewpoint system with target viewpoints TARGET-VIEWPOINTS, 
basic or derived viewpoints VIEWPOINTS, and a set of long-term models LTMS, with one model 
for each viewpoint in VIEWPOINTS. :CLASS can be used to specify subclasses of MVS."
  (flet ((sanity-check-target-viewpoints ()
           (dolist (tv target-viewpoints target-viewpoints)
             (unless (find (viewpoint-typeset tv) viewpoints 
                           :key #'viewpoint-typeset
                           :test #'(lambda (x y) (subsetp x y)))
               (warn "~&None of the supplied viewpoints can predict target feature ~A.~%"
                     (viewpoints:viewpoint-name tv)))))
         (sanity-check-viewpoints ()
           (dolist (v viewpoints viewpoints)
             (unless (some #'(lambda (tv)
			       (subsetp (viewpoint-typeset tv) (viewpoint-typeset v)))
                           target-viewpoints)
               (warn "~&Viewpoint ~A cannot predict any of the supplied target features.~%"
                     (viewpoints:viewpoint-name v))))))
    ;;(format t "~&MAKE-MVS: basic-viewpoints = ~A~&MAKE-MVS: viewpoints = ~A"
    ;;        (sanity-check-basic-viewpoints) 
    ;;        (sanity-check-viewpoints))
    (let ((ltms-table (make-hash-table :test #'equal))
	  (stms-table (make-hash-table :test #'equal)))
      (when (eq class 'abstract-mvs)
	(assert (not (null latent-variable))
		(latent-variable)
		":LATENT-VARIABLE keyword argument is required for MAKE-MVS when
 :CLASS is ABSTRACT-MVS.~%")
	;; Because viewpoint-alphabet is stored in viewpoints, and these may depend on
	;; the latent state, viewpoints need to know the latent variable.
	(loop for viewpoint in viewpoints do
	     (setf (viewpoints:latent-variable viewpoint) latent-variable))
	;; Store all models for all viewpoints in a hash table whose 
	;; keys consist of the latent-variable name and category.
	(loop for category-models in ltms do
	     (let* ((category (car category-models))
		    (lt-models (cdr category-models))
		    (st-models (lv:with-latent-category (category latent-variable)
				 (get-short-term-models viewpoints))))
	       (setf (gethash category ltms-table) lt-models)
	       (setf (gethash category stms-table) st-models))))
      ;; Instantiate the class.
      (let ((mvs (apply #'make-instance
			(append (list class
				      :target (sanity-check-target-viewpoints)
				      :viewpoints (apply #'vector 
							 (sanity-check-viewpoints)))
				
				(if (eq class 'abstract-mvs)
				    (list :ltm ltms-table :stm stms-table
					  :latent-variable latent-variable)
				    (list :ltm (apply #'vector ltms)
					  :stm (get-short-term-models viewpoints)))))))
    (set-mvs-parameters mvs)
    mvs))))

(defun set-mvs-parameters-function (m &key
					(ltm-order-bound *ltm-order-bound*)
					(ltm-mixtures *ltm-mixtures*)
					(ltm-update-exclusion *ltm-update-exclusion*)
					(ltm-escape *ltm-escape*)
					(stm-order-bound *stm-order-bound*)
					(stm-mixtures *stm-mixtures*)
					(stm-update-exclusion *stm-update-exclusion*)
					(stm-escape *stm-escape*))
  (map 'vector #'(lambda (model)
                   (set-ppm-parameters model :order-bound ltm-order-bound
                                       :mixtures ltm-mixtures
                                       :update-exclusion ltm-update-exclusion
                                       :escape ltm-escape))
       (mvs-ltm m))
  (map 'vector #'(lambda (model)
                   (set-ppm-parameters model :order-bound stm-order-bound
                                       :mixtures stm-mixtures
                                       :update-exclusion stm-update-exclusion
                                       :escape stm-escape))
       (mvs-stm m)))

(defmethod set-mvs-parameters ((m mvs) &rest parameters
			       &key &allow-other-keys)
  (apply #'set-mvs-parameters-function (cons m parameters)))

(defun get-short-term-models (viewpoints)
  "Returns a vector of freshly initialised ppm short term models
corresponding to the supplied list of viewpoints and initialised with
the supplied parameters."
  (map 'vector #'(lambda (v) (make-ppm (viewpoint-alphabet v))) viewpoints))
                  
;;;========================================================================
;;; Model Construction and Prediction 
;;;========================================================================

(defmethod model-dataset ((m mvs) dataset &key construct? predict?)
  "Models a dataset <dataset> (a vector of sequence vectors) given the
multiple-viewpoint system <m>."
  (labels ((model-d (dataset sequence-index)
             (when *debug* (format t "~&Composition ~A~%" sequence-index))
	     ;; Run garbage collection to clean up any left-over garbage from
	     ;; modelling the previous sequence.
	     (sb-ext:gc :full t)
             (unless (null dataset)
	       (let ((prediction-set (model-sequence m (coerce (car dataset) 'list)
						     :construct? construct?
						     :predict? predict?)))
		 (when *output-csv*
		   (mapcar
		    (lambda (sp)
		      (mapcar #'prediction-sets::output-prediction
			      (prediction-sets:prediction-set sp)))
		    prediction-set))
		 (unless (= sequence-index 1)
		   (operate-on-models m #'increment-sequence-front))
		 (operate-on-models m #'reinitialise-ppm :models 'stm)
		 (model-d (cdr dataset) (1- sequence-index))))))
    (dataset-prediction-sets m (model-d dataset (length dataset)))))

(defmethod model-sequence ((m mvs) sequence &key construct? predict? 
                           (construct-from 0) (predict-from 0))
  "Models a sequence <sequence> consisting of a vector of
event-vectors given the multiple-viewpoint system <m>. The indices of
the component models into the set of sequences must be set to the
appropriate sequence index before this method is called."
  (let* ((event-count (length sequence))
         (viewpoint-count (count-viewpoints m))
         (prediction-sets '())
         (ltm-locations
          (make-sequence 'vector viewpoint-count :initial-element (ppm:get-root)))
         (stm-locations
          (make-sequence 'vector viewpoint-count :initial-element (ppm:get-root))))
    (dotimes (event-index event-count)
      (when *debug* (format t "~&Event ~A~%" event-index))
      (let* ((events (subseq sequence 0 (1+ event-index)))
             (event-array (get-event-array m events))
             (construct? (if (< event-index construct-from) nil construct?))
             (predict? (if (< event-index predict-from) nil predict?)))
;;         (set-model-alphabets m event-array events 
	;;                              *marginalise-using-current-event*)
        (multiple-value-bind (ltm-next-locations stm-next-locations
                              ltm-prediction-sets stm-prediction-sets)
            (model-event m event-array events :ltm-locations ltm-locations 
                         :stm-locations stm-locations 
                         :construct? construct? :predict? predict?)
;	  (when (equal event-index 0)
;	    (print (map 'list #'md:onset events))
;	    (print (viewpoints:viewpoint-alphabet (viewpoints:get-viewpoint 'bioi)))
;	    (print (viewpoints:viewpoint-alphabet (viewpoints:get-viewpoint 'onset)))
;	    (print (prediction-set (first ltm-prediction-sets))))
          (setf ltm-locations ltm-next-locations
                stm-locations stm-next-locations)
          (operate-on-models m #'increment-event-front)
          (when (>= event-index predict-from)
            (let ((combined 
                   (combine-predictions m ltm-prediction-sets 
                                        stm-prediction-sets events)))
              (unless (null combined)
                (push combined prediction-sets)))))))
    (when construct?
      (operate-on-models m #'model-sentinel-event :models 'ltm
                         :ltm-args (list (coerce ltm-locations 'list)))
      (operate-on-models m #'initialise-virtual-nodes))
    (sequence-prediction-sets m sequence (reverse prediction-sets))))

(defmethod model-event ((m mvs) event-array events &key ltm-locations
						     stm-locations construct? predict?)
  "Models a vector of events <event-array> appearing at a vector of
locations (<ltm-locations> and <stm-locations>) in the ltm and stm of
multiple viewpoint system <m>."
  (let* ((viewpoint-count (count-viewpoints m))
         (viewpoints (mvs-viewpoints m))
         (stm-prediction-sets '())
         (ltm-prediction-sets '()))
    (dotimes (i viewpoint-count)
      (let* ((event (aref event-array i))
             (viewpoint (aref viewpoints i))
             (ltm (aref (mvs-ltm m) i))
             (stm (aref (mvs-stm m) i))
             (ltm-location (aref ltm-locations i))
             (stm-location (aref stm-locations i)))
        (set-model-alphabets m event-array events viewpoint ltm stm 
                             *marginalise-using-current-event*)
        (unless (undefined-p event)
          (when (and *debug* predict?) (format t "~&LTM: ~S" (viewpoints:viewpoint-name viewpoint)))
          (multiple-value-bind (ltm-next-location ltm-distribution ltm-order)
              (ppm:ppm-model-event ltm event :location ltm-location 
				   :construct? (and construct? 
						    (or (eq *models* :ltm+)
							(eq *models* :both+)))
				   :predict? (and predict? 
						  (or (eq *models* :ltm+)
						      (eq *models* :ltm)
						      (eq *models* :both)
						      (eq *models* :both+))))
;	    (when (equal (length events) 1)
;	      (print "LTM Distribution")
;	      (print ltm-distribution))
            (setf (aref ltm-locations i) ltm-next-location)
            (when (and *debug* predict?) (format t "~&ltm-distribution = ~&~A~%" ltm-distribution))
            (push (make-event-prediction :order ltm-order
                                         :viewpoint viewpoint
                                         :event (car (last events))
                                         :element event
                                         :set ltm-distribution)
                  ltm-prediction-sets))
          
          (when (and *debug* predict?) (format t "~&STM: ~S" (viewpoints:viewpoint-name viewpoint)))
          (multiple-value-bind (stm-next-location stm-distribution stm-order)
              (ppm:ppm-model-event stm event :location stm-location 
				   :construct? (or (eq *models* :stm)
						   (eq *models* :both) 
						   (eq *models* :both+))
				   :predict? (and predict? 
						  (or (eq *models* :stm)
						      (eq *models* :both) 
						      (eq *models* :both+))))
            (setf (aref stm-locations i) stm-next-location)
            (when (and *debug* predict?) (format t "~&stm-distribution = ~&~A~%" stm-distribution))
            (push (make-event-prediction :order stm-order
                                         :viewpoint viewpoint
                                         :event (car (last events))
                                         :element event
                                         :set stm-distribution)
                  stm-prediction-sets)))))
    (values ltm-locations stm-locations ltm-prediction-sets
            stm-prediction-sets)))


;;;========================================================================
;;; Combining event predictions
;;;========================================================================

(defun combine-predictions (mvs ltm-prediction-sets stm-prediction-sets events)
  (case *models*
    ((or :ltm :ltm+)
     (combine-viewpoint-predictions mvs ltm-prediction-sets events :ltm))
    (:stm 
     (combine-viewpoint-predictions mvs stm-prediction-sets events :stm))
    (otherwise
     (combine-ltm-stm-predictions
      (combine-viewpoint-predictions mvs ltm-prediction-sets events :ltm)
      (combine-viewpoint-predictions mvs stm-prediction-sets events :stm)))))

(defun combine-viewpoint-distributions (dists model) 
  (combine-distributions dists *viewpoint-combination* *viewpoint-bias* model))

(defun combine-ltm-stm-distributions (dists) 
  (combine-distributions dists *ltm-stm-combination* *ltm-stm-bias* :ltm-stm))

(defun combine-ltm-stm-predictions (ltm-predictions stm-predictions)
  (mapcar #'(lambda (l s) (combine-ltm-stm-distributions (list l s)))
          ltm-predictions stm-predictions))

(defun combine-viewpoint-predictions (mvs prediction-sets events model)
  ;;(format t "~&Viewpoint Combination: ~A ~A~%" model prediction-sets)
  (flet ((basic-prediction-set (prediction-sets target-viewpoint)
           (find-if #'(lambda (p) (viewpoints:viewpoint-equal p target-viewpoint))
                    prediction-sets :key #'prediction-viewpoint))
         (derived-prediction-sets (prediction-sets target-viewpoint)
           (remove-if #'(lambda (ps) 
                          (let ((v (prediction-viewpoint ps)))
                            ;;(format t "~&dps: ~A ~A" basic-viewpoint v)
                            (or (viewpoints:basic-p v) 
                                (not 
                                 (subsetp (viewpoint-typeset target-viewpoint)
					  (viewpoint-typeset v))))))
                      prediction-sets)))
    (let ((target-viewpoints (mvs-target mvs))
          (distributions '()))
      (dolist (target-viewpoint target-viewpoints)
        (let* ((derived-viewpoints
                (coerce (remove-if-not #'(lambda (v) 
                                           (or (viewpoint-equal target-viewpoint v)
					       (subsetp (viewpoint-typeset target-viewpoint)
							(viewpoint-typeset v))))
                                       (mvs-viewpoints mvs))
                        'list))
               (derived-prediction-sets
                (derived-prediction-sets prediction-sets target-viewpoint))
               (basic-prediction-set
                (basic-prediction-set prediction-sets target-viewpoint))
               (target-distributions (unless (null basic-prediction-set) 
                                      (list basic-prediction-set))))
          ;;(format t "~&~A: ~A ~A~%" basic-viewpoint 
          ;;        basic-prediction-set
          ;;        derived-prediction-sets)
          (if (and (null basic-prediction-set) (null derived-prediction-sets))
              ;; When all viewpoints are undefined: 
              ;; 1. Ignore it 
              ;; nil))
              ;; 2. Predict using current basic viewpoint 
              ;; ? 
              ;; 3. uniform distrubution over basic alphabet;
	      (push 
               (combine-viewpoint-distributions
                (mapcar #'(lambda (v)
                            (make-custom-event-prediction target-viewpoint v events :flat))
                        derived-viewpoints)
                model)
               distributions)
              ;; Otherwise...
              (progn 
                (dolist (derived-prediction-set derived-prediction-sets)
                  (let ((target-distribution
                         (derived->target derived-prediction-set target-viewpoint
					  events)))
                    ;;(format t "~&Derived distribution: ~A~%" (prediction-set derived-prediction-set))
                    ;;(format t "~&Basic distribution: ~A~%" (prediction-set basic-distribution))
                    (unless (null target-distribution)
                      (cache-ep (prediction-set target-distribution) model 
                                (viewpoint-type 
                                 (prediction-viewpoint derived-prediction-set))
                                (car (last events)))
                      (push target-distribution target-distributions))))
                (dolist (v derived-viewpoints)
                  (unless (find v target-distributions :key #'prediction-viewpoint :test #'viewpoint-equal)
                    (push (make-custom-event-prediction target-viewpoint v events :empty) target-distributions)))
                (push (combine-viewpoint-distributions target-distributions model)
                      distributions)))))
      ;; (format t "~&Combined viewpoint distributions = ~A~%" (mapcar #'prediction-sets:prediction-set (reverse distributions)))
      (reverse distributions))))

(defun make-custom-event-prediction (target-viewpoint viewpoint events type)
  "TYPE can be :FLAT or :EMPTY."
  (unless (or (viewpoints:basic-p target-viewpoint) (eq type :empty))
    (set-alphabet-from-context target-viewpoint events
			       (get-viewpoints (viewpoint-typeset target-viewpoint))))
  (make-event-prediction
   :target-viewpoint target-viewpoint
   :order (case type (:flat 0) (:empty "NA"))  ; (list (list (format nil "~A.~(~A~).~A" "order" model (viewpoint-name basic-viewpoint)) 0))
   :viewpoint viewpoint
   :weights nil
   :event (car (last events))
   :element (viewpoint-element target-viewpoint events)
   :set (case type
          (:flat (prediction-sets:flat-distribution (viewpoints:viewpoint-alphabet target-viewpoint)))
          (:empty nil))))

(defun derived->target (source-prediction-set target-viewpoint events)
  (unless (viewpoints:basic-p target-viewpoint)
    (set-alphabet-from-context target-viewpoint events
			       (get-viewpoints (viewpoint-typeset target-viewpoint))))p
  (let* ((source-viewpoint (prediction-viewpoint source-prediction-set))
	 (source-distribution (prediction-set source-prediction-set))
	 (target-alphabet (viewpoint-alphabet target-viewpoint))
	 (source-alphabet (viewpoint-alphabet source-viewpoint))
	 (basic-viewpoint (get-viewpoint (viewpoint-typeset source-viewpoint)))
	 (continuations (alphabet->events basic-viewpoint events))
	 (target-basic-mapping (make-hash-table :test #'equal))
	 (source-basic-mapping (make-hash-table :test #'equal))
	 (basic-distribution (make-hash-table :test #'equal))
	 (target-distribution))
    (assert (subsetp (viewpoint-typeset target-viewpoint)
		     (viewpoint-typeset source-viewpoint)))
 ;   (format t "Target alphabet: ~A~%" (viewpoint-alphabet target-viewpoint))
					;
 ;   (format t "Source alphabet: ~A~%" (viewpoint-alphabet source-viewpoint))
 ;   (format t "Source distribution: ~A~%" source-distribution)
    ;; Create source to basic and target to basic mappings
    ;; TODO: use inverse viewpoint function if defined
    (loop for continuation in continuations do
	 (let* ((temp-comp (append (butlast events) (list continuation)))
		(target-element (viewpoint-element target-viewpoint temp-comp))
		(source-element (viewpoint-element source-viewpoint temp-comp))
		(basic-element (viewpoint-element basic-viewpoint temp-comp))
		(basic-elements-for-target (gethash target-element target-basic-mapping ()))
		(basic-elements-for-source (gethash source-element source-basic-mapping ())))
	   (setf (gethash target-element target-basic-mapping)
		 (cons basic-element basic-elements-for-target))
	   (setf (gethash source-element source-basic-mapping)
		 (cons basic-element basic-elements-for-source))))
 ;   (format t "Source-basic mapping:~%~{~A~}" (loop for e in source-alphabet collect
;						   (format nil "~A: ~{~A~^, ~}~%" e
;							   (gethash e source-basic-mapping))))
;    (format t "Target-basic mapping:~%~{~A~}" (loop for e in target-alphabet collect
;						   (format nil "~A: ~{~A~^, ~}~%" e
;							   (gethash e target-basic-mapping))))
    ;; Construct the basic distribution
    (loop for source-element in source-alphabet do
	 (let* ((basic-elements (gethash source-element source-basic-mapping))
		(source-probability (nth 1 (assoc source-element source-distribution
						  :test #'equal)))
		(basic-probability (/ source-probability (length basic-elements))))
	   (dolist (be basic-elements)
	     (incf (gethash be basic-distribution 0) basic-probability))))
    ;; TODO: if target viewpoint == basic viewpoint skip the next step
    ;; Construct the target distribution from the basic distribution
    (loop for target-element in target-alphabet do
	 (let* ((basic-elements (gethash target-element target-basic-mapping))
		(basic-probabilities (mapcar (lambda (be)
					       (gethash be basic-distribution 0))
					     basic-elements))
		(target-probability (apply #'+ basic-probabilities)))
	   (push (list target-element target-probability) target-distribution)))
;    (format t "Target distribution: ~A~%" target-distribution)
    (make-event-prediction
     :target-viewpoint target-viewpoint
     :viewpoint source-viewpoint
     :order (prediction-sets:prediction-order source-prediction-set)
     :weights (prediction-sets:prediction-weights source-prediction-set)
     :event (car (last events))
     :element (viewpoint-element target-viewpoint events)
     :set target-distribution)))
   
