;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-             
;;;; ======================================================================
;;;; File:       multiple-viewpoint-system.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2003-04-27 18:54:17 marcusp>                           
;;;; Time-stamp: <2013-04-22 12:10:59 jeremy>                           
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
  ((basic :reader mvs-basic :initarg :basic :type list)
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
  (utils:string-append *ep-cache-dir* filename))

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
  (let ((e-id (md:ident e)))
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

0 - viewpoints derived from basic viewpoints in typeset of current viewpoint 
1 - viewpoints derived from basic viewpoint in the multiple viewpoint system
2 or NIL - any viewpoints 
a list   - a specified list of viewpoints 

See also VIEWPOINTS:SET-ALPHABET-FROM-CONTEXT."
  (let ((unconstrained 
         (case unconstrained
           ;(-1 viewpoint currently being predicted) 
           ;; viewpoints derived from basic viewpoints in typeset of
           ;; current viewpoint
           (0 (mapcar #'viewpoints:get-viewpoint 
                      (viewpoints:viewpoint-typeset viewpoint)))
           ;; viewpoints derived from basic viewpoints in MVS 
           (1 (mvs-basic m)) 
           ;; all basic viewpoints (and their derived viewpoints) 
           (2 (mapcar #'viewpoints:get-viewpoint viewpoints:*basic-types*))
           ;; specified list           
           (t unconstrained))))
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
           (mvs-basic m)))

(defmethod sequence-prediction-sets ((m mvs) events event-prediction-sets)
  (let ((index (md:get-composition-index (md:ident (car events)))))
    (apply #'mapcar
           (cons #'(lambda (&rest e)
                     (make-sequence-prediction :viewpoint (car e)
                                               :index index
                                               :set (cdr e)))
                 (cons (mvs-basic m) event-prediction-sets)))))

(defmethod dataset-prediction-sets ((m mvs) sequence-prediction-sets)
  (apply #'mapcar
         (cons #'(lambda (&rest c)
                   (make-dataset-prediction :viewpoint (car c)
                                            :set (cdr c)))
               (cons (mvs-basic m) sequence-prediction-sets))))


;;;========================================================================
;;; Model Initialisation 
;;;========================================================================

(defun make-mvs (basic-viewpoints viewpoints ltms)
  "Returns an mvs object initialised with <viewpoints>, a list of
viewpoint objects, <ltm> and <stm> each of which is a list of ppm
objects."
  (flet ((sanity-check-basic-viewpoints ()
           (dolist (bv basic-viewpoints basic-viewpoints)
             (unless (find (type-of bv) viewpoints 
                           :key #'viewpoints:viewpoint-typeset
                           :test #'(lambda (x y) (member x y)))
               (warn "~&None of the supplied viewpoints can predict basic feature ~A.~%"
                     (viewpoints:viewpoint-name bv)))))
         (sanity-check-viewpoints ()
           (dolist (v viewpoints viewpoints)
             (unless (some #'(lambda (bv) 
                               (find (type-of bv) 
                                     (viewpoints:viewpoint-typeset v)
                                     :test #'eql))
                           basic-viewpoints)
               (warn "~&Viewpoint ~A cannot predict any of the supplied basic features.~%"
                     (viewpoints:viewpoint-name v))))))
    ;;(format t "~&MAKE-MVS: basic-viewpoints = ~A~&MAKE-MVS: viewpoints = ~A"
    ;;        (sanity-check-basic-viewpoints) 
    ;;        (sanity-check-viewpoints))
    (let ((mvs (make-instance 'mvs
                              :basic (sanity-check-basic-viewpoints)
                              :viewpoints (apply #'vector 
                                                 (sanity-check-viewpoints))
                              :ltm (apply #'vector ltms)
                              :stm (get-short-term-models viewpoints))))
      (set-mvs-parameters mvs)
      mvs)))
                                   
(defmethod set-mvs-parameters ((m mvs) &key
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
                   
(defun get-short-term-models (viewpoints)
  "Returns a vector of freshly initialised ppm short term models
corresponding to the supplied list of viewpoints and initialised with
the supplied parameters."
  (apply #'vector (mapcar #'(lambda (v) (make-ppm (viewpoint-alphabet v)))
                          viewpoints)))


;;;========================================================================
;;; Model Construction and Prediction 
;;;========================================================================

(defmethod model-dataset ((m mvs) dataset &key construct? predict?)
  "Models a dataset <dataset> (a vector of sequence vectors) given the
multiple-viewpoint system <m>."
  (labels ((model-d (dataset sequence-index prediction-sets)
             (when *debug*
               (format t "~&Composition ~A~%" sequence-index))
             (if (null dataset) (reverse prediction-sets)
                 (let ((prediction-set (model-sequence m (car dataset) 
                                                       :construct? construct?
                                                       :predict? predict?)))
                   (unless (= sequence-index 1)
                     (operate-on-models m #'increment-sequence-front))
                   (operate-on-models m #'reinitialise-ppm :models 'stm)
                   (model-d (cdr dataset) (1- sequence-index)
                            (cons prediction-set prediction-sets))))))
    (dataset-prediction-sets m (model-d dataset (length dataset) '()))))

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
          (make-sequence 'vector viewpoint-count :initial-element *root*))
         (stm-locations
          (make-sequence 'vector viewpoint-count :initial-element *root*)))
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
          (setf ltm-locations ltm-next-locations
                stm-locations stm-next-locations)
          (operate-on-models m #'increment-event-front)
          (when (>= event-index predict-from)
            (let ((combined 
                   (combine-predictions (mvs-basic m) ltm-prediction-sets 
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
        ;(print (viewpoints:viewpoint-name viewpoint))
        (set-model-alphabets m event-array events viewpoint ltm stm 
                             *marginalise-using-current-event*)
        (unless (undefined-p event)
          ;;(when predict? 
          ;;  (format t "~&LTM: ~S" (viewpoints:viewpoint-name viewpoint)))
          (multiple-value-bind (ltm-next-location ltm-distribution)
              (ppm:ppm-model-event ltm event :location ltm-location 
                               :construct? (and construct? 
                                                (or (eq *models* :ltm+)
                                                    (eq *models* :both+)))
                               :predict? (and predict? 
                                              (or (eq *models* :ltm+)
                                                  (eq *models* :ltm)
                                                  (eq *models* :both)
                                                  (eq *models* :both+))))
            (setf (aref ltm-locations i) ltm-next-location)
            ;(format t "~&ltm-distribution = ~&~A~%" ltm-distribution)
            (push (make-event-prediction :viewpoint viewpoint
                                         :event (car (last events))
                                         :element event
                                         :set ltm-distribution)
                  ltm-prediction-sets))
          
          ;;(when predict? 
          ;;  (format t "~&STM: ~S" (viewpoints:viewpoint-name viewpoint)))
          (multiple-value-bind (stm-next-location stm-distribution)
              (ppm:ppm-model-event stm event :location stm-location 
                               :construct? (or (eq *models* :stm)
                                               (eq *models* :both) 
                                               (eq *models* :both+))
                               :predict? (and predict? 
                                              (or (eq *models* :stm)
                                                  (eq *models* :both) 
                                                  (eq *models* :both+))))
            (setf (aref stm-locations i) stm-next-location)
            (push (make-event-prediction :viewpoint viewpoint
                                         :event (car (last events))
                                         :element event
                                         :set stm-distribution)
                  stm-prediction-sets)))))
    (values ltm-locations stm-locations ltm-prediction-sets
            stm-prediction-sets)))


;;;========================================================================
;;; Combining event predictions
;;;========================================================================

(defun combine-predictions (basic-viewpoints ltm-prediction-sets 
                            stm-prediction-sets events)
  (case *models*
    ((or :ltm :ltm+)
     (combine-viewpoint-predictions basic-viewpoints ltm-prediction-sets 
                                    events :ltm))
    (:stm (combine-viewpoint-predictions basic-viewpoints stm-prediction-sets 
                                         events :stm))
    (otherwise
     (combine-ltm-stm-predictions
      (combine-viewpoint-predictions basic-viewpoints ltm-prediction-sets 
                                     events :ltm)
      (combine-viewpoint-predictions basic-viewpoints stm-prediction-sets 
                                     events :stm)))))

(defun combine-viewpoint-distributions (dists) 
  (combine-distributions dists *viewpoint-combination* *viewpoint-bias* :viewpoint))

(defun combine-ltm-stm-distributions (dists) 
  (combine-distributions dists *ltm-stm-combination* *ltm-stm-bias* :ltm-stm))

(defun combine-ltm-stm-predictions (ltm-predictions stm-predictions)
  ;; (format t "~&LTM-STM~%")
  (mapcar #'(lambda (l s) (combine-ltm-stm-distributions (list l s)))
          ltm-predictions stm-predictions))

(defun combine-viewpoint-predictions (basic-viewpoints prediction-sets 
                                      events model)
  ;; (format t "~&VIEWPOINTS: ~A~%" model)
  (flet ((basic-prediction-set (prediction-sets basic-viewpoint)
           (find-if #'(lambda (p) (viewpoints:viewpoint-equal p basic-viewpoint))
                    prediction-sets :key #'prediction-viewpoint))
         (derived-prediction-sets (prediction-sets basic-viewpoint)
           (remove-if #'(lambda (ps) 
                          (let ((v (prediction-viewpoint ps)))
                            ;;(format t "~&dps: ~A ~A" basic-viewpoint v)
                            (or (viewpoints:basic-p v) 
                                (not 
                                 (viewpoints:in-typeset-p basic-viewpoint v)))))
                      prediction-sets)))
    (let ((distributions '()))
      ;;(format t "~&prediction-sets = ~A~%" prediction-sets)
      (dolist (basic-viewpoint basic-viewpoints)
        (let* ((derived-prediction-sets
                (derived-prediction-sets prediction-sets basic-viewpoint))
               (basic-prediction-set
                (basic-prediction-set prediction-sets basic-viewpoint))
               (basic-distributions (unless (null basic-prediction-set) 
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
               (make-event-prediction
                :viewpoint basic-viewpoint
                :event (car (last events))
                :element (viewpoint-element basic-viewpoint events)
                :set (prediction-sets:flat-distribution (viewpoints:viewpoint-alphabet basic-viewpoint)))
               distributions)
              (progn 
                (dolist (derived-prediction-set derived-prediction-sets)
                  (let ((basic-distribution
                         (derived->basic derived-prediction-set basic-viewpoint
                                         events)))
;;                     (format t "~&Derived distribution: ~A~%" 
;;                             (prediction-set derived-prediction-set))
;;                     (format t "~&Basic distribution: ~A~%" 
;;                             (prediction-set basic-distribution))
                    (unless (null basic-distribution)
                      (cache-ep (prediction-set basic-distribution) model 
                                (viewpoint-type 
                                 (prediction-viewpoint derived-prediction-set))
                                (car (last events)))
                      (push basic-distribution basic-distributions))))
                (push (combine-viewpoint-distributions basic-distributions)
                      distributions)))))
      (reverse distributions))))

(defun derived->basic (derived-prediction-set basic-viewpoint events)
  "Given a prediction set <derived-prediction-set> for a derived
viewpoint returns a probability distribution over the alphabet of
<basic-viewpoint> (which is in the typeset of the derived viewpoint)
given a sequence of events <sequence>."
  (let* ((derived-viewpoint (prediction-viewpoint derived-prediction-set))
         (derived-distribution (prediction-set derived-prediction-set))
         (basic-alphabet (viewpoint-alphabet basic-viewpoint))
         (basic-distribution (make-hash-table :test #'equal)))
    (when *debug* 
      (format t "~&~A (~A) = ~&~A~%" (viewpoint-name derived-viewpoint) 
              (prediction-element derived-prediction-set) derived-distribution))
    ;;(format t "~&~A: ~A~%" basic-viewpoint basic-alphabet)
;;     (format t "~&Viewpoint: ~A; element: ~A~%" 
;;            (viewpoint-type derived-viewpoint)
;;            (prediction-element derived-prediction-set))
    (if (viewpoints:inverse-viewpoint-function-defined-p derived-viewpoint)
        (dolist (ep derived-distribution) 
          (let* ((e (nth 0 ep))
                 (p (nth 1 ep))
                 (basic-elements 
                  (viewpoints:basic-element derived-viewpoint basic-viewpoint 
                                            e events))
                 (p (unless (or (null basic-elements) (undefined-p e)) 
                      (/ p (length basic-elements)))))
            ;(format t "~&e = ~A; old-p = ~A; l = ~A; p = ~A~%" 
            ;        e (nth 1 ep) (length basic-elements) p)
            (dolist (be basic-elements) 
              (if (gethash be basic-distribution)
                  (incf (gethash be basic-distribution) p)
                  (setf (gethash be basic-distribution) p)))))
        (let ((mapping (mapping derived-viewpoint basic-viewpoint events)))
          (dolist (map mapping)
            (let* ((derived-element (car map))
                   (basic-elements (nth 1 map))
                   (probability (nth 1 (assoc derived-element derived-distribution
                                              :test #'equal)))
                   (basic-probability (/ probability (length basic-elements))))
              (dolist (be basic-elements)
                (if (gethash be basic-distribution)
                    (incf (gethash be basic-distribution) basic-probability)
                    (setf (gethash be basic-distribution) basic-probability)))))))
    (let ((viewpoint-element (viewpoint-element basic-viewpoint events))
          (distribution 
           (normalise-distribution 
            (mapcar #'(lambda (a) (list a (gethash a basic-distribution)))
                    basic-alphabet))))
      (when *debug* 
        (format t "~&~A (~A) = ~&~A~%" (viewpoint-name basic-viewpoint)
                viewpoint-element distribution))
      (make-event-prediction
       :viewpoint basic-viewpoint
       :event (car (last events))
       :element viewpoint-element
       :set distribution))))

(defun mapping (derived-viewpoint basic-viewpoint events)
  "Returns an alist whose keys are the elements of the alphabet of
<derived-viewpoint> and whose values are lists containing those
elements of the alphabet of <basic-viewpoint> which map to the
relevent derived alphabet given a list of events <sequence> and a set
of single event continuations over the basic alphabet."
  (let* ((derived-alphabet (viewpoint-alphabet derived-viewpoint))
         (continuations (viewpoints:alphabet->events basic-viewpoint events))
         (mappings '()))
    (dolist (derived-element derived-alphabet mappings)
      (let ((mapping '()))
        (dolist (continuation continuations)
          (let* ((temp-comp (append (butlast events) (list continuation)))
                 (viewpoint-element
                  (viewpoint-element derived-viewpoint temp-comp))
                 (basic-element (viewpoint-element basic-viewpoint temp-comp)))
            (when (viewpoints:viewpoint-element-equal basic-viewpoint
                                           derived-viewpoint
                                           viewpoint-element
                                           derived-element)
              (push basic-element mapping))))
        (unless (null mapping)
          (push (list derived-element mapping) mappings))))))
   
