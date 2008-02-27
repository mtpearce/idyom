;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-             
;;;; ======================================================================
;;;; File:       ppm-star.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2002-07-02 18:54:17 marcusp>                           
;;;; Time-stamp: <2008-01-25 15:30:22 marcusp>                           
;;;; ======================================================================
;;;;
;;;; DESCRIPTION 
;;;;
;;;;   Implements the construction of a PPM* finite-context model
;;;;   following the suffix-tree implementation described by Bunton
;;;;   (1996) and using Ukkonen's (1995) online left-to-right suffix
;;;;   tree construction algorithm. The construction of generalised 
;;;;   suffix trees is supported using the the sentinel symbol, $. 
;;;;   Printing model trees to postscript is supported via the 
;;;;   PSGRAPH package. 
;;;;
;;;; EXAMPLES 
;;;;
;;;;   (defun test-model (sequences alphabet &key (ps t))
;;;;     (let ((model (ppm:make-ppm alphabet :escape :c :mixtures t
;;;;                                :update-exclusion nil :order-bound nil)))
;;;;       (prog1 (ppm:model-dataset model sequences :construct? t :predict t)
;;;;         (when ps (ppm:write-model-to-postscript model)))))
;;;; 
;;;;   (test-model '((a b r a c a d a b r a)) '(a b c d r))
;;;;   (test-model '((l e t l e t t e r t e l e)) '(e l t r))
;;;;   (test-model '((a s s a n i s s i m a s s a)) '(a i m n s))
;;;;   (test-model '((m i s s i s s i p p i)) '(i m p s))
;;;;   (test-model '((w o o l o o b o o l o o)) '(b l o w))
;;;;   (test-model '((k k k k k k k k k)) '(k))
;;;;   (test-model '((a a a a a a a a b)) '(a b))
;;;;   (test-model '((a g c g a c g a g)) '(a c g))
;;;;   (test-model '((a b a b c)) '(a b c))
;;;;
;;;;   (test-model '((a b r a c a d a b r a $)
;;;;   	             (l e t l e t t e r t e l e $)
;;;;   	             (a s s a n i s s i m a s s a $)
;;;;   	             (m i s s i s s i p p i $)
;;;;   	             (w o o l o o b o o l o o $))
;;;;               '(a b c d e i l m n o p r s t w $))
;;;; 
;;;;   (test-model '((a b r a c a d a b r a $) (a b r a c a d a b r a $))
;;;;               '(a b c d r $))
;;;; 
;;;; TODO 
;;;;
;;;;   - Bunton improvements: state-selection;
;;;;   - Other smoothing techniques (Katz, Kneser-Ney); 
;;;;   - Other modelling techniques (PST, LZ, BW, DMC). 
;;;;
;;;; ======================================================================

(cl:defpackage #:ppm-star
  (:use #:cl) ;; #:psgraph #:utils)
  (:nicknames #:ppm)
  (:export "PPM" "*ROOT*" "MAKE-PPM" "REINITIALISE-PPM" "SET-PPM-PARAMETERS"
           "SET-ALPHABET" "INCREMENT-SEQUENCE-FRONT" "INCREMENT-EVENT-FRONT"
           "MODEL-DATASET" "MODEL-SEQUENCE" "MODEL-EVENT"
           "MODEL-SENTINEL-EVENT" "INITIALISE-VIRTUAL-NODES"
           "WRITE-MODEL-TO-POSTSCRIPT" "WRITE-MODEL-TO-FILE"
           "READ-MODEL-FROM-FILE" "GET-MODEL")
  (:documentation "Prediction by Partial Match modelling including
methods for model initialisation, construction and prediction."))

(cl:in-package #:ppm)

(defvar *earth* nil "The order -1 context.")
(defvar *root*  nil "The order 0 context.")
(defvar *sentinel* '$ "The unique end of sequence symbol.")
(defun sentinel-p (s) (eql s *sentinel*))

(defun eequal (obj1 obj2) 
  "Predicate for comparing elements of sequences."
  (equal obj1 obj2))

(defmacro with-element-comparator (fun &body body)
  `(let ((old-fun (symbol-function 'ppm::eequal))) 
    (setf (symbol-function 'ppm::eequal) ,fun)
    (prog1 (progn ,@body)
      (setf (symbol-function 'ppm::eequal) old-fun))))

;;;===========================================================================
;;; Distributions 
;;;===========================================================================

(defun distribution-symbols (distribution) 
  (mapcar #'car distribution))

(defun distribution-probabilities (distribution) 
  (mapcar #'cadr distribution))

(defun normalise-distribution (distribution)
  "Normalises <distribution> such that it sums to one."
  (if (sums-to-one-p distribution) distribution
      (let* ((sum (the double-float (sum-distribution distribution)))
             (scaling-factor (if (zerop sum) 1.0d0 (/ 1.0d0 sum))))
        (declare (type double-float scaling-factor))
        (mapcar #'(lambda (ep) 
                    (let ((e (nth 0 ep)) (p (nth 1 ep)))
                      (declare (type double-float p))
                      (list e (* p scaling-factor))))
                distribution))))

(defun sum-distribution (distribution)
  "Returns the sum of <distribution> a set of probabilities distributed
   over an alphabet."
  (declare (values double-float))
  (the double-float (apply #'+ (distribution-probabilities distribution))))

(defun sums-to-one-p (distribution)
  "Returns true if <distribution> sums to one otherwise nil."
  (< 0.999d0 (sum-distribution distribution) 1.0d0))
        
;;;===========================================================================
;;; Data Structures
;;;===========================================================================

(defclass ppm ()
  ((leaves    :accessor ppm-leaves   :initarg :leaves   :type hash-table)
   (branches  :accessor ppm-branches :initarg :branches :type hash-table)
   (front     :accessor ppm-front    :initarg :front    :type index)
   (dataset   :reader ppm-dataset    :initarg :dataset  :type hash-table)
   ;;parameters used in construction 
   (leaf-index    :accessor ppm-leaf-index :initarg :leaf-index
                  :type (integer 0 *))
   (branch-index  :accessor ppm-branch-index :initarg :branch-index
                  :type (integer 0 *))
   (virtual-nodes :accessor ppm-virtual-nodes :initarg :virtual-nodes
                  :type hash-table)
   ;;parameters used in prediction 
   (alphabet :accessor ppm-alphabet :initarg :alphabet :type list)
   (update-exclusion :accessor ppm-update-exclusion :initarg :update-exclusion
                     :type (or null symbol))
   (mixtures :accessor ppm-mixtures :initarg :mixtures :type (or null symbol))
   (order-bound :accessor ppm-order-bound :initarg :order-bound
                :type (or null symbol))
   (escape :accessor ppm-escape :initarg :escape :type (or null symbol))
   (k :accessor ppm-k :initarg :k :type integer)
   (d :accessor ppm-d :initarg :d :type integer))
  (:documentation "A ppm object contains all the parameters required for
   constructing and predicting from a ppm* model."))

(defstruct branch-record
  "A branching node contains references to its right brother and first
   child. <count0> is the number of times this subsequence appears in
   *sequence*. <count1> is the update excluded count.  <slink> is a
   reference to the node representing the subsequence of *sequence* with
   the first character removed. <label> is a label object representing the
   label of the branch and <depth> is a natural number representing the
   depth of the branch in the tree in terms of symbols."
  (label   nil   :type (or null label))
  (brother nil   :type (or null branch leaf))
  (child   nil   :type (or null branch leaf))
  (slink   nil   :type (or null branch))
  (depth   0     :type (integer -1 *)) 
  (count0  1     :type (integer 0 *)) 
  (count1  1     :type (integer 0 *)))

(defstruct leaf-record
  "A leaf node contains a reference to its first <brother> as well as a
   <label> and full counts <count0> and update excluded counts <count1>."
  (label   nil   :type (or null label))
  (brother nil   :type (or null branch leaf))
  (count0  1     :type (integer 0 *)) 
  (count1  1     :type (integer 0 *))) 

(defstruct branch
  "An index into the branch vector of a ppm model."
  (index 0 :type (integer 0 *))) 

(defstruct leaf
  "An index into the leaf vector of a ppm model."
  (index 0 :type (integer 0 *))) 

(defstruct location
  "The location of a symbol sequence in the model is represented as an
   edge between <node> and <child> where <match> is the matched portion
   of the edge label and <rest> is the remaining portion."
  (node  nil  :type (or null branch))
  (match nil  :type (or null label))
  (rest  nil  :type (or null label))
  (child nil  :type (or null branch leaf)))

(defstruct label
  "The label of a node is represented as a subsequence of the dataset of
   length <length> beginning at index <left>."
  (left   (make-index) :type index)
  (length nil          :type (or null integer)))

(defstruct index
  "An index into the data where an event is uniquely identifyed by its
   sequence-id <s> and event-id <e>."
  (s 0 :type (integer 0 *))
  (e 0 :type (integer 0 *)))


;;;===========================================================================
;;; Various utility methods and functions
;;;===========================================================================

(defmethod dataset-length ((m ppm))
  "Returns the number of sequences in (ppm-dataset <m>)."
  (hash-table-count (ppm-dataset m)))

(defmethod dataset-sequence-length ((m ppm) sequence-index)
  "Returns the number of events in the sequence in (ppm-dataset <m>)
   indexed by <sequence-index>."
  (hash-table-count (gethash sequence-index (ppm-dataset m))))

(defmethod set-model-front ((m ppm) sequence-index event-index)
  "Sets (ppm-front <m>) the current index into the sequence to <index>."
  (set-sequence-front m sequence-index)
  (set-event-front m event-index))

(defmethod set-event-front ((m ppm) event-index)
  "Set the current index into the sequence to <event-index>."
  (setf (index-e (ppm-front m)) event-index))

(defmethod set-sequence-front ((m ppm) sequence-index)
  "Set the current index into the sequence to <event-index>."
  (setf (index-s (ppm-front m)) sequence-index))

(defmethod increment-event-front ((m ppm))
  "Increments the index into the event table by one."
  (incf (index-e (ppm-front m))))

(defmethod increment-sequence-front ((m ppm))
  "Increments the sequence index into dataset by one and sets the
   event index to zero."
  (incf (index-s (ppm-front m)))
  (set-event-front m 0))

(defmethod set-alphabet ((m ppm) alphabet)
  "Sets the alphabet slot in ppm model <m> to <alphabet>."
  (setf (ppm-alphabet m) alphabet))

(declaim (inline set-branch-record-child))
(defun set-branch-record-child (branch-record child)
  "Sets the child of <branch-record> to <child>." 
  (setf (branch-record-child branch-record) child))

(declaim (inline set-branch-record-slink))
(defun set-branch-record-slink (branch-record slink)
  "Sets the suffix link of <branch-record> to <slink>." 
  (setf (branch-record-slink branch-record) slink))

(declaim (inline set-record-brother))
(defun set-record-brother (record brother)
  "Sets the brother of <record> to <brother>." 
  (if (leaf-record-p record)
      (setf (leaf-record-brother record) brother)
      (setf (branch-record-brother record) brother)))

(declaim (inline increment-node-record-count))
(defun increment-node-record-count (node-record excluded)
  "Increments the count of <node-record> by one. If <excluded> is non-nil
   the update-excluded count is incremented."
  (case (type-of node-record)
    (leaf-record (if excluded (incf (leaf-record-count1 node-record))
                     (incf (leaf-record-count0 node-record))))
    (branch-record (if excluded (incf (branch-record-count1 node-record))
                       (incf (branch-record-count0 node-record))))))

(declaim (inline root-p))
(defun root-p (branch)
  "A branch <branch> is the root if its index is 1."
  (when (branch-p branch) (= (branch-index branch) 1)))

(declaim (inline earth-p))
(defun earth-p (branch)
  "A branch <branch> is the earth if its index is 0."
  (when (branch-p branch) (= (branch-index branch) 0)))

(defmethod alphabet-size ((m ppm))
  "Returns the cardinality of the alphabet of model <m>."
  (length (ppm-alphabet m)))

(defmethod drop ((m ppm) n label)
  "Returns a copy of <label> with the first <n> symbols removed."
  (let* ((old-left (label-left label))
         (new-left (make-index :s (index-s old-left)
                               :e (+ (index-e old-left) n)))
         (old-length (label-length label))
         (new-length (if (null old-length) old-length (- old-length n))))
    (make-label :left new-left :length new-length)))

(defmethod empty-p ((m ppm) label)
  "A <label> is empty if its length is less than one."
  (<= (get-length m label) 0))

(defmethod increment-branch-index ((m ppm))
  "Increments the current branch-index by one."
  (incf (ppm-branch-index m)))
  
(defmethod increment-leaf-index ((m ppm))
  "Increments the current leaf-index by one."
  (incf (ppm-leaf-index m)))

(defmethod set-branch-record ((m ppm) index record)
  "Sets the branch in model <m>  at position <index> to <record>."
  (let ((branches (ppm-branches m)))
    (setf (gethash index branches) record)))

(defmethod set-leaf-record ((m ppm) index record)
  "Sets the leaf in model <m> at position <index> to <record>."
  (let ((leaves (ppm-leaves m)))
    (setf (gethash index leaves) record)))

(defmethod get-label ((m ppm) branch-or-leaf)
  "Returns the label of <branch-or-leaf> in <m>." 
  (if (branch-p branch-or-leaf)
      (branch-record-label (get-record m branch-or-leaf))
      (leaf-record-label (get-record m branch-or-leaf))))

(defmethod set-label ((m ppm) branch-or-leaf label)
  "Sets the label of <branch-or-leaf> to be <label>."
  (if (branch-p branch-or-leaf)
      (setf (branch-record-label (get-record m branch-or-leaf)) label)
      (setf (leaf-record-label (get-record m branch-or-leaf)) label)))

(defmethod get-brother ((m ppm) branch-or-leaf)
  "Returns the brother of <branch-or-leaf>"
  (if (branch-p branch-or-leaf)
      (branch-record-brother (gethash (branch-index branch-or-leaf)
                                      (ppm-branches m)))
      (leaf-record-brother (gethash (leaf-index branch-or-leaf)
                                    (ppm-leaves m)))))

(defmethod get-record ((m ppm) branch-or-leaf)
  "Returns the node record of <branch-or-leaf> in ppm model <m>."
  (if (branch-p branch-or-leaf)
      (gethash (branch-index branch-or-leaf) (ppm-branches m))
      (gethash (leaf-index branch-or-leaf) (ppm-leaves m))))

(defmethod get-count ((m ppm) branch-or-leaf &optional (excluded nil))
  "Returns the count associated with <branch-or-leaf>."
  (if excluded
      (if (branch-p branch-or-leaf)
          (branch-record-count1 (get-record m branch-or-leaf))
          (leaf-record-count1 (get-record m branch-or-leaf)))
      (if (branch-p branch-or-leaf)
          (branch-record-count0 (get-record m branch-or-leaf))
          (leaf-record-count0 (get-record m branch-or-leaf)))))

(defmethod get-node-index ((m ppm) branch-or-leaf)
  "Returns the array index of <branch-or-leaf>."
  (if (branch-p branch-or-leaf) (branch-index branch-or-leaf)
      (leaf-index branch-or-leaf)))

(defmethod instantiate-label ((m ppm) label)
  "Instantiates a <label> with values from the sequence of model <m>."
  (let* ((label-left (label-left label))
         (sequence-index (index-s label-left))
         (event-index (index-e label-left))
         (length (get-length m label))
         (label-list '()))
    (dotimes (e length)
      (let ((index (make-index :s sequence-index :e (+ e event-index))))
        (push (get-symbol m index) label-list)))
    (reverse label-list)))

(defmethod get-length ((m ppm) label)
  "Returns the length of <label>." 
  (let* ((left (label-left label))
         (length (label-length label)))
    (if (null length)
        (let* ((current-seq-index (index-s (ppm-front m)))
               (label-seq-index (index-s left))
               (label-event-index (index-e left))
               (event-id
                (if (= current-seq-index label-seq-index)
                    (index-e (ppm-front m))
                    (dataset-sequence-length m label-seq-index))))
          (if (>= event-id label-event-index)
              (- event-id label-event-index)
              event-id))
        (if (> length 0) length 0))))

(defmethod get-order ((m ppm) location)
  "Returns the order of <location> (NB not label length) in <M>."
  (if (branch-p location) (branch-record-depth (get-record m location))
      (let* ((node (location-node location))
             (match (location-match location)))
        (+ (branch-record-depth (get-record m node)) (label-length match)))))

(defmethod get-matching-child ((m ppm) node symbol)
  "Returns the child of <node> the first symbol of whose label matches symbol."
  (labels ((get-matching-brother (child)
             (cond ((null child) nil)
                   ((eequal (get-symbol m (label-left (get-label m child)))
                            symbol)
                    child)
                   (t (get-matching-brother (get-brother m child))))))
    (let ((child (if (branch-p node) (branch-record-child
                                      (get-record m node)))))
      (get-matching-brother child))))

(defmethod get-matching-brother ((m ppm) first-child match)
  "Returns the brother of <first-child> whose brother matches <match>."
  (labels ((get-next-brother (child brother)
             (cond ((null child) nil)
                   ((equal brother match) child)
                   (t (get-next-brother brother (get-brother m child))))))
    (let ((brother (if (null first-child) nil (get-brother m first-child))))
      (get-next-brother first-child brother))))

(defmethod list-children ((m ppm) node)
  "Returns a list of the children of <node>."
  (labels ((add-next-brother (child result)
             (if (null child) (reverse result)
                 (add-next-brother (get-brother m child) (cons child result)))))
    (let ((child (when (branch-p node) (branch-record-child (get-record m node)))))
      (add-next-brother child '()))))

(defmethod get-symbol ((m ppm) index)
  "Returns the symbol specified by the database index <index>"
  (gethash (index-e index) (gethash (index-s index) (ppm-dataset m))))

(defmethod add-event-to-model-dataset ((m ppm) symbol)
  "Adds <symbol> to the end of the vector holding the current sequence."
  (let ((seq-hash (gethash (index-s (ppm-front m)) (ppm-dataset m))))
    (unless (hash-table-p seq-hash) 
      (setf (gethash (index-s (ppm-front m)) (ppm-dataset m))
            (make-hash-table)))
    (setf (gethash (index-e (ppm-front m))
                   (gethash (index-s (ppm-front m)) (ppm-dataset m)))
          symbol)))


;;;===========================================================================
;;; Initialisation 
;;;===========================================================================

(defun make-ppm (alphabet &key (mixtures t) (escape :c) (order-bound nil)
                          (update-exclusion nil) (dataset nil) (leaves nil)
                          (branches nil))
  "Returns a PPM* model initialised with the supplied parameters."
  (multiple-value-bind (k d)
      (case escape
        (:a (values 0 1))
        (:b (values -1 1))
        ((or :c :x) (values 0 1))
        (:d (values -1/2 2))
        (otherwise (values 0 1)))
    (setf *earth* (make-branch :index 0) *root* (make-branch :index 1))
    (let* ((dataset (if (null dataset) (make-hash-table) dataset))
           (front (make-index :s (hash-table-count dataset) :e 0))
           (initial-leaves (if (null leaves) (make-hash-table) leaves))
           (initial-branches (if (null branches) (make-hash-table) branches))
           (leaf-index (hash-table-count initial-leaves))
           (branch-index (hash-table-count initial-branches))
           (virtual-nodes (make-hash-table :test #'equalp))
           (model (make-instance 'ppm 
                                 :leaves initial-leaves
                                 :branches initial-branches
                                 :front front 
                                 :dataset dataset 
                                 :leaf-index leaf-index
                                 :branch-index branch-index
                                 :virtual-nodes virtual-nodes
                                 :alphabet alphabet
                                 :update-exclusion update-exclusion
                                 :mixtures mixtures
                                 :order-bound order-bound
                                 :escape escape
                                 :d d
                                 :k k)))
      (when (and (null leaves) (null branches)) (initialise-nodes model))
      model)))

(defmethod reinitialise-ppm ((m ppm))
  "Reinitialises the parameters of <m> used in model construction."
  (initialise-virtual-nodes m)
  (initialise-nodes m))

(defmethod initialise-nodes ((m ppm))
  "Initialises the leaves, branches, leaf-index and branch-index slots of
   ppm model <m>."
  (let* ((root-label (make-label :left (make-index) :length 0))
         (root-record (make-branch-record :label root-label :slink *earth*
                                          :count0 0 :count1 0))
         (earth-record (make-branch-record :child *root* :depth -1
                                           :count0 0 :count1 0)))
    (setf (gethash 0 (ppm-branches m)) earth-record
          (gethash 1 (ppm-branches m)) root-record
          (ppm-branch-index m) 2
          (ppm-leaf-index m) 0)))

(defmethod initialise-virtual-nodes ((m ppm))
  "Initialises the <virtual-nodes> slot of ppm model <m>."
  (setf (ppm-virtual-nodes m) (make-hash-table :test #'equalp)))

(defmethod set-ppm-parameters ((m ppm) &key (mixtures t) (escape :c)
                                 (order-bound nil) (update-exclusion nil))
  (multiple-value-bind (k d)
      (case escape
        (:a (values 0 1))
        (:b (values -1 1))
        ((or :c :x) (values 0 1))
        (:d (values -1/2 2))
        (otherwise (values 0 1)))
    (setf (ppm-mixtures m) mixtures
          (ppm-order-bound m) order-bound
          (ppm-update-exclusion m) update-exclusion
          (ppm-escape m) escape
          (ppm-k m) k
          (ppm-d m) d)))

  
;;;===========================================================================
;; Model Construction and Prediction 
;;;===========================================================================

(defmethod model-dataset ((m ppm) dataset &key construct? predict? initialise?)
  "Models a dataset <dataset> (a vector of sequence vectors) given
   PPM model <m>. If <construct?> is non-nil the dataset is added to the
   model and if <predict?> is non-nil a distribution over (ppm-alphabet m)
   is returned for each event in each sequence in <dataset>."
  (labels ((model-d (dataset sequence-index prediction-sets)
             (if (null dataset) (reverse prediction-sets)
                 (let ((prediction-set
                        (model-sequence m (car dataset) :construct? construct? 
                                        :predict? predict?))
                       (index-s (index-s (ppm-front m))))
                   (unless (= sequence-index 1) (increment-sequence-front m))
                   (when initialise? (reinitialise-ppm m))
                   (model-d (cdr dataset)
                            (1- sequence-index)
                            (cons (cons index-s prediction-set)
                                  prediction-sets))))))
    (model-d dataset (length dataset) '())))
                   
(defmethod model-sequence ((m ppm) sequence &key construct? predict?)
  "Models a sequence <sequence> (a vector of symbols) given PPM
   model <m>. If <construct?> is non-nil the sequence is added to the
   model and if <predict?> is non-nil a distribution over (ppm-alphabet m)
   is returned for each event in the sequence. The model's index into
   the vector of sequences must be set to the appropriate sequence
   index before this method is called."
  (labels ((model-seq (sequence location prediction-set)
             (if (null sequence)
                 (progn
                   (when construct? (model-sentinel-event m location))
                   (reverse prediction-set))
                 (let ((symbol (car sequence)))
                   (multiple-value-bind (next-location event-distribution)
                       (model-event m symbol :location location 
                                    :construct? construct? :predict? predict?)
                     (increment-event-front m)
                     (model-seq (cdr sequence) next-location
                                (cons (list symbol event-distribution)
                                      prediction-set)))))))
    (prog1 (model-seq sequence *root* '())
      (when construct? (initialise-virtual-nodes m)))))

(defmethod model-sentinel-event ((m ppm) location)
  (add-event-to-model-dataset m *sentinel*) 
  (ukkstep m nil location *sentinel* t))

(defmethod model-event ((m ppm) symbol &key location construct? predict?)
  "Models an event <symbol> appear at location <location> in the ppm
   model <m>. If <construct?> is non-nil the event is added to the model
   and if <predict?> is non-nil a distribution over (ppm-alphabet m)
   is returned for <location>.  The model's index into the sequence
   vector must be set to the appropriate event index before this method
   is called."
  (add-event-to-model-dataset m symbol)
  (let* ((novel? (when construct? (unless (occurs? m location symbol) t)))
         (distribution (when predict? (get-distribution m location)))
         (next-location (ukkstep m nil location symbol construct?)))
    (when construct? (increment-counts m next-location novel?))
    (values next-location distribution)))


;;;===========================================================================
;;; Moving around and constructing ppm models
;;;===========================================================================

(defmethod ukkstep ((m ppm) node location symbol construct?)
  "Updates the suffix link of node and inserts the relevant suffixes of
   the current prefix of the current sequence in the dataset into <m>."
  (cond ((occurs? m location symbol)
         (when construct? (update-slink m node location :occurs? t))
         (canonise m location (make-label :left (ppm-front m) :length 1)))
        ((root-p location)
         (when construct?
           (insert-relevant-suffix m location)
           (update-slink m node location))
         location)
        (t (if construct? 
               (let ((slink (insert-relevant-suffix m location)))
                 (update-slink m node location :slink slink)
                 (ukkstep m slink (get-next-location m location) symbol t))
               (ukkstep m node (get-next-location m location) symbol nil)))))
               

(defmethod update-slink ((m ppm) node location &key occurs? slink)
  "Updates the suffix link of <node>."
  (if (null node) nil
      (let ((node-record (get-record m node))
            (location (if (location-p location) (location-node location)
                          location)))
        (if (or occurs? (null slink))
            (set-branch-record-slink node-record location)
            (set-branch-record-slink node-record slink)))))

(defmethod occurs? ((m ppm) location symbol)
  "Returns t if the current input symbol occurs in <location>, else nil."
  (if (location-p location)
      (when (eequal (get-symbol m (label-left (location-rest location)))
                   symbol)
        location)
      (get-matching-child m location symbol)))

(defmethod canonise ((m ppm) location sequence)
  "Returns the deepest possible location of <sequence> in <m> from
   the start location <location>."
  (cond ((empty-p m sequence) location)
        ((branch-p location)
         (let ((sequence-length (label-length sequence))
               (child (get-matching-child
                       m location (get-symbol m (label-left sequence)))))
           (if (null child) location
               (let* ((child-label (get-label m child))
                      (child-label-length (get-length m child-label))
                      (match (make-label :left (label-left child-label)
                                         :length sequence-length))
                      (rest (drop m sequence-length child-label)))
                 (if (<= sequence-length child-label-length)
                     (if (and (branch-p child) (empty-p m rest)) child
                         (make-location :node location
                                        :match match
                                        :rest rest
                                        :child child))
                     (if (branch-p child)
                         (canonise m child (drop m child-label-length sequence))
                         (canonise m (make-location :node location
                                                    :match match
                                                    :rest rest
                                                    :child child)
                                   (drop m child-label-length sequence))))))))
        (t (let* ((sequence-length (label-length sequence))
                  (match (location-match location))
                  (rest (location-rest location))
                  (rest-length (get-length m rest))
                  (child (location-child location))
                  (new-match (make-label :left (label-left match)
                                         :length (+ (label-length match)
                                                    sequence-length)))
                  (new-rest (drop m sequence-length rest)))
             (if (or (<= sequence-length rest-length) (leaf-p child))
                 (if (and (branch-p child) (empty-p m new-rest)) child
                     (make-location :node (location-node location)
                                    :match new-match
                                    :rest new-rest
                                    :child child))
                 (canonise m child (drop m rest-length sequence)))))))

(defmethod get-next-location ((m ppm) location)
  "Returns the next-location on successive iterations of <ukkstep> using
   suffix links from <location>."
  (if (branch-p location)
      (branch-record-slink (get-record m location))
      (if (root-p (location-node location))
          (canonise m (location-node location)
                    (drop m 1 (location-match location)))
          (canonise m (branch-record-slink
                       (get-record m (location-node location)))
                    (location-match location)))))

(defmethod insert-relevant-suffix ((m ppm) location)
  "Inserts the current relevant suffix by adding the appropriate nodes at
   <location>."
  (if (branch-p location)
      (progn (insert-leaf m location) nil)
      (let ((new-node (split-location m location)))
        (insert-leaf m new-node)
        new-node)))

(defmethod insert-leaf ((m ppm) branch)
  "Adds a new leaf node to <branch> in model <m>." 
  (let* ((parent-record (get-record m branch))
         (first-child (branch-record-child parent-record))
         (last-child (get-matching-brother m first-child nil))
         (new-leaf (make-leaf :index (ppm-leaf-index m)))
         (front (ppm-front m))
         (new-leaf-label (make-label :left (make-index :s (index-s front)
                                                       :e (index-e front))))
         (new-leaf-record (make-leaf-record :label new-leaf-label)))
    (set-leaf-record m (ppm-leaf-index m) new-leaf-record)
    (increment-leaf-index m)
    (if (null first-child) 
        (set-branch-record-child parent-record new-leaf)
        (set-record-brother (get-record m last-child) new-leaf))))

(defmethod split-location ((m ppm) location)
  "Splits <location> in model <m> into two nodes with labels
   corresponding to the matched and remaining portions of the label of
   the transition which it represents and returns the former."
  (let* ((parent-record (get-record m (location-node location)))
         (parent-depth (branch-record-depth parent-record))
         (parent-child (branch-record-child parent-record))
         (match-length (label-length (location-match location)))
         (rest-length (label-length (location-rest location)))
         (node (location-child location))
         (node-record (get-record m node))
         (node-index (get-node-index m node))
         (new-node (make-branch :index (ppm-branch-index m)))
         (node-label-left (label-left (get-label m node)))
         (new-node-count0 (get-count m node))
         (new-node-count1 (get-count m node t))
         (new-node-depth (+ parent-depth match-length))
         (new-child-count0 (get-virtual-node-count m location))
         (new-child-count1 (get-virtual-node-count m location t))
         (matching-brother (get-matching-brother m parent-child node))
         (new-node-label (make-label :left (make-index
                                            :s (index-s node-label-left)
                                            :e (index-e node-label-left))
                                     :length match-length))
         (new-child-label
          (make-label :left (make-index
                             :s (index-s node-label-left)
                             :e (+ (index-e node-label-left) match-length))
                      :length (label-length (location-rest location))))
         (new-child-record
          (if (leaf-p node)
              (make-leaf-record :label new-child-label
                                :count0 new-child-count0
                                :count1 new-child-count1)
              (make-branch-record :label new-child-label
                                  :child (branch-record-child node-record)
                                  :slink (branch-record-slink node-record)
                                  :depth (+ rest-length new-node-depth)
                                  :count0 new-child-count0
                                  :count1 new-child-count1)))
         (new-node-record
          (if (leaf-p node)
              (make-branch-record :label new-node-label 
                                  :child (make-leaf :index node-index)
                                  :brother (leaf-record-brother node-record)
                                  :depth new-node-depth 
                                  :count0 new-node-count0
                                  :count1 new-node-count1)
              (make-branch-record :label new-node-label
                                  :child (make-branch :index node-index)
                                  :brother (branch-record-brother node-record)
                                  :depth new-node-depth 
                                  :count0 new-node-count0
                                  :count1 new-node-count1))))
    (if (leaf-p node)
        (set-leaf-record m node-index new-child-record)
        (set-branch-record m node-index new-child-record))
    (set-branch-record m (ppm-branch-index m) new-node-record)
    (increment-branch-index m)
    (if (or (null parent-child) (equal parent-child node))
        (set-branch-record-child parent-record new-node)
        (unless (null matching-brother)
          (set-record-brother (get-record m matching-brother) new-node)))
    new-node))


;;;===========================================================================
;;; Maintaining counts 
;;;===========================================================================

(defmethod increment-counts ((m ppm) location novel?)
  "Increments the counts for states on the chain of suffix links
   from <location> after allocating virtual-nodes for any string
   transitions existing in the chain. If <novel?> is non-null then the
   current symbol is novel at an excited suffix child of <location> and
   the update excluded count is incremented for <location>."
  (labels ((increment-count (location up-ex)
             (if (branch-p location)
                 (let ((branch-record (get-record m location)))
                   (unless (gethash location (ppm-virtual-nodes m))
                     (increment-node-record-count branch-record up-ex)))
                 (let ((child-record (get-record m (location-child location)))
                       (match (location-match location)))
                   (when (= (label-length match) 1)
                     (increment-node-record-count child-record up-ex)))))
           (allocate-virtual-node (location vn)
             (let* ((child (location-child location))
                    (vnode (gethash child (ppm-virtual-nodes m))))
               (if vnode
                   (setf (gethash child vn)
                         (make-virtual-node
                          :count0 (virtual-node-count0 vnode)
                          :count1 (virtual-node-count1 vnode)))
                   (setf (gethash child vn)
                         (make-virtual-node
                          :count0 (get-count m child)
                          :count1 (get-count m child t)))))
             vn)
           (increment-suffix-count (location vn)
             (if (root-p location)
                 (progn (increment-count location nil) vn)
                 (let ((next-location (get-next-location m location))
                       (vn (if (branch-p location) vn
                               (allocate-virtual-node location vn))))
                   (increment-count location nil)
                   (increment-suffix-count next-location vn)))))
    (let* ((vn (make-hash-table :test #'equalp))
           (vn (increment-suffix-count location vn)))
      (when novel? (increment-count location t))
      (setf (ppm-virtual-nodes m) vn))))
            
(defmethod get-virtual-node-count ((m ppm) location &optional (excluded nil))
  "Returns the count of the virtual-node associated with <location>." 
  (let* ((child (location-child location))
         (vnode (gethash child (ppm-virtual-nodes m))))
    (if (null vnode)
        (if excluded (get-count m child t) (get-count m child))
        (if excluded (virtual-node-count1 vnode)
            (virtual-node-count0 vnode)))))

(declaim (inline make-virtual-node))
(defun make-virtual-node (&key count0 count1)
  `((count0 ,count0) (count1 ,count1)))

(declaim (inline virtual-node-count0))
(defun virtual-node-count0 (virtual-node)
  (nth 1 (assoc 'count0 virtual-node)))

(declaim (inline virtual-node-count1))
(defun virtual-node-count1 (virtual-node)
  (nth 1 (assoc 'count1 virtual-node)))


;;;===========================================================================
;;; Probability estimation 
;;;===========================================================================

(defmethod get-distribution ((m ppm) location)
  "Selects a location on the chain of excited locations from <location>
   and estimates the distribution governing the emission of the current
   symbol from that location."
  (multiple-value-bind (selected-location selected?)
      (select-state m location)
    ;(format t "~S~%" (get-order m location))
    (probability-distribution m selected-location selected?)))
    
(defmethod select-state ((m ppm) location)
  "Returns the shortest deterministic state on the chain of suffix links
   from <location> or else <location> itself when (ppm-order-bound <m>)
   is null or the first state whose order is <= (ppm-order-bound <m>) if
   if it is an integer." 
  (labels ((order-bounded-state (location)
             (if (root-p location) location
                 (if (<= (get-order m location) (ppm-order-bound m))
                     location
                     (order-bounded-state (get-next-location m location)))))
           (shortest-deterministic-state (location selected)
             (if (root-p location) selected
                 (let ((slink (get-next-location m location)))
                   (if (branch-p location)
                       (shortest-deterministic-state slink selected)
                       (shortest-deterministic-state slink location))))))
    ;(format t "~&Order bound: ~A; location order: ~A~&" 
    ;        (ppm-order-bound m) (get-order m location))
    (if (null (ppm-order-bound m))
        (let ((sds (shortest-deterministic-state location '())))
          (if (null sds)
              (values location nil)
              (values sds t)))
        (if (<= (get-order m location) (ppm-order-bound m))
            (values location nil)
            (values (order-bounded-state location) t)))))

(defmethod probability-distribution ((m ppm) location selected?)
  "Returns the probability of <symbol> by computing a mixture of the 
   probabilities assigned to <symbol> by all the states in a chain of
   suffix links from <location>."
  (let* ((initial-distribution 
          (mapcar #'(lambda (a) (list a 0.0d0)) (ppm-alphabet m)))
         (up-ex (if (null selected?) (ppm-update-exclusion m)))
         (mixture 
          (compute-mixture m initial-distribution location '() :up-ex up-ex)))
    (normalise-distribution mixture)))

(defmethod compute-mixture ((m ppm) distribution location excluded
                            &key (up-ex (ppm-update-exclusion m))
                            (escape 1.0d0))
  "Returns the estimated probability of <symbol> appearing at <location>
   in the suffix tree of model <m>. <excluded> is a list of symbols that
   have been predicted at higher orders and are thus excluded from the
   smoothing computation."
  (if (earth-p location)
      (order-minus1-distribution m distribution excluded escape up-ex)
      (let* ((transition-counts (transition-counts m location up-ex))
             (child-count (child-count m transition-counts))
             (node-count (node-count m transition-counts excluded))
             (weight (the double-float (weight m node-count child-count)))
             (next-distribution
              (next-distribution m distribution transition-counts
                                 node-count excluded weight escape))
             (next-location (get-next-location m location))
             (next-excluded transition-counts)
             (next-escape (* (the double-float escape) (- 1.0d0 weight))))
        (compute-mixture m next-distribution next-location next-excluded
                         :escape next-escape))))

(defmethod next-distribution ((m ppm) distribution transition-counts node-count
                              excluded weight escape)
  "Updates <distribution> an alist of <symbol probability> pairs."
  (mapcar #'(lambda (d)
              (next-probability m d transition-counts node-count excluded
                                weight escape))
          distribution))

(defmethod next-probability ((m ppm) pair transition-counts node-count excluded
                             weight escape)
  "Updates the probability for <pair> a list containing a symbol and a
probability."
  (declare (type integer node-count) (type double-float weight escape))
  (let* ((symbol (nth 0 pair))
         (old-probability (nth 1 pair))
         (trans-count 
          (the integer (transition-count m symbol transition-counts)))
         (probability (if (zerop node-count) 0.0d0
                          (* weight (float (/ trans-count node-count) 0.0d0)))))
    (declare (type double-float probability old-probability))
    (if (null (ppm-mixtures m))
        (cond ((excluded? symbol excluded) pair)
              ((and (> trans-count 0) (> node-count 0))
               (list symbol (* escape probability)))
              (t pair))
        (list symbol (+ old-probability (* escape probability))))))
  
(defun excluded? (symbol excluded-list)
  "Returns true if <symbol> appears as a key in alist <excluded-list>."
  (assoc symbol excluded-list :test #'eequal))

(defmethod weight ((m ppm) node-count child-count)
  "Returns the weighting to give to a node s where count node-count (s)
   = <node-count> and child-count(s) = <child-count>."
  (declare (type integer node-count child-count) (values double-float))
  (let ((denominator (+ node-count (if (eql (ppm-escape m) :a) 1
                                       (/ child-count (ppm-d m))))))
    (if (zerop denominator) 0.0d0 
        (float (/ node-count denominator) 0.0d0))))

(defmethod transition-counts ((m ppm) location up-ex)
  "Returns a list of the form (symbol frequency-count) for the transitions
   appearing at location <location> in the suffix tree of model <m>."
  (if (branch-p location)
      (let ((tc '()))
        (dolist (child (list-children m location) (reverse tc))
          (let ((sym (get-symbol m (label-left (get-label m child)))))
            (when (member sym (ppm-alphabet m) :test #'eequal)
              (push (list sym (get-count m child up-ex)) tc)))))
      (let ((sym (get-symbol m (label-left (location-rest location)))))
        ;; we dynamically set derived alphabets on a per-event basis 
        (when (member sym (ppm-alphabet m) :test #'eequal)
          (list (list sym (get-virtual-node-count m location up-ex)))))))

(defmethod child-count ((m ppm) transition-counts)
  "Returns the token count given <child-count-list>, an alist of the
form (symbol frequency count), i.e., the total number of symbols that
have occurred with non-zero frequency. If the escape method is X only
those symbols that have occurred exactly once are counted."
  (declare (type list transition-counts))
  (case (ppm-escape m)
    (:x (+ (count-if #'(lambda (x) (= (nth 1 x) 1)) transition-counts) 1))
    (otherwise (length transition-counts))))

(defmethod transition-count ((m ppm) symbol transition-counts)
  "Returns the frequency count associated with <symbol> in <child-list>
   an alist of the form (symbol frequency-count)."
  (let ((count (assoc symbol transition-counts :test #'eequal)))
    (if (null count) 0 (+ (nth 1 count) (ppm-k m)))))

(defmethod node-count ((m ppm) transition-counts excluded-list)
  "Returns the total token count for symbols appearing in <child-list>
   , an alist of the form (symbol frequency-count), excluding the
   counts for those symbols that appear in <excluded-list>."
  (reduce #'+ (mapcar #'(lambda (c)
                          (if (excluded? (nth 0 c) excluded-list) 0
                              (+ (nth 1 c) (ppm-k m))))
                      transition-counts)))

(defmethod order-minus1-distribution ((m ppm) distribution excluded escape
                                      up-ex)
  "Returns the order -1 distribution." 
  (declare (type double-float escape))
  (mapcar #'(lambda (pair)
              (let ((symbol (nth 0 pair))
                    (p (nth 1 pair)))
                (declare (type double-float p))
                (if (and (null (ppm-mixtures m)) (excluded? symbol excluded)
                         (> p 0.0d0))
                    pair
                    (list symbol
                          (+ p 
                             (* escape 
                                (the double-float 
                                  (order-minus1-probability m up-ex))))))))
          distribution))

(defmethod order-minus1-probability ((m ppm) up-ex)
  "Returns the order -1 probability corresponding to a uniform distribution
   over the alphabet."
  (declare (values double-float))
  ;(print (list (alphabet-size m) (transition-counts m *root* up-ex)))
  (/ 1.0d0 ;(float (alphabet-size m) 0.0d0)))
     (float (- (+ 1.0d0 (alphabet-size m))
               (length (the list (transition-counts m *root* up-ex))))
            0.0d0)))
           

;;;===========================================================================
;;; I/O routines for ppm models  
;;;===========================================================================

;; (defun get-model (filename alphabet dataset &key (order-bound nil)
;;                            (mixtures t) (escape :c) (update-exclusion nil))
;;   "Returns a PPM model initialised with the supplied parameters. If
;;    <filename> exists the model is read from the designated file otherwise
;;    it is constructed from the database and written to <filename>." 
;;   (unless (file-exists filename)
;;     (let ((model (make-ppm alphabet)))
;;       (model-dataset model dataset :construct? t :predict? nil)
;;       (write-model-to-file model filename)
;;       (format t "~&Written PPM* model to ~A.~%" filename)))
;;   (read-model-from-file filename :order-bound order-bound :mixtures mixtures
;;                         :escape escape :update-exclusion update-exclusion))

;; (defun read-model-from-file (filename &key (order-bound nil)
;;                                       (mixtures t) (escape :c)
;;                                       (update-exclusion nil))
;;   "Returns the suffix tree stored in <filename>."
;;   (let* ((model (read-object-from-file filename :ppm))
;;          (leaves (alist->hash-table (nth 1 (assoc 'leaves model))))
;;          (branches (alist->hash-table (nth 1 (assoc 'branches model))))
;;          (dataset (alist->dataset (nth 1 (assoc 'dataset model))))
;;          (alphabet (nth 1 (assoc 'alphabet model))))
;;     (make-ppm alphabet :leaves leaves :branches branches :dataset dataset
;;               :order-bound order-bound :mixtures mixtures :escape escape
;;               :update-exclusion update-exclusion)))
              

;; (defmethod write-model-to-file ((m ppm) filename)
;;   "Writes the suffix tree of <m> to <filename>."
;;   (let* ((leaves (hash-table->alist (ppm-leaves m)))
;;          (branches (hash-table->alist (ppm-branches m)))
;;          (dataset (dataset->alist m))
;;          (alphabet (ppm-alphabet m))
;;          (model (list (list 'leaves leaves)
;;                       (list 'branches branches)
;;                       (list 'dataset dataset)
;;                       (list 'alphabet alphabet))))
;;     (write-object-to-file model filename :ppm)))

;; (defmethod dataset->alist ((m ppm))
;;   "Returns (ppm-dataset <m>) in the form of an alist."
;;   (mapcar #'(lambda (item)
;;               (list (car item)
;;                     (hash-table->alist (nth 1 item))))
;;           (hash-table->alist (ppm-dataset m))))

;; (defun alist->dataset (alist)
;;   "Returns a hash-table corresponding to <alist> which is suitable for
;;    storing the dataset of a PPM model."
;;   (alist->hash-table (mapcar #'(lambda (item)
;;                                  (list (car item)
;;                                        (alist->hash-table (nth 1 item))))
;;                              alist)))

;; (defmethod write-model-to-postscript ((m ppm) &key (filename "stree.ps"))
;;                                       ;(depth nil))
;;   "Prints the suffix tree of <m> to a postscript file <path>."
;;   (labels ((get-node-count (node)
;;              (list (string-append "Count0: "
;;                                   (format nil "~D" (get-count m node)))
;;                    (string-append "Count1: "
;;                                   (format nil "~D" (get-count m node t)))))
;;            (list-node-children (node)
;;              ;(when (or (null depth) 
;;              ;          (and (branch-record-p (get-record m node))
;;              ;               (<= (branch-record-depth (get-record m node)) 
;;              ;                   depth)))
;;                (list-children m node));)

;;            (list->string (list)
;;              (reduce #'(lambda (&optional s1 s2) (string-append s1 s2))
;;                      (mapcar #'(lambda (symbol)
;;                                  (format nil "~A " (if (symbolp symbol)
;;                                                        (symbol-name symbol)
;;                                                        symbol)))
;;                              list)))
;;            (label->string (node)
;;              (let ((label (instantiate-label m (get-label m node))))
;;                (cons (list->string label) (get-node-count node)))))
;;     (let ((psgraph:*fontsize* 14)
;;           (psgraph:*second-fontsize* 12)
;;           (psgraph:*boxradius* 10) 
;;           (psgraph:*boxedge* 10)
;;           (psgraph:*boxgray* "0")
;;           (psgraph:*edgegray* "0")
;;           (psgraph:*extra-x-spacing* 90)
;;           (psgraph:*extra-y-spacing* 20))
;;       (with-open-file (*standard-output* filename :direction :output
;;                                          :if-exists :supersede)
;;         (psgraph:psgraph *standard-output* *root*
;;                           #'list-node-children
;;                           #'label->string 
;;                           t nil #'eq nil)))))


