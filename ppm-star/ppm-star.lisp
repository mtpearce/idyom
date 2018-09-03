;;;; ======================================================================
;;;; File:       ppm-star.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2002-07-02 18:54:17 marcusp>                           
;;;; Time-stamp: <2018-09-03 14:28:38 marcusp>                           
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
;;;; TODO 
;;;;
;;;;   - Bunton improvements: state-selection;
;;;;   - Other smoothing techniques (Kneser-Ney, Katz); 
;;;;   - Other modelling techniques (PST, LZ, BW, DMC). 
;;;;
;;;; ======================================================================

(cl:in-package #:ppm)

(defvar *sentinel* '$ "The unique end of sequence symbol.")
(defun sentinel-p (s) (eql s *sentinel*))

(defun eequal (obj1 obj2)
  "Predicate for comparing elements of sequences."
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 0) (compilation-speed 0)))
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
      (let* ((sum (sum-distribution distribution))
             (scaling-factor (if (zerop sum) 1.0 (/ 1.0 sum))))
        (mapcar #'(lambda (ep) 
                    (let ((e (nth 0 ep)) (p (nth 1 ep)))
                      (list e (* p scaling-factor))))
                distribution))))

(defun sum-distribution (distribution)
  "Returns the sum of <distribution> a set of probabilities distributed
   over an alphabet."
  (apply #'+ (distribution-probabilities distribution)))

(defun sums-to-one-p (distribution)
  "Returns true if <distribution> sums to one otherwise nil."
  (< 0.999 (sum-distribution distribution) 1.0))

(defun get-probability (element distribution)
  (cadr (assoc element distribution :test #'eequal)))

(defun codelength (probability)
  (- (log probability 2)))
        
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
   (normalise :accessor ppm-normalise :initarg :normalise :type (or null symbol))
   (exclusion :accessor ppm-exclusion :initarg :exclusion :type (or null symbol))
   (update-exclusion :accessor ppm-update-exclusion :initarg :update-exclusion
                     :type (or null symbol))
   (mixtures :accessor ppm-mixtures :initarg :mixtures :type (or null symbol))
   (order-bound :accessor ppm-order-bound :initarg :order-bound
                :type (or null symbol))
   (escape :accessor ppm-escape :initarg :escape :type (or null symbol))
   (k :accessor ppm-k :initarg :k :type float)
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
  "An index into the data where an event is uniquely identified by its
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
(defun increment-node-record-count (node-record update-excluded)
  "Increments the count of <node-record> by one. If <update-excluded> is non-nil
   the update-excluded count is incremented."
  (case (type-of node-record)
    (leaf-record (if update-excluded
                     (incf (leaf-record-count1 node-record))
                     (incf (leaf-record-count0 node-record))))
    (branch-record (if update-excluded
                       (incf (branch-record-count1 node-record))
                       (incf (branch-record-count0 node-record))))))

(declaim (inline root-p))
(defun root-p (branch)
  "A branch <branch> is the root if its index is 1."
  (when (branch-p branch) (= (branch-index branch) 1)))

(declaim (inline earth-p))
(defun earth-p (branch)
  "A branch <branch> is the earth if its index is 0."
  (when (branch-p branch) (= (branch-index branch) 0)))

(defun node-equal (node1 node2)
  "Equality predicate for branches and leaves."
  (or (and (branch-p node1) (branch-p node2)
           (= (branch-index node1) (branch-index node2)))
      (and (leaf-p node1) (leaf-p node2)
           (= (leaf-index node1) (leaf-index node2)))))

(declaim (inline get-root))
(defun get-root () (make-branch :index 1))

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

(defmethod get-count ((m ppm) branch-or-leaf update-excluded)
  "Returns the count associated with <branch-or-leaf>."
  (if update-excluded
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

(defmethod instantiate-label ((m ppm) label &key full)
  "Instantiates a <label> with values from the sequence of model
<m>. If <order> is nil, the label of the corresponding location is
returned but if <order> is a number specifying the order (i.e., depth)
of the location, the label is instantiated from the root of the tree."
  (let* ((label-left (label-left label))
         (sequence-index (index-s label-left))
         (event-index (index-e label-left))
         (length (get-length m label))
         (e-start (if full (- event-index (- full length)) event-index))
         (length (if full full length))
         (label-list '()))
    (dotimes (e length)
      (let ((index (make-index :s sequence-index :e (+ e-start e)))) 
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

(defmethod location->string ((m ppm) location)
  (utils:list->string (location->list m location)))

(defmethod location->list ((m ppm) location)
  (if (branch-p location)
      (instantiate-label m (get-label m location) :full (get-order m location))
      (append (instantiate-label m (get-label m (location-node location)) :full (get-order m (location-node location)))
              (instantiate-label m (location-match location)))))

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
                   ((and (null brother) (null match)) child)
                   ((node-equal brother match) child)
                   (t (get-next-brother brother (get-brother m child))))))
    (let ((brother (if (null first-child) nil (get-brother m first-child))))
      (get-next-brother first-child brother))))

(defmethod list-children ((m ppm) node)
  "Returns a list of the children of <node>."
  (labels ((add-next-brother (child result)
             (if (null child) 
                 (let ((result (reverse result)))
                   ;;(print (mapcar #'(lambda (x) (list (get-symbol m (label-left (get-label m x))))) result))
                   result)
                 (add-next-brother (get-brother m child) (cons child result)))))
    (let ((child (when (branch-p node) (branch-record-child (get-record m node)))))
      (add-next-brother child '()))))

(defmethod get-symbol ((m ppm) index)
  "Returns the symbol specified by the database index <index>"
  (gethash (index-e index) (gethash (index-s index) (ppm-dataset m))))

(defmethod add-event-to-model-dataset ((m ppm) symbol)
  "Adds <symbol> to the current sequence in ppm-dataset."
  (let ((seq-hash (gethash (index-s (ppm-front m)) (ppm-dataset m))))
    (unless (hash-table-p seq-hash) 
      (setf (gethash (index-s (ppm-front m)) (ppm-dataset m))
            (make-hash-table)))
    (setf (gethash (index-e (ppm-front m))
                   (gethash (index-s (ppm-front m)) (ppm-dataset m)))
          symbol)))

(defmethod print-dataset ((m ppm))
  (dotimes (i (dataset-length m))
    (dotimes (j (dataset-sequence-length m i))
      (format t "~A " (get-symbol m (make-index :s i :e j))))
    (format t "~%")))
      

;;;===========================================================================
;;; Initialisation 
;;;===========================================================================

(defun make-ppm (alphabet &key (exclusion t) (mixtures t) (escape :c) 
                            (order-bound nil) (update-exclusion nil)
                            (normalise t)
                            (dataset nil) (leaves nil) (branches nil))
  "Returns a PPM* model initialised with the supplied parameters."
  (multiple-value-bind (k d)
      (case escape
        (:a (values 0 1))
        (:b (values -1 1))
        ((or :c :x) (values 0 1))
        (:d (values -1/2 2))
        (otherwise (values 0 1)))
    (let* ((dataset (if (null dataset) (make-hash-table) dataset))
           (front (make-index :s (hash-table-count dataset) :e 0))
           (initial-leaves (if (null leaves) (make-hash-table) leaves))
           (initial-branches (if (null branches) (make-hash-table) branches))
           (leaf-index (hash-table-count initial-leaves))
           (branch-index (hash-table-count initial-branches))
           (virtual-nodes (make-hash-table :test #'equal))
           (model (make-instance 'ppm 
                                 :leaves initial-leaves
                                 :branches initial-branches
                                 :front front 
                                 :dataset dataset 
                                 :leaf-index leaf-index
                                 :branch-index branch-index
                                 :virtual-nodes virtual-nodes
                                 :alphabet alphabet
                                 :normalise normalise
                                 :exclusion exclusion
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
  (let* ((earth (make-branch :index 0))
         (root (make-branch :index 1))
         (root-label (make-label :left (make-index) :length 0))
         (root-record (make-branch-record :label root-label :slink earth
                                          :count0 0 :count1 0))
         (earth-record (make-branch-record :child root :depth -1
                                           :count0 0 :count1 0)))
    (setf (gethash 0 (ppm-branches m)) earth-record
          (gethash 1 (ppm-branches m)) root-record
          (ppm-branch-index m) 2
          (ppm-leaf-index m) 0)))

(defmethod initialise-virtual-nodes ((m ppm))
  "Initialises the <virtual-nodes> slot of ppm model <m>."
  (setf (ppm-virtual-nodes m) (make-hash-table :test #'equal)))

(defmethod set-ppm-parameters ((m ppm) &key (normalise t) (exclusion t) (mixtures t) (escape :c)
                                 (order-bound nil) (update-exclusion nil))
  (multiple-value-bind (k d)
      (case escape
        (:a (values 0 1))
        (:b (values -1 1))
        ((or :c :x) (values 0 1))
        (:d (values -1/2 2))
        (otherwise (values 0 1)))
    (setf (ppm-normalise m) normalise
          (ppm-exclusion m) exclusion
          (ppm-mixtures m) mixtures
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
                       (ppm-model-event m symbol :location location 
                                        :construct? construct? :predict? predict?)
                     (increment-event-front m)
                     (model-seq (cdr sequence) next-location
                                (cons (list symbol event-distribution)
                                      prediction-set)))))))
    (prog1 (model-seq sequence (get-root) '())
      (when construct? (initialise-virtual-nodes m)))))

(defmethod model-sentinel-event ((m ppm) location)
  (add-event-to-model-dataset m *sentinel*)
  (ukkstep m nil location *sentinel* t)
  (increment-event-front m))

(defmethod ppm-model-event ((m ppm) symbol &key location construct? predict?)
  "Models an event <symbol> appear at location <location> in the ppm
   model <m>. If <construct?> is non-nil the event is added to the model
   and if <predict?> is non-nil a distribution over (ppm-alphabet m)
   is returned for <location>.  The model's index into the sequence
   vector must be set to the appropriate event index before this method
   is called."
  ;; (format t "~%~%SYMBOL = ~A~&" symbol)
  (add-event-to-model-dataset m symbol)
  (let* ((gd (when predict? (multiple-value-list (get-distribution m location))))
         (distribution (car gd))
         (order (cadr gd))
         (novel? (when construct? (unless (occurs? m location symbol) t)))
         (next-location (ukkstep m nil location symbol construct?)))
    (when construct? (increment-counts m next-location novel?))
    (values next-location distribution order)))


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
  ;; (format t "~%~%SPLIT-LOCATION ~A" (location->string m location))
  (let* (;; the parent record corresponding to the location
         (parent-record (get-record m (location-node location)))
         (parent-depth (branch-record-depth parent-record))
         (parent-child (branch-record-child parent-record))
         (match-length (label-length (location-match location)))
         (rest-length (label-length (location-rest location)))
         ;; the child of the parent
         (node (location-child location))
         (node-record (get-record m node))
         (node-index (get-node-index m node))
         ;; a new node for the matched portion of the parent's label
         (new-node (make-branch :index (ppm-branch-index m)))
         (node-label-left (label-left (get-label m node)))
         (new-node-count0 (get-count m node nil))
         (new-node-count1 (get-count m node t))
         (new-node-depth (+ parent-depth match-length))
         (new-node-label (make-label :left (make-index
                                            :s (index-s node-label-left)
                                            :e (index-e node-label-left))
                                     :length match-length))
         (new-node-child (if (leaf-p node) (make-leaf :index node-index)
                             (make-branch :index node-index)))
         (new-node-brother (if (leaf-p node) (leaf-record-brother node-record)
                               (branch-record-brother node-record)))
         (new-node-record
          (make-branch-record :label new-node-label 
                              :child new-node-child
                              :brother new-node-brother
                              :depth new-node-depth 
                              :count0 new-node-count0
                              :count1 new-node-count1))
         ;; a new node for the unmatched portion of the parent's label
         (new-child-count0 (get-virtual-node-count m location nil))
         (new-child-count1 (get-virtual-node-count m location t))
         (new-child-label
          (make-label :left (make-index
                             :s (index-s node-label-left)
                             :e (+ (index-e node-label-left) match-length))
                      :length rest-length))
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
                                  :count1 new-child-count1))))
    ;; (format t "~&Parent: ~A depth = ~A match = ~A rest = ~A" (instantiate-label m (get-label m (location-node location) :full t))
    ;;         parent-depth match-length rest-length)
    ;; (format t "~&Child: ~A ~A" (if (leaf-p node) "leaf" "branch") (instantiate-label m (get-label m node)))
    ;; (format t "~&Match: ~A" (instantiate-label m (location-match location)))
    ;; (format t "~&New node: branch ~A depth = ~A count0 = ~A count1 = ~A" 
    ;;         (instantiate-label m new-node-label) new-node-depth new-node-count0 new-node-count1)
    ;; (format t "~&New child: ~A ~A depth = ~A count0 = ~A count1 = ~A~%" (if (leaf-p node) "leaf" "branch") 
    ;;         (instantiate-label m new-child-label) (if (branch-p node) (+ rest-length new-node-depth)) new-child-count0 new-child-count1)
    (if (leaf-p node)
        (set-leaf-record m node-index new-child-record)
        (set-branch-record m node-index new-child-record))
    (set-branch-record m (ppm-branch-index m) new-node-record)
    (increment-branch-index m)
    (if (or (null parent-child) (node-equal parent-child node))
        (set-branch-record-child parent-record new-node)
        (let ((matching-brother (get-matching-brother m parent-child node)))
          (unless (null matching-brother)
            (set-record-brother (get-record m matching-brother) new-node))))
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
  (labels ((increment-count (location update-excluded)
             (if (branch-p location)
                 (let* ((key (location->list m location))
                        (key (unless (root-p location) (subseq key 0 (1- (length key)))))
                        (vnode (when key (gethash key (ppm-virtual-nodes m)))))
                   (when (or (root-p location) (not vnode))
                     ;; (if update-excluded (print "branch"))
                     (increment-node-record-count (get-record m location) update-excluded)))
               (let* ((child (location-child location))
                      (child-record (get-record m child))
                      (match (location-match location)))
                 (when (= (label-length match) 1)
                   ;; (if update-excluded (print "leaf"))
                   (increment-node-record-count child-record update-excluded)))))
           (increment-suffix-counts (location vn)
             (if (root-p location)
                 (progn (increment-count location nil) vn)
                 (let ((vn (allocate-virtual-node m location vn)))
                   (increment-count location nil)
                   (increment-suffix-counts (get-next-location m location) vn)))))
    (let ((vn (increment-suffix-counts location (make-hash-table :test #'equal))))
      ;; (format t "~&Increment UX count: ~A" (location->string m location))
      ;; (print (list novel? (index-e (ppm-front m)) (get-order m location)))
      ;; (print-dataset m)
      (increment-count location t)
      (setf (ppm-virtual-nodes m) vn))))

(defstruct virtual-node
  (parent nil :type (or null branch))
  (child  nil :type (or null branch leaf))
  (offset 1   :type (integer 1 *))
  (count0 1   :type (integer 1 *))
  (count1 1   :type (integer 1 *)))

(defmethod get-virtual-node-count ((m ppm) location update-excluded)
  "Returns the count of the virtual-node associated with
<location>. If <update-excluded> is not null, the update-excluded
count is returned otherwise the full count is returned."
  (let ((vnode (retrieve-virtual-node m location)))
    (if (null vnode)
        (let ((state (if (branch-p location) location (location-child location))))
          (get-count m state update-excluded))
        (if update-excluded
            (virtual-node-count1 vnode)
            (virtual-node-count0 vnode)))))

(defmethod retrieve-virtual-node ((m ppm) location)
  "Retrieves the virtual state corresponding to <location> from the
virtual nodes stored in PPM model <m>."
  (when (location-p location)
    (gethash (location->list m location) (ppm-virtual-nodes m))))

(defmethod allocate-virtual-node ((m ppm) location vn)
  "Allocates a virtual node into the hash-table <vn> corresponding to
the location <location> and returns <vn>."
  (when (location-p location)
    (let* ((s (location->list m location))
           (state (location-child location))
           (s-1 (when s (subseq s 0 (1- (length s)))))
           (vnode (gethash s-1 (ppm-virtual-nodes m)))
           (vnode (if (and vnode (eq (virtual-node-child vnode) state)) vnode))
           (new-vnode (make-virtual-node
                       :parent (location-node location)
                       :child  state
                       :offset (label-length (location-match location))
                       :count0 (if vnode (virtual-node-count0 vnode) (get-count m state nil))
                       :count1 (if vnode (virtual-node-count1 vnode) (get-count m state t)))))
      (setf (gethash s vn) new-vnode)
      (remhash s-1 vn)))
  vn)
          

;;;===========================================================================
;;; State selection
;;;===========================================================================

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
              (if (branch-p location) selected
                  (shortest-deterministic-state (get-next-location m location) location))))
    (if (null (ppm-order-bound m))
        (let ((sds (shortest-deterministic-state location nil)))
          (if (or (null sds) (eq sds location))
              (values location nil)
              (values sds t)))
        (if (<= (get-order m location) (ppm-order-bound m))
            (values location nil)
            (values (order-bounded-state location) t)))))


;;;===========================================================================
;;; Probability estimation 
;;;===========================================================================

(defmethod get-distribution ((m ppm) location)
  "Selects a location on the chain of excited states from <location>
   and estimates the distribution governing the emission of the
   current symbol from that location by computing a mixture of the
   probabilities assigned to each symbol at all states in the chain of
   suffix links from the selected location."
  ;; (format t "~&GET-DISTRIBUTION ~A~%" (location->string m location))
  ;; 1. Select state
  (multiple-value-bind (selected-location selected?)
      (select-state m location)
    ;; (format t "~&~A ~A ~A, order ~S~%"
    ;;         (if selected? "Selected" "Unselected")
    ;;         (if (branch-p location) "branch" "location")
    ;;         (location->string m selected-location)
    ;;         (get-order m selected-location))
    ;; 2. Estimate distribution
    (let* ((initial-distribution (mapcar #'(lambda (a) (list a 0.0)) (ppm-alphabet m)))
           (update-exclusion (if (null selected?) (ppm-update-exclusion m)))
           (mixture (compute-mixture m initial-distribution selected-location nil
                                     :update-exclusion update-exclusion)))
      (when (ppm-normalise m)
        (setf mixture (normalise-distribution mixture)))
      (values mixture (get-order m selected-location)))))

(defmethod compute-mixture ((m ppm) distribution location excluded
                            &key (update-exclusion (ppm-update-exclusion m))
                              (escape 1.0))
  "Returns the estimated probability of <symbol> appearing at
   <location> in the suffix tree of model <m>. <excluded> is a
   hash-table of symbols that have been predicted at higher orders and
   are thus excluded from the smoothing computation."
  (if (earth-p location)
      (let ((om1d (order-minus1-distribution m distribution excluded escape
                                             update-exclusion)))
        ;; (format t "~%COMPUTE-MIXTURE ~A ~A" nil update-exclusion)
        ;; (format t "~&distribution: ~A~%excluded ~A~%escape ~A~%~%" om1d
        ;;         (when excluded (utils:hash-table->alist excluded)) escape)
        om1d)
      (let* ((transition-counts (transition-counts m location update-exclusion))
             (type-count (type-count m transition-counts)) 
             (state-count (state-count m transition-counts excluded))
             (weight (weight m state-count type-count))
             (next-distribution
              (next-distribution m distribution transition-counts
                                 state-count excluded weight escape))
             (next-location (get-next-location m location))
             (next-excluded transition-counts)
             (next-escape (* escape (- 1.0 weight))))
        ;; (format t "~%COMPUTE-MIXTURE ~A ~A"  (location->string m location) update-exclusion)
        ;; (format t "~&type-count ~A~%state-count ~A~%weight ~A~%distribution: ~A~%excluded ~A~%escape ~A~%"
        ;;         type-count state-count weight next-distribution
        ;;       (when excluded (utils:hash-table->alist excluded)) escape)
        (compute-mixture m next-distribution next-location next-excluded
                         :escape next-escape))))

(defmethod next-distribution ((m ppm) distribution transition-counts state-count
                              excluded weight escape)
  "Updates <distribution>, an alist of <symbol probability> pairs."
  (mapcar #'(lambda (d)
              (next-probability m d transition-counts state-count excluded
                                weight escape))
          distribution))

(defmethod next-probability ((m ppm) pair transition-counts state-count excluded
                             weight escape)
  "Updates the probability for <pair> a list containing a symbol and a
probability."
  (let* ((symbol (nth 0 pair))
         (old-probability (nth 1 pair))
         (trans-count 
          (transition-count m symbol transition-counts))
         (probability (if (zerop state-count) 0.0
                          (* weight (float (/ trans-count state-count) 0.0)))))
    (if (null (ppm-mixtures m))
        (cond ((excluded? symbol excluded) pair)
              ((and (> trans-count 0) (> state-count 0))
               (list symbol (* escape probability)))
              (t pair))
        (list symbol (+ old-probability (* escape probability))))))

(defun excluded? (symbol excluded)
  "When exclusions are enabled for model <m>, returns true if <symbol>
appears as a key in hash-table <excluded>."
  (when excluded (gethash symbol excluded)))

(defmethod weight ((m ppm) state-count type-count)
  "Returns the weighting to give to a state s given the state-count and 
   type count."
  (let ((denominator (+ state-count (if (eq (ppm-escape m) :a) 1
                                       (/ type-count (ppm-d m))))))
    (if (zerop denominator) 0.0 
        (float (/ state-count denominator) 0.0))))

(defmethod transition-counts ((m ppm) location update-exclusion)
  "Returns a hash-table of the form (symbol frequency-count) for the
   transitions appearing at location <location> in the suffix tree of
   model <m>. If <update-exclusion> is T, the update excluded counts
   are used otherwise full counts are used."
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 0) (compilation-speed 0)))
  (let ((tc (make-hash-table :test #'equal)))
    (if (branch-p location)
        (let ((alphabet (ppm-alphabet m)))
          (dolist (child (list-children m location) tc)
            (let ((sym (get-symbol m (label-left (get-label m child)))))
              (when (member sym alphabet :test #'eequal)
                (setf (gethash sym tc) (get-count m child update-exclusion))))))
        (let ((sym (get-symbol m (label-left (location-rest location)))))
          (when (member sym (ppm-alphabet m) :test #'eequal)
            (setf (gethash sym tc) (get-virtual-node-count m location update-exclusion)))))
    tc))

(defmethod transition-count ((m ppm) symbol transition-counts)
  "Returns the frequency count associated with <symbol> in
   <transition-counts> a hash-table of the form (symbol
   frequency-count)."
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 0) (compilation-speed 0)))
  (let ((count (gethash symbol transition-counts))
        (k (ppm-k m)))
    (if (null count) 0 (+ count k))))

(defmethod type-count ((m ppm) transition-counts)
  "Returns the type count given <transition-counts>, a hash-table of the
form (symbol frequency count), i.e., the total number of symbols that
have occurred with non-zero frequency. If the escape method is X only
those symbols that have occurred exactly once are counted."
  (case (ppm-escape m)
    (:x (let ((count 1))
          (maphash #'(lambda (k v)
                       (declare (ignore k))
                       (when (= v 1) (incf count 1)))
                   transition-counts)
          count))
    (otherwise (hash-table-count transition-counts))))

(defmethod state-count ((m ppm) transition-counts excluded)
  "Returns the total token count for symbols appearing in
   <transition-counts>, a hash-table of the form (symbol
   frequency-count), excluding the counts for those symbols that
   appear in <excluded>."
  (let ((ppmk (ppm-k m))
        (count 0))
    (maphash #'(lambda (k v)
                 (unless (and (ppm-exclusion m) (excluded? k excluded))
                   (incf count (+ v ppmk))))
             transition-counts)
    count))

(defmethod order-minus1-distribution ((m ppm) distribution excluded escape
                                      update-exclusion)
  "Returns the order -1 distribution."
  (let ((order-minus1-p (order-minus1-probability m update-exclusion)))
    (mapcar #'(lambda (pair)
                (let ((symbol (nth 0 pair))
                      (p (nth 1 pair)))
                  (if (and (null (ppm-mixtures m)) (excluded? symbol excluded)
                           (> p 0.0))
                      pair
                      (list symbol
                            (+ p 
                               (* escape 
                                  order-minus1-p))))))
            distribution)))

(defmethod order-minus1-probability ((m ppm) update-exclusion)
  "Returns the order -1 probability corresponding to a uniform distribution
   over the alphabet."
  ;; (format t "~&ORDER-MINUS1-PROBABILITY~&order -1 p: 1 / ~A + 1 - ~A~%" (alphabet-size m) 
  ;;       (hash-table-count (transition-counts m (get-root) update-exclusion)))
  (/ 1.0 
     (float (- (+ 1.0 (alphabet-size m))
               (if (ppm-exclusion m)
                   (hash-table-count (transition-counts m (get-root) update-exclusion))
                   1.0))
            0.0)))
