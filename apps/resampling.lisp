;;;; ======================================================================
;;;; File:       resampling.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-16 18:54:17 marcusp>                           
;;;; Time-stamp: <2022-06-14 09:14:45 marcusp>                           
;;;; ======================================================================
;;;;
;;;; DESCRIPTION 
;;;;
;;;;   For examining dataset prediction performance using entropy 
;;;;   computed by cross-validation of a dataset. 
;;;;
;;;; ======================================================================

(cl:in-package #:resampling)

(defparameter *model-dir* 
  (ensure-directories-exist
   (merge-pathnames "data/models/" (utils:ensure-directory apps:*root-dir*))))

(defparameter *resampling-dir*
  (ensure-directories-exist
   (merge-pathnames "data/resampling/" (utils:ensure-directory apps:*root-dir*))))

(defparameter *use-old-format-method* nil)

;;;===========================================================================
;;; Dataset Prediction 
;;;===========================================================================
    
(defun idyom-resample (dataset-id target-viewpoints source-viewpoints
                       &key pretraining-ids (k 10)
                         resampling-indices (models :both+)
                         (ltmo mvs::*ltm-params*)
                         (stmo mvs::*stm-params*)
                         (voices nil)
                         (texture :melody)
                         (use-resampling-set-cache? t)
                         (use-ltms-cache? t))
  "IDyOM top level: returns the mean information content for
   <dataset-id> given a model with the supplied parameters on
   <testing-sequence> where the long-term model has been trained on
   <training-sequence> and both <testing-sequence> and
   <training-sequence> are composed from <alphabet>. If <models> is
   'ltm only a long-term model is used, else if it is 'stm only a
   short-term model is used and otherwise both models are used and
   their predictions combined. The parameters
   <use-resampling-set-cache?> and <use-ltms-cache?> enable or disable
   respectively the caching of resampling-sets and LTMs."
  (let* (;; Check model memory parameters
         (ltmo (apply #'check-model-defaults (cons mvs::*ltm-params* ltmo)))
         (stmo (apply #'check-model-defaults (cons mvs::*stm-params* stmo)))
         ;; Set LTM and STM parameters
         (mvs::*models* models)
         (mvs::*ltm-order-bound* (getf ltmo :order-bound))
         (mvs::*ltm-mixtures* (getf ltmo :mixtures))
         (mvs::*ltm-update-exclusion* (getf ltmo :update-exclusion))
         (mvs::*ltm-escape* (getf ltmo :escape))
         (mvs::*ltm-exclusion* (getf ltmo :exclusion))
         (mvs::*stm-order-bound* (getf stmo :order-bound))
         (mvs::*stm-mixtures* (getf stmo :mixtures))
         (mvs::*stm-update-exclusion* (getf stmo :update-exclusion))
         (mvs::*stm-escape* (getf stmo :escape))
         (mvs::*stm-exclusion* (getf stmo :exclusion))
         ;; data
         (dataset (md:get-music-objects (if (listp dataset-id) dataset-id (list dataset-id))
                                        nil :voices voices :texture texture))
         (pretraining-set (md:get-music-objects pretraining-ids nil :voices voices :texture texture))
         ;; viewpoints
         (sources (get-viewpoints source-viewpoints))
         (targets
          (viewpoints:get-basic-viewpoints target-viewpoints (append dataset pretraining-set)))
         ;; resampling sets
         (k (if (eq k :full) (length dataset) k))
         (resampling-sets (get-resampling-sets dataset-id :k k
                                               :use-cache? use-resampling-set-cache?))
         (resampling-id 0)
         ;; If no resampling sets specified, then use all sets
         (resampling-indices (if (null resampling-indices)
                                 (utils:generate-integers 0 (1- k))
                                 resampling-indices))
         ;; the result
         (sequence-predictions))
    (dolist (resampling-set resampling-sets sequence-predictions)
      ;; (format t "~&~0,0@TResampling set ~A: ~A~%" resampling-id resampling-set)
      (when (member resampling-id resampling-indices)
        (let* ((training-set (get-training-set dataset resampling-set))
               (training-set (monodies-to-lists (append pretraining-set training-set)))
               (test-set (monodies-to-lists (get-test-set dataset resampling-set)))
               (ltms (get-long-term-models sources training-set
                                           pretraining-ids dataset-id
                                           resampling-id k 
                                           voices texture
                                           use-ltms-cache?))
               (mvs (make-mvs targets sources ltms))
               (predictions
                (mvs:model-dataset mvs test-set :construct? t :predict? t)))
          (push predictions sequence-predictions)))
      (incf resampling-id))))

(defun check-model-defaults (defaults &key
			      (order-bound (getf defaults :order-bound))
			      (mixtures (getf defaults :mixtures))
                              (update-exclusion (getf defaults :update-exclusion))
                              (exclusion (getf defaults :exclusion))
			      (escape (getf defaults :escape)))  
  (list :order-bound order-bound :mixtures mixtures :update-exclusion update-exclusion :escape escape :exclusion exclusion))

(defun monodies-to-lists (monodies) (mapcar #'monody-to-list monodies))
(defun monody-to-list (monody) (coerce monody 'list))

(defun output-information (resampling-predictions dataset-id &optional (detail 3) (information-measure :information.content))
  "Processes the output of IDYOM-RESAMPLE. <detail> is an integer
specifying the desired level of detail (1 = dataset average; 2 =
composition average; 3 = for all events in all
compositions. <information-measure> specifies whether to return
information content (:information.content), entropy (:entropy) or
both ('(:information-content :entropy))."
  (let ((data (resampling-predictions->dataframe resampling-predictions dataset-id))
        (event-ics) (composition-ics) (dataset-ics) (event-entropies) (composition-entropies) (dataset-entropies))
    (when (or (eq information-measure :information.content) (and (listp information-measure) (member :information.content information-measure)))
      (setf event-ics (get-column-by-composition :information.content data))
      (setf composition-ics (mapcar #'(lambda (x) (apply #'utils:average x)) event-ics))
      (setf dataset-ics (apply #'utils:average composition-ics)))
    (when (or (eq information-measure :entropy) (and (listp information-measure) (member :entropy information-measure)))
      (setf event-entropies (get-column-by-composition :entropy data))
      (setf composition-entropies (mapcar #'(lambda (x) (apply #'utils:average x)) event-entropies))
      (setf dataset-entropies (apply #'utils:average composition-entropies)))
    (if (and dataset-ics dataset-entropies)
        (case detail
          (1 (values dataset-ics dataset-entropies))
          (2 (values dataset-ics dataset-entropies composition-ics composition-entropies))
          (3 (values dataset-ics dataset-entropies composition-ics composition-entropies event-ics event-entropies)))
        (if dataset-ics
            (case detail
              (1 dataset-ics)
              (2 (values dataset-ics composition-ics))
              (3 (values dataset-ics composition-ics event-ics)))
            (case detail
              (1 dataset-entropies)
              (2 (values dataset-entropies composition-entropies))
              (3 (values dataset-entropies composition-entropies event-entropies)))))))

;;;===========================================================================
;;; Formatted output of information content etc.
;;;===========================================================================

(defun probability (event-prediction) 
  (cadr (prediction-sets:event-prediction event-prediction)))

(defun quote-string (string)
  (format nil "~s" string))

(defun format-information-content (resampling-predictions file dataset-id detail
				   &key (separator " "))
  (with-open-file (o file :direction :output :if-exists :supersede)
    (case detail 
      (1 (format t "~&Not implemented.~%"))
      (2 (format-information-content-detail=2 o resampling-predictions dataset-id
					      :separator separator))
      (3 (format-information-content-detail=3 o resampling-predictions dataset-id
					      :separator separator)))))

(defun format-information-content-detail=2 (stream resampling-predictions
					    dataset-id
					    &key (separator " "))
  (multiple-value-bind (dataset-mean composition-means)
      (resampling:output-information resampling-predictions dataset-id 2)
    (format stream "~&melody.id~Amelody.name~Amean.information.content~%"
	    separator separator)
    (do* ((ic composition-means (cdr ic))
          (cid 1 (1+ cid)))
         ((null ic) dataset-mean)
      (let ((d (quote-string (md:get-description dataset-id (1- cid)))))
        (format stream "~&~A~A~A~A~A~%" cid separator d separator (car ic))))))

(defun format-information-content-detail=3
    (stream resampling-predictions dataset-id &key (separator " ")
						(null-token "NA"))
  (if *use-old-format-method*
      (format-information-content-detail=3-old stream resampling-predictions
					       dataset-id :separator separator)
      (let ((df (resampling-predictions->dataframe resampling-predictions dataset-id)))
        (print-data df stream :null-token null-token :separator separator)
        df)))
  
(defun resampling-predictions->dataframe (resampling-predictions dataset-id)
  (let* ((resampling-predictions (reorder-resampling-predictions
                                  resampling-predictions))
         (data (make-instance 'dataframe)))
    
    (dolist (sp resampling-predictions) ; compositions
      (let ((composition-id (prediction-sets:prediction-index sp))
            (results (make-hash-table :test #'equal))
            (features))
        (dolist (fp (prediction-sets:prediction-set sp)) ; target viewpoints
          (let ((feature (viewpoints:viewpoint-type
                          (prediction-sets:prediction-viewpoint fp))))
            (pushnew feature features)
            (dolist (ep (prediction-sets:prediction-set fp)) ; events
              (let* ((event (prediction-sets:prediction-event ep))
                     (event-id (md:get-event-index
                                (md:get-attribute event 'identifier))))
                (setf (gethash (list composition-id event-id) results)
                      (format-event-prediction ep results
                                               dataset-id composition-id
                                               feature))))))
        (let ((past-results nil))
          (with-hash-table-iterator (next results)
            (loop
               (multiple-value-bind (more k v)
                   (next)
                 (unless more (return nil))
                 (setf (gethash k results)
                       (combine-event-probabilities v past-results features))
                 (setf past-results (gethash k results))
                 ;;(remhash 'distribution (gethash k results))
                 ))))
        (add-results-to-dataframe results data :blacklist '(:distribution))))
    data))

(defun reorder-resampling-predictions (resampling-predictions)
  "Reorders <resampling-predictions> so that instead of compositions 
being nested within features, features are nested within compositions.
Additionally merges resampling sets and orders by composition. The outcome
is a list of composition prediction sets, ordered by composition ID."
  (let* ((reordered
	  ;; Nest features within compositions
	  (loop for cv-set in resampling-predictions
	     collect
	       (let* ((composition-ids
		       (mapcar #'(lambda (x) (prediction-sets:prediction-index x))
			       (prediction-sets:prediction-set (car cv-set))))
		      (new-composition-sets
		       (mapcar #'(lambda (id)
				   (make-instance 'prediction-sets:sequence-prediction
						  :index id :set nil))
			       composition-ids)))
		 (loop for feature-set in cv-set
		    do (loop
			  for composition-set in (prediction-sets:prediction-set
						  feature-set)
			  for new-composition-set in new-composition-sets
			  do (push composition-set (prediction-sets:prediction-set
						    new-composition-set))))
		 (loop for new-composition-set in new-composition-sets
		    do (setf (prediction-sets:prediction-set new-composition-set)
			     (reverse (prediction-sets:prediction-set
				       new-composition-set))))
		 new-composition-sets)))
	 ;; Merge resampling sets
	 (reordered (apply #'append reordered))
	 ;; Order by composition
	 (reordered (sort reordered #'< :key #'prediction-sets:prediction-index)))
    reordered))

(defun create-key (feature attribute)
  (intern (concatenate 'string (symbol-name feature) "."
		       (format nil "~A" attribute)) :keyword))

(defun format-event-prediction (ep results dataset-id composition-id feature)
  (let* ((event (prediction-sets:prediction-event ep))
	 (event-id (md:get-event-index (md:get-attribute event 'identifier)))
	 (probability (float (probability ep) 0.0))
	 (distribution (prediction-sets:prediction-set ep))
	 (orders (prediction-sets:prediction-order ep))
	 (weights (prediction-sets:prediction-weights ep))
	 (existing-results (gethash (list composition-id event-id) results))
	 (event-results (if existing-results existing-results (make-hash-table)))
	 (timebase (md:timebase event)))
    ;; Store event information
    (unless existing-results
      (setf (gethash :dataset.id event-results) dataset-id)
      (setf (gethash :melody.id event-results) (1+ composition-id))
      (setf (gethash :note.id event-results) (1+ event-id))
      (setf (gethash :melody.name event-results) (quote-string (md:get-description
								dataset-id
								composition-id)))
      ;; TODO - this needs to be specific to each type of music-object
      ;; (music-event, music-slice etc.)
      (dolist (attribute (viewpoints:get-basic-types event))
	(let ((value (md:get-attribute event attribute)))
	  (when (and value (member attribute '(:dur :bioi :deltast :onset) :test #'eq))
	    (setf value (* value (/ timebase 96))))
	  (setf (gethash attribute event-results) value))))
    ;; Store feature prediction
    (dolist (o orders) ; orders
      (setf (gethash (create-key feature (car o)) event-results) (cadr o)))
    (when weights
      (dolist (w weights) ; weights
	(setf (gethash (create-key feature (car w)) event-results) (cadr w))))
    (setf (gethash (create-key feature :probability) event-results) probability)
    (setf (gethash (create-key feature :information.content) event-results)
	  (- (log probability 2)))
    (setf (gethash (create-key feature :entropy) event-results)
	  (float (prediction-sets:shannon-entropy distribution) 0.0))
    (setf (gethash (create-key feature :distribution) event-results) distribution)
    (dolist (p distribution)
      (let ((value (if (string= (symbol-name feature) "ONSET")
                       (- (car p) (- (md:get-attribute event :onset) (md:get-attribute event :bioi)))
                       (car p))))
        (setf (gethash (create-key feature value) event-results) (cadr p))))
    event-results))

(defun combine-event-probabilities (event-results previous-results features)
  (let* ((probability-keys (mapcar #'(lambda (f) (create-key f 'probability)) features))
	 (probabilities (mapcar #'(lambda (x) (gethash x event-results)) probability-keys))
	 (probability (apply #'* probabilities))
	 (distribution-keys (mapcar #'(lambda (f) (create-key f 'distribution)) features))
	 (distributions (mapcar #'(lambda (x) (gethash x event-results)) distribution-keys))
	 (distribution (mapcar #'(lambda (x) (let ((elements (mapcar #'first x))
						   (probabilities (mapcar #'second x)))
					       (list elements (apply #'* probabilities))))
			       (apply #'utils:cartesian-product distributions)))
         (information-gain (when previous-results
                             (kl-divergence distribution (gethash :distribution previous-results)))))
    (setf (gethash :distribution event-results) distribution)
    (setf (gethash :probability event-results) probability)
    (setf (gethash :information.content event-results) (- (log probability 2)))
    (setf (gethash :entropy event-results) (prediction-sets:shannon-entropy distribution))
    (setf (gethash :information.gain event-results) information-gain)
    ;; TODO elements of combined distribution
    (mapc #'(lambda (key) (remhash key event-results)) distribution-keys)
    event-results))

(defun kl-divergence (d1 d2)
  (reduce #'+ (mapcar #'(lambda (x y)
                          (* (cadr y) (- (log (cadr y) 2) (log (cadr x) 2))))
                      d1 d2)))
  

;;;===========================================================================
;;; Constructing the Long term models 
;;;===========================================================================

(defun get-long-term-models (viewpoints training-set pretraining-ids
                             training-id resampling-id
                             resampling-count 
                             voices texture
                             use-cache?)
  "Returns a vector of long-term models -- one for each viewpoint in
<viewpoints> -- trained on <training-set> and initialised with the
supplied keyword parameters. If <use-cache?> is T, models are written
to file, and reused on subsequent calls, otherise they are constructed
anew each time."
  (let ((constructor-fun
         (if use-cache?
             #'(lambda (viewpoint)
                 (let ((filename
                        (get-model-filename viewpoint pretraining-ids
                                            training-id resampling-id
                                            resampling-count 
                                            voices texture))
                       (training-set
                        (viewpoint-sequences viewpoint training-set))
                       (alphabet (viewpoint-alphabet viewpoint)))
                   (get-model filename alphabet training-set)))
             #'(lambda (viewpoint)
                 (let ((training-set
                        (viewpoint-sequences viewpoint training-set))
                       (alphabet (viewpoint-alphabet viewpoint)))
                   (ppm:build-model training-set alphabet))))))
    (mapcar constructor-fun viewpoints)))

(defun get-model-filename (viewpoint pretraining-ids training-id resampling-id
                           resampling-count voices texture)
  "Returns the filename in *model-directory* containing the ppm model
for <viewpoint> in <dataset-id>."
  (string-append (namestring *model-dir*)
                 (viewpoint-name viewpoint)
                 (if (null pretraining-ids) "_NIL"
                     (string-append
                      "_"
                      (subseq (apply #'string-append 
                                     (mapcar #'(lambda (x) (format nil "-~A" x))
                                             pretraining-ids)) 1)))
                 (cond ((null training-id) "_NIL-")
                       ((consp training-id)
                        (format nil "_~{~A~^-~}" training-id))
                       (t (format nil "_~A" training-id)))
                 (cond ((and (null resampling-id) (null resampling-count))
                        "")
                       ((null resampling-count) 
                        (format nil "-~A" (if (numberp resampling-id)
                                              (1+ resampling-id)
                                              resampling-id)))
                       (t 
                        (format nil "-~A:~A" (if (numberp resampling-id)
                                              (1+ resampling-id)
                                              resampling-id)
                                resampling-count)))
                 (format nil "_~(~A~)" texture)
                 (format nil "~{-~A~}" voices)
                 ".ppm"))


;;;===========================================================================
;;; Resampling sets
;;;===========================================================================

(defun get-training-set (dataset resampling-set)
  "Returns a list of compositions in dataset whose indices are
   members of the training indices of <resampling-set>."
  (let ((training-indices (nth 1 (assoc 'train resampling-set)))
        (training-set '())
        (composition-index 0))
    (dolist (composition dataset)
      (if (member composition-index training-indices)
          (push composition training-set))
      (incf composition-index))
    (reverse training-set)))

(defun get-test-set (dataset resampling-set)
  "Returns a list of compositions in dataset whose indices are
   members of the test indices of <resampling-set>."
  (let ((test-indices (nth 1 (assoc 'test resampling-set)))
        (test-set '())
        (composition-index 0))
    (dolist (composition dataset)
      (if (member composition-index test-indices)
          (push composition test-set))
      (incf composition-index))
    (reverse test-set)))

(defun get-resampling-sets (dataset-id &key (k 10) (use-cache? t))
  "Returns the resampling-sets for dataset <dataset-id>. If
   <use-cache?> is T and the cache file exists, they are read from
   file, otherwise they are created and optionally cached if
   <use-cache?> is T."
  (let* ((dataset-ids (if (consp dataset-id) dataset-id (list dataset-id)))
         (filename (get-resampling-sets-filename dataset-ids k)))
    (if (and use-cache? (file-exists filename))
        ;; Retrieve the previously cached resampling-set.
        (read-object-from-file filename :resampling)
        (let* ((composition-count
                (apply #'+ (mapcar #'md:count-compositions
                                   dataset-ids)))
               (resampling-sets (create-resampling-sets
                                 composition-count k)))
          (when use-cache? (write-resampling-sets-to-file
                            resampling-sets filename))
          resampling-sets))))

(defun write-resampling-sets-to-file (resampling-sets filename)
  "Writes <resampling-sets> to <file>." 
  (write-object-to-file resampling-sets filename :resampling)
  (format t "~%Written resampling set to ~A." filename))

(defun get-resampling-sets-filename (dataset-ids k)
  "Returns the filename in *resampling-sets-directory* containing the
   resampling-sets for <dataset-id>." 
  (string-append (namestring *resampling-dir*)
                 (format nil "~{~S-~}~S" (sort dataset-ids #'<) k)
                 ".resample"))
  

;;;===========================================================================
;;; Constructing random partitions of each dataset 
;;;===========================================================================

(defun create-resampling-sets (count k)
  "Returns a list of length <k> whose elements are lists representing a
   complete partition of the integers from 0 to (- count 1) where the
   elements of the individual sets are randomly selected without
   replacement."
  (let* ((test-sets (make-array k :initial-element nil))
	 (indices (loop for i from 0 to (1- count) collect i))
	 (shuffled-indices (utils:shuffle indices))
	 (current-test-set 0))
    (dolist (i shuffled-indices)
      (push i (svref test-sets current-test-set))
      (setf current-test-set (mod (1+ current-test-set) k)))
    (loop for i from 0 to (1- k)
       collect (let* ((test-set (sort (svref test-sets i) #'<))
		      (train-set (sort (remove-if #'(lambda (x) (member x test-set))
						  indices)
				       #'<)))
		 (list (list 'test test-set)
		       (list 'train train-set))))))

;;;===========================================================================
;;; Defining a dataframe class
;;;===========================================================================

(defclass dataframe ()
  ((data :initform (make-hash-table :test #'equal) :accessor data)
   (num-rows :initform 0 :accessor num-rows))
  (:documentation "A <dataframe> efficiently accumulates and stores text
data in a tabular form. Columns are identified by unique IDs, and are
stored as lists within a hash table.  Note that lists are accumulated
in reverse order, so that appending to a column can be achieved by
consing a new value to the beginning of the list."))

;; adding data

(defgeneric add-row (row place)
  (:documentation "Adds a new row, <row>, to a data storage object, <place>."))

(defmethod add-row ((row hash-table) (place dataframe))
  (let ((old-keys (loop for key being the hash-keys of (data place) collect key))
	(new-keys (loop for key being the hash-keys of row collect key)))
    (if (utils:any-duplicated new-keys)
	(error "Duplicated keys are not allowed when adding new rows."))
    (let ((old-unmatched-keys (set-difference old-keys new-keys))
	  (new-matched-keys (intersection old-keys new-keys))
	  (new-unmatched-keys (set-difference new-keys old-keys)))
      (dolist (key old-unmatched-keys)
	(push nil (gethash key (data place))))
      (dolist (key new-matched-keys)
	(push (gethash key row) (gethash key (data place))))
      (dolist (key new-unmatched-keys)
	(setf (gethash key (data place))
	      (cons (gethash key row)
		    (make-list (num-rows place) :initial-element nil))))
      (incf (num-rows place)))))

(defun add-results-to-dataframe (results dataframe &key blacklist)
  (flet ((sort-function (x y) (let ((x1 (car x)) (x2 (cadr x))
				    (y1 (car y)) (y2 (cadr y)))
				(if (= x1 y1) (< x2 y2) (< x1 y1)))))
    
    (let ((sorted-results (utils:hash-table->sorted-alist results #'sort-function)))
      (dolist (event sorted-results)
	(let* ((event-ht (cdr event)))
          (dolist (bl blacklist)
            (remhash bl event-ht))
	  (add-row event-ht dataframe))))))


;; accessing data

(defgeneric get-column-names (dataframe)
  (:documentation "Return a list of column names in <dataframe>"))

(defmethod get-column-names ((df dataframe))
  (loop for key being the hash-keys of (data df) collect key))

(defgeneric get-column (column dataframe)
  (:documentation "Return the data for <column> in <dataframe>."))

(defmethod get-column (c (df dataframe)) (gethash c (data df)))

(defgeneric get-columns (columns dataframe)
  (:documentation "Returns the data for <columns> in <dataframe>"))

(defmethod get-columns (columns (df dataframe))
  (mapcar #'(lambda (column) (get-column column df)) columns))

(defgeneric get-column-by-composition (column dataframe)
  (:documentation "Returns the data for <column> in <dataframe> as a list of lists, one for each composition."))

(defmethod get-column-by-composition (column (df dataframe))
  (let ((composition-ids (get-column :melody.id df))
        (data (get-column column df))
        (result)
        (cid-result))
    (do ((cid composition-ids (cdr cid))
         (cid-1 nil (car cid))
         (values data (cdr values)))
        ((or (null cid) (null values)) (push cid-result result))
      ;; (print (list (car cid) cid-1 (car values)))
      (if (and cid-1 (= (car cid) cid-1))
          (push (car values) cid-result)
          (progn
            (when cid-result (push cid-result result))
            (setf cid-result (list (car values))))))))

;; printing data 

(defgeneric print-data (data stream &key separator order-by-key null-token)
  (:documentation "Prints <data> to <stream>. If <order-by-key>, then the output
is ordered by key."))

(defmethod print-data ((data dataframe) destination
		       &key separator order-by-key null-token)
  (let* ((separator (if separator separator " "))
	 (columns (loop
		     for key being the hash-keys of (data data)
		     using (hash-value value)
		     collect (cons (string-downcase (symbol-name key))
				   (reverse value))))
	 (columns (if order-by-key
		      (sort columns #'string< :key #'car)
		      columns))
	 (columns (coerce columns 'vector))
	 (num-rows (num-rows data))
	 (num-cols (array-dimension columns 0)))
    (assert (> num-rows 0))
    (assert (> num-cols 0))
    (assert (eql (num-rows data) (1- (length (svref columns 0)))))
    (dotimes (i (1+ (num-rows data)))
      (dotimes (j num-cols)
	(let* ((token (pop (svref columns j)))
	       (token (if (and (null token) null-token) null-token token))) 
	  (format destination "~A~A" token separator)))
      (format destination "~&"))))
  

