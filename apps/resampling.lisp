;;;; ======================================================================
;;;; File:       resampling.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-16 18:54:17 marcusp>                           
;;;; Time-stamp: <2017-05-16 23:29:41 peter>                           
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
			 (polyphonic-expansion :full)
			 (harmonic-reduction :regular-harmonic-rhythm)
			 (slices-or-chords :chords)
			 (num-quantiles 10)
                         (use-resampling-set-cache? t)
			 (resampling-set-cache-path nil)
                         (use-ltms-cache? t)
			 (detail nil))
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
  (assert (integerp num-quantiles))
  (let* (;; Check model memory parameters
         (ltmo (apply #'check-model-defaults (cons mvs::*ltm-params* ltmo)))
         (stmo (apply #'check-model-defaults (cons mvs::*stm-params* stmo)))
         ;; Set LTM and STM parameters
         (mvs::*models* models)
         (mvs::*ltm-order-bound* (getf ltmo :order-bound))
         (mvs::*ltm-mixtures* (getf ltmo :mixtures))
         (mvs::*ltm-update-exclusion* (getf ltmo :update-exclusion))
         (mvs::*ltm-escape* (getf ltmo :escape))
         (mvs::*stm-order-bound* (getf stmo :order-bound))
         (mvs::*stm-mixtures* (getf stmo :mixtures))
         (mvs::*stm-update-exclusion* (getf stmo :update-exclusion))
         (mvs::*stm-escape* (getf stmo :escape))
         ;; data
         (dataset (md:get-music-objects (if (listp dataset-id)
					    dataset-id (list dataset-id))
                                        nil :voices voices
					:texture texture
					:polyphonic-expansion polyphonic-expansion
					:harmonic-reduction harmonic-reduction
					:slices-or-chords slices-or-chords))
         (pretraining-set (md:get-music-objects
			   pretraining-ids nil :voices voices :texture texture
			   :polyphonic-expansion polyphonic-expansion
			   :harmonic-reduction harmonic-reduction
			   :slices-or-chords slices-or-chords))
	 (dataset
	  (progn
	    (utils:message "Adding local key cache to the main dataset.")
	    (mapcar #'viewpoints:add-local-key-cache dataset)))
	 (pretraining-set
	  (progn
	    (utils:message "Adding local key cache to the pretraining dataset.")
	    (mapcar #'viewpoints:add-local-key-cache pretraining-set)))
	 (viewpoints:*use-cached-local-key* t)
         ;; viewpoints
         (sources (get-viewpoints source-viewpoints))
         (targets
          (viewpoints:get-basic-viewpoints target-viewpoints
					   (append dataset pretraining-set)))
         ;; resampling sets
         (k (if (eq k :full) (length dataset) k))
         (resampling-sets (get-resampling-sets
			   dataset-id :k k
			   :use-cache? use-resampling-set-cache?
			   :resampling-set-cache-path resampling-set-cache-path))
         (resampling-id 0)
         ;; If no resampling sets specified, then use all sets
         (resampling-indices (if (null resampling-indices)
                                 (utils:generate-integers 0 (1- k))
                                 resampling-indices))
         ;; the result
         (sequence-predictions))
    (discretise-viewpoints (append sources targets)
			   (cons dataset-id pretraining-ids)
			   num-quantiles
			   slices-or-chords harmonic-reduction
			   polyphonic-expansion)
    (let ((viewpoints:*discretise-viewpoints* t)) 
      (utils:message (format nil "Iterating over ~A resampling indice(s)."
			     (length resampling-indices)))
      (dolist (resampling-set resampling-sets sequence-predictions)
	;; (format t "~&~0,0@TResampling set ~A: ~A~%" resampling-id resampling-set)
					;(format t "~&Resampling ~A" resampling-id)
	(when (member resampling-id resampling-indices)
	  (utils:message (format nil "Modelling resampling fold ~A/~A."
				 (1+ resampling-id) (length resampling-indices)))
	  (let* ((training-set (get-training-set dataset resampling-set))
		 (training-set (monodies-to-lists (append pretraining-set
							  training-set)))
		 (test-set (monodies-to-lists (get-test-set dataset
							    resampling-set)))
		 (ltms (get-long-term-models sources training-set
					     pretraining-ids dataset-id
					     resampling-id k 
					     voices texture
					     use-ltms-cache?))
		 (mvs (make-mvs targets sources ltms))
		 (predictions (mvs:model-dataset mvs test-set
						 :construct? t :predict? t
						 :detail detail)))
	    (if (typep predictions 'utils:dataframe)
		(if sequence-predictions
		    (setf sequence-predictions (utils:bind-by-row sequence-predictions
								  predictions))
		    (setf sequence-predictions predictions))
		(push predictions sequence-predictions))))
	(incf resampling-id))
      (when (typep sequence-predictions 'utils:dataframe)
	(utils:sort-by-columns sequence-predictions '(:melody.id)))
      (when (= detail 1) (error "Detail level 1 not yet implemented."))
      sequence-predictions)))

(defun discretise-viewpoints (viewpoints datasets num-quantiles
			      slices-or-chords reduction expansion)
  "Takes as input a list of viewpoints, <viewpoint-list>, and iterates
over this list. Whenever a continuous viewpoint is found, viewpoint 
quantiles are calculated for it."
  (assert (not (null datasets)))
  (assert (listp datasets))
  (assert (every #'(lambda (v) (typep v 'viewpoints:viewpoint)) viewpoints))
  (viewpoints:reset-viewpoint-quantiles)
  (setf viewpoints:*discretise-viewpoints* nil)
  (dolist (v viewpoints)
    (when (viewpoints:continuous-p v)
      (viewpoints:set-viewpoint-quantiles
       v datasets num-quantiles :slices-or-chords slices-or-chords
       :reduction reduction :expansion expansion))))

(defun check-model-defaults (defaults &key
			      (order-bound (getf defaults :order-bound))
			      (mixtures (getf defaults :mixtures))
			      (update-exclusion (getf defaults :update-exclusion))
			      (escape (getf defaults :escape)))  
  (list :order-bound order-bound :mixtures mixtures :update-exclusion update-exclusion :escape escape))


(defun monodies-to-lists (monodies) (mapcar #'monody-to-list monodies))
(defun monody-to-list (monody) (coerce monody 'list))

; Deprecated
(defun output-information-content (resampling-predictions &optional (detail 3))
  "Processes the output of IDYOM-RESAMPLE. <detail> is an integer
specifying the desired level of detail."
  (let* ((event-ics (information-content-profiles resampling-predictions))
         (melody-ics (mapcar #'(lambda (x) (apply #'utils:average x)) event-ics))
         (overall-ics (apply #'utils:average melody-ics)))
    (case detail 
      (1 overall-ics)
      (2 (values overall-ics melody-ics))
      (3 (values overall-ics melody-ics event-ics)))))

; Deprecated
(defun information-content-profiles (dataset-predictions) 
  "Processes the output of IDYOM-RESAMPLE, multiplying
probabilities for different attributes and returning a list of lists
the information content for each event in each melody (sorted by
dataset-id)."
  (let ((results (make-hash-table)) (keys))
    ;; FOR EACH: resampling set prediction 
    (dolist (rsp dataset-predictions)
      ;; FOR EACH: feature prediction 
      (dolist (fp rsp)
        (let ((feature 
               (viewpoints:viewpoint-type 
                (prediction-sets:prediction-viewpoint fp))))
          ;; FOR EACH: song prediction 
          (dolist (sp (prediction-sets:prediction-set fp))
            (let ((song-id (prediction-sets:prediction-index sp))
                  (fsp nil))
              (pushnew song-id keys)
              ;; FOR EACH: event 
              (dolist (ep (prediction-sets:prediction-set sp))
                (let ((probability (probability ep)))
                  ;; (format t "~&~A ~A ~A~%" song-id (prediction-sets::prediction-element ep) (prediction-sets:prediction-set ep))
                  (push probability fsp)))
              (let ((fr (gethash feature results)))
                (setf (gethash feature results)
                      (cons (list song-id (nreverse fsp)) fr))))))))
    (let ((information-contents))
      ;; For each song
      ;; (print keys)
      (dolist (key (sort keys #'<))
        (let* (;; get the probabilities for each feature 
               (sp (mapcar #'(lambda (x) 
                               (cadr (assoc key (cadr x))))
                           (utils:hash-table->alist results)))
               ;; (foo (progn (print (list key sp)) nil))
               ;; multiply them together 
               (spm (apply #'mapcar #'* sp))
               ;; compute the information content 
               (sic (mapcar #'(lambda (x) (- (log x 2))) spm)))
          (push sic information-contents)))
      (nreverse information-contents))))

(defun probability (event-prediction) 
  (cadr (prediction-sets:event-prediction event-prediction)))


;;;===========================================================================
;;; Formatted output of information content etc.
;;;===========================================================================

(defun quote-string (string)
  (format nil "~s" string))

;; Deprecated
(defun format-information-content (resampling-predictions file dataset-id detail
				   &key (separator " "))
  (with-open-file (o file :direction :output :if-exists :supersede)
    (case detail 
      (1 (format t "~&Not implemented.~%"))
      (2 (format-information-content-detail=2 o resampling-predictions dataset-id
					      :separator separator))
      (3 (format-information-content-detail=3 o resampling-predictions dataset-id
					      :separator separator)))))

;; Deprecated
(defun format-information-content-detail=2 (stream resampling-predictions
					    dataset-id
					    &key (separator " "))
  (multiple-value-bind (overall-mean composition-means)
      (resampling:output-information-content resampling-predictions 2)
    (format stream "~&melody.id~Amelody.name~Amean.information.content~%"
	    separator separator)
    (do* ((ic composition-means (cdr ic))
          (cid 1 (1+ cid)))
         ((null ic) overall-mean)
      (let ((d (quote-string (md:get-description dataset-id (1- cid)))))
        (format stream "~&~A~A~A~A~A~%" cid separator d separator (car ic))))))

;; Deprecated
(defun create-key (feature attribute)
  (intern (concatenate 'string (symbol-name feature) "."
		       (format nil "~A" attribute)) :keyword))

;; Deprecated
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

;; Deprecated
(defun format-information-content-detail=3
    (stream resampling-predictions dataset-id &key (separator " ")
						(null-token "NA"))
  (if *use-old-format-method*
      (format-information-content-detail=3-old stream resampling-predictions
					       dataset-id :separator separator)
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
			  (mvs:format-event-prediction ep results
						   dataset-id composition-id
						   feature))))))
	    (maphash #'(lambda (k v)
			 (setf (gethash k results)
			       (mvs:combine-event-probabilities v features)))
		     results)
	    (mvs:add-results-to-dataframe results data)))
	(print-data data stream :null-token null-token :separator separator))))


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
  (utils:message (format nil "Getting long-term models."))
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
                   (build-model training-set alphabet))))))
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

(defun get-resampling-sets (dataset-id &key (k 10) (use-cache? t)
					 resampling-set-cache-path)
  "Returns the resampling-sets for dataset <dataset-id>. If
   <use-cache?> is T and the cache file exists, they are read from
   file, otherwise they are created and optionally cached if
   <use-cache?> is T."
  (let* ((dataset-ids (if (consp dataset-id) dataset-id (list dataset-id)))
         (filename (if resampling-set-cache-path
		       resampling-set-cache-path
		       (get-resampling-sets-filename dataset-ids k))))
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
