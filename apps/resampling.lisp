;;;; ======================================================================
;;;; File:       resampling.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-16 18:54:17 marcusp>                           
;;;; Time-stamp: <2016-04-14 10:45:39 marcusp>                           
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
         (mvs::*stm-order-bound* (getf stmo :order-bound))
         (mvs::*stm-mixtures* (getf stmo :mixtures))
         (mvs::*stm-update-exclusion* (getf stmo :update-exclusion))
         (mvs::*stm-escape* (getf stmo :escape))
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
         ;; an output filename
         (filename (apps:dataset-modelling-filename dataset-id target-viewpoints source-viewpoints
                                                    :extension ".dat"
                                                    :pretraining-ids pretraining-ids
                                                    :k k :resampling-indices resampling-indices
                                                    :texture texture :voices voices
                                                    :models models :ltmo ltmo :stmo stmo))
         ;; the result
         (sequence-predictions))
    (dolist (resampling-set resampling-sets (values sequence-predictions filename))
      ;; (format t "~&~0,0@TResampling set ~A: ~A~%" resampling-id resampling-set)
      ;(format t "~&Resampling ~A" resampling-id)
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
			      (escape (getf defaults :escape)))  
  (list :order-bound order-bound :mixtures mixtures :update-exclusion update-exclusion :escape escape))


(defun monodies-to-lists (monodies) (mapcar #'monody-to-list monodies))
(defun monody-to-list (monody) (coerce monody 'list))

(defun output-information-content (resampling-predictions &optional (detail 3))
  "Processes the output of IDYOM-RESAMPLE. <detail> is an integer
specifying the desired level of detail."
  (let* ((event-ics (information-content-profiles resampling-predictions))
         (melody-ics (mapcar #'mean event-ics))
         (overall-ics (mean melody-ics)))
    (case detail 
      (1 overall-ics)
      (2 (values overall-ics melody-ics))
      (3 (values overall-ics melody-ics event-ics)))))
    
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

(defun mean (numbers) (/ (apply #'+ numbers) (length numbers)))


;;;===========================================================================
;;; Formatted output of information content etc.
;;;===========================================================================

(defun quote-string (string)
  (format nil "~s" string))

(defun format-information-content (resampling-predictions file dataset-id detail)
  (with-open-file (o file :direction :output :if-exists :supersede)
    (case detail 
      (1 (format t "~&Not implemented.~%"))
      (2 (format-information-content-detail=2 o resampling-predictions dataset-id))
      (3 (format-information-content-detail=3 o resampling-predictions dataset-id)))))

(defun format-information-content-detail=2 (stream resampling-predictions dataset-id)
  (multiple-value-bind (overall-mean composition-means)
      (resampling:output-information-content resampling-predictions 2)
    (format stream "~&melody.id melody.name mean.information.content~%")
    (do* ((ic composition-means (cdr ic))
          (cid 1 (1+ cid)))
         ((null ic) overall-mean)
      (let ((d (quote-string (md:get-description dataset-id (1- cid)))))
        (format stream "~&~A ~A ~A~%" cid d (car ic))))))

(defun format-information-content-detail=3 (stream resampling-predictions dataset-id) 
  (let ((results (make-hash-table :test #'equal))
        (features))
    (flet ((create-key (feature attribute) (intern (concatenate 'string (symbol-name feature) "." (format nil "~A" attribute)) :keyword))
           (sort-function (x y) (let ((x1 (car x)) (x2 (cadr x)) (y1 (car y)) (y2 (cadr y))) (if (= x1 y1) (< x2 y2) (< x1 y1)))))
      ;; FOR EACH: resampling set prediction 
      (dolist (rsp resampling-predictions)
        ;; FOR EACH: feature prediction 
        (dolist (fp rsp)
          (let ((feature (viewpoints:viewpoint-type (prediction-sets:prediction-viewpoint fp))))
            (pushnew feature features)
            ;; FOR EACH: song prediction 
            (dolist (sp (prediction-sets:prediction-set fp))
              (let ((composition-id (prediction-sets:prediction-index sp)))
                ;; FOR EACH: event 
                (dolist (ep (prediction-sets:prediction-set sp))
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
                      (setf (gethash 'dataset.id event-results) dataset-id)
                      (setf (gethash 'melody.id event-results) (1+ composition-id))
                      (setf (gethash 'note.id event-results) (1+ event-id))
                      (setf (gethash 'melody.name event-results)
                            (quote-string (md:get-description
                                           dataset-id
                                           composition-id)))
                      ;; TODO - this needs to be specific to each type of music-object (music-event, music-slice etc.)
                      (dolist (attribute (viewpoints:get-basic-types event))
                        (let ((value (md:get-attribute event attribute)))
                          (when (member attribute '(:dur :bioi :deltast :onset) :test #'eq)
                            (setf value (* value (/ timebase 96))))
                          (setf (gethash attribute event-results) value))))
                    ;; Store feature prediction
                    (dolist (o orders) ; orders
                      (setf (gethash (create-key feature (car o)) event-results) (cadr o)))
                    (when weights
                      (dolist (w weights) ; weights
                        (setf (gethash (create-key feature (car w)) event-results) (cadr w))))
                    (setf (gethash (create-key feature 'probability) event-results) probability)
                    (setf (gethash (create-key feature 'information.content) event-results) (- (log probability 2)))
                    (setf (gethash (create-key feature 'entropy) event-results) (float (prediction-sets:shannon-entropy distribution) 0.0))
                    (setf (gethash (create-key feature 'distribution) event-results) distribution)
                    (dolist (p distribution)
                      (setf (gethash (create-key feature (car p)) event-results) (cadr p)))
                    (setf (gethash (list composition-id event-id) results) event-results))))))))
      ;; Combine probabilities from different features
      (maphash #'(lambda (k v)
                   (let* ((event-results v)
                          (probability-keys (mapcar #'(lambda (f) (create-key f 'probability)) features))
                          (probabilities (mapcar #'(lambda (x) (gethash x v)) probability-keys))
                          (probability (apply #'* probabilities))
                          (distribution-keys (mapcar #'(lambda (f) (create-key f 'distribution)) features))
                          (distributions (mapcar #'(lambda (x) (gethash x v)) distribution-keys))
                          (distribution (mapcar #'(lambda (x) (let ((elements (mapcar #'first x))
                                                                    (probabilities (mapcar #'second x)))
                                                                (list elements (apply #'* probabilities))))
                                                (apply #'utils:cartesian-product distributions))))
                     (setf (gethash 'probability event-results) probability)
                     (setf (gethash 'information.content event-results) (- (log probability 2)))
                     (setf (gethash 'entropy event-results) (prediction-sets:shannon-entropy distribution))
                     ;; TODO elements of combined distribution
                     (mapc #'(lambda (key) (remhash key event-results)) distribution-keys)
                     (setf (gethash k results) event-results)))
               results)
      ;; Sort values and print
      (let ((sorted-results (utils:hash-table->sorted-alist results #'sort-function))
            (print-header t))
        (dolist (entry sorted-results)
          (when print-header
            (maphash #'(lambda (k v) (declare (ignore v)) (format stream "~A " (string-downcase (symbol-name k)))) (cdr entry))
            (setf print-header nil))
          (format stream "~&")
          (maphash #'(lambda (k v) (declare (ignore k)) (format stream "~A " (if v v "NA"))) (cdr entry)))))))


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
  (if (= count k) 
      (let ((list (generate-integers 0 (1- count))))
        (mapcar #'(lambda (x) 
                    (list (list 'test (list x)) 
                          (list 'train (remove x list :test #'=))))
                list))
      (let* ((new-count (+ (- count (rem count k)) k))
             (list (generate-integers 0 (1- new-count)))
             (set-size (floor (length list) k))
             (test-sets (remove-extras (partition-list list set-size) count)))
        (mapcar #'(lambda (s) (list (list 'test s)
                                    (list 'train (get-training-indices s count))))
                test-sets))))

(defun get-training-indices (test-indices composition-count)
  "Removes members of <test-indices> from <all-indices>."
  (remove-if #'(lambda (i) (member i test-indices))
             (generate-integers 0 (- composition-count 1))))

(defun remove-extras (sublists count)
  "Removes all numbers greater than or equal to count from all sublists
   of <sublists>." 
  (mapcar #'(lambda (list)
              (remove-if #'(lambda (n) (>= n count)) list))
          sublists))

(defun partition-list (list n &optional result)
  "Returns a list of sublists each of which contains <n> elements of <list>
   assigned randomly without replacement from <list>. <n> must be an integral
   divisor of the length of <list>."
  (cond ((null list) result)
        ((< (length list) n) (cons list result))
        (t (multiple-value-bind (r l)
               (random-select list n)
             (partition-list l n (cons r result))))))

(defun random-select (list n &optional result new-list)
  "Given a <list> and a number <n> returns two values: the first is a
   list containing <n> elements drawn at random from
   from <list> without replacement; and the second  is <list> with
   those elements removed."
  (cond ((= n 0) (values (reverse result) (reverse (append list new-list))))
        ((< (random 1.0 (make-random-seed t)) (/ n (length list)))
         (random-select (cdr list) (- n 1) (cons (car list) result) new-list))
        (t (random-select (cdr list) n result (cons (car list) new-list)))))

