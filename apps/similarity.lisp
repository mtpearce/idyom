;;;; ======================================================================
;;;; File:       similarity.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2011-08-18 10:28:11 marcusp>
;;;; Time-stamp: <2022-10-10 10:56:13 marcusp>
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage #:similarity
  (:use #:cl #:utils #:md #:mvs)
  (:export #:idyom-dissimilarity)
  (:documentation "Modelling dissimilarity between pieces of music using compression distance."))

(cl:in-package #:similarity)

;; top-level function

(defun idyom-dissimilarity (dataset-ids-1 dataset-ids-2 target-viewpoints source-viewpoints 
                            &key output-path
                              (overwrite t)
                              (ltmo mvs::*ltm-params*) (stmo mvs::*stm-params*)
                              (texture :melody)
                              (voices nil)
                              (aggregation-function #'utils:average) ;; use #'+ for NCD
                              (symmetric t)
                              (normalised nil))
  (let* (;; output
         (ltmo (apply #'resampling::check-model-defaults (cons mvs::*ltm-params* ltmo)))
         (stmo (apply #'resampling::check-model-defaults (cons mvs::*stm-params* stmo)))
         (filename (apps:dataset-modelling-filename (append dataset-ids-1 dataset-ids-2)
                                                    target-viewpoints source-viewpoints
                                                    :extension
                                                    (format nil "-similarity-~A-~A-~A.dat"
                                                            (cond ((eq aggregation-function #'utils:average)
                                                                   "mean")
                                                                  ((eq aggregation-function #'+)
                                                                   "sum")
                                                                  (t (warn "Unknown aggregation function")))
                                                            (if symmetric "t" "nil") (if normalised "t" "nil"))
                                                    :pretraining-ids nil :k nil :resampling-indices nil :models :ltm
                                                    :texture texture :voices voices
                                                    :ltmo ltmo :stmo stmo))
         (filepath (when output-path (ensure-directories-exist
                                      (merge-pathnames filename (utils:ensure-directory output-path))))))
    (if (and filepath (probe-file filepath) (not overwrite))
        (format t "~&Preserving existing output file: ~A~%" filepath)
        (let* (;; modelling options
               (mvs::*models* :ltm)
               (mvs::*ltm-order-bound* (getf ltmo :order-bound))
               (mvs::*stm-order-bound* (getf stmo :order-bound))
               (mvs::*ltm-update-exclusion* (getf ltmo :update-exclusion))
               (mvs::*stm-update-exclusion* (getf stmo :update-exclusion))
               (mvs::*ltm-escape* (getf ltmo :escape))
               (mvs::*stm-escape* (getf stmo :escape))
               (mvs::*ltm-mixtures* (getf ltmo :mixtures))
               (mvs::*stm-mixtures* (getf stmo :mixtures))
               ;; data
               (dataset1 (music-data:get-event-sequences dataset-ids-1))
               (dataset1 (mapcar #'(lambda (x) (coerce x 'list)) dataset1))
               (dataset2 (music-data:get-event-sequences dataset-ids-2))
               (dataset2 (mapcar #'(lambda (x) (coerce x 'list)) dataset2))
               ;; output 
               (stream (if (null filepath) t (open filepath :direction :output :if-does-not-exist :create :if-exists :supersede)))
               ;; modelling
               (c1-cids) (c1-descriptions) (c2-cids) (c2-descriptions) (similarities))
          (viewpoints:initialise-basic-viewpoints (append dataset1 dataset2))
          (format stream "~&c1.did c1.cid c1.description c2.did c2.cid c2.description similarity~%")
          (dolist (c1 dataset1)
            (dolist (c2 dataset2)
              (let* ((similarity (compression-distance c1 c2 target-viewpoints source-viewpoints 
                                                       :set-alphabets nil :normalised normalised :symmetric symmetric
                                                       :aggregation-function #'utils:average))
                     (c1-did (md:get-dataset-index (md:get-identifier (car c1))))
                     (c1-cid (md:get-composition-index (md:get-identifier (car c1))))
                     (c1-description (md:get-description c1-did c1-cid))
                     (c2-did (md:get-dataset-index (md:get-identifier (car c2))))
                     (c2-cid (md:get-composition-index (md:get-identifier (car c2))))
                     (c2-description (md:get-description c2-did c2-cid)))
                (format stream "~&~A ~A ~A ~A ~A ~A ~A~%" c1-did c1-cid (quote-string c1-description) c2-did c2-cid 
                        (quote-string c2-description) similarity)
                (push similarity similarities)
                (push c1-cid c1-cids)
                (push c2-cid c2-cids)
                (push c1-description c1-descriptions)
                (push c2-description c2-descriptions))))
          (when (streamp stream) (close stream))
          (nreverse (mapcar #'list c1-cids c1-descriptions c2-cids c2-descriptions similarities))))))

(defun quote-string (string)
  (format nil "~s" string))


;; Compression distance

(defun compression-distance (training-composition testing-composition target-viewpoints source-viewpoints                            
                             &key (set-alphabets t) (normalised nil) (symmetric t) (aggregation-function #'utils:average))
  (let ((d12 (compress-sequence training-composition testing-composition target-viewpoints source-viewpoints 
                                :set-alphabets set-alphabets :with-training t :aggregation-function aggregation-function)))
    (cond ((and (not symmetric) (not normalised))
           ;; compression distance (asymmetric, unnormalised)
           d12)
          ((and symmetric (not normalised))
           ;; symmetric unnormalised compression distance
           (let ((d21 (compress-sequence testing-composition training-composition target-viewpoints source-viewpoints 
                                         :set-alphabets set-alphabets :with-training t :aggregation-function aggregation-function)))
             (max d12 d21)))
          ((and (not symmetric) normalised)
           ;; asymmetric normalised compression distance
           (let ((d1 (compress-sequence training-composition testing-composition target-viewpoints source-viewpoints 
                                        :set-alphabets set-alphabets :with-training nil :aggregation-function aggregation-function)))
             (/ d12 d1)))
          ((and symmetric normalised)
           ;; normalised symmetric compression distance (NCD)
           (let ((d1 (compress-sequence training-composition testing-composition target-viewpoints source-viewpoints 
                                        :set-alphabets set-alphabets :with-training nil :aggregation-function aggregation-function))
                 (d2 (compress-sequence testing-composition training-composition target-viewpoints source-viewpoints 
                                        :set-alphabets set-alphabets :with-training nil :aggregation-function aggregation-function))
                 (d21 (compress-sequence testing-composition training-composition target-viewpoints source-viewpoints 
                                         :set-alphabets set-alphabets :with-training t :aggregation-function aggregation-function)))
             (/ (max d12 d21) (max d1 d2)))))))

(defun compress-sequence (training-composition testing-composition target-viewpoints source-viewpoints                            
                          &key (with-training t) (set-alphabets t) (aggregation-function #'utils:average))
  (when set-alphabets
    (viewpoints:initialise-basic-viewpoints (list training-composition testing-composition)))
  (let* ((mvs::*models* (if with-training :ltm :stm))
         (mvs (create-mvs (if with-training training-composition nil) target-viewpoints source-viewpoints))
         (prediction (get-prediction mvs testing-composition :construct? (if with-training nil t) :predict? t
                                     :aggregation-function aggregation-function)))
    prediction))


;;; IDyOM Modelling

(defun get-prediction (mvs event-list &key (construct? nil) (predict? nil) (aggregation-function #'utils:average))
  (let* ((test-set (list event-list))
         (prediction (mvs:model-dataset mvs test-set :construct? construct? :predict? predict?)))
    (apply aggregation-function 
           (if (= (length prediction) 1)
               (prediction-sets:codelengths (car (prediction-sets:prediction-set (car prediction))))
               (mapcar #'prediction-sets:codelength (multiply-dataset-predictions prediction))))))

(defun create-mvs (training-composition target-viewpoints source-viewpoints)
  (let* ((training-set (list training-composition))
         (basic-viewpoints (viewpoints:get-viewpoints target-viewpoints))
         (derived-viewpoints (viewpoints:get-viewpoints source-viewpoints))
         (ltms (get-long-term-models derived-viewpoints training-set)))
    ;;(ppm:write-model-to-postscript (car ltms) (format nil "/home/marcusp/tmp/stree-~A-~A.ps" 
    ;;                                                  (md:get-attribute (car training-composition) 'dataset-id) 
    ;;                                                  (md:get-attribute (car training-composition) 'composition-id)))
    ;;(viewpoints:initialise-basic-viewpoints training-set)
    (mvs:make-mvs basic-viewpoints derived-viewpoints ltms)))

(defun get-long-term-models (viewpoints training-set)
  (mapcar #'(lambda (viewpoint)
              (let* ((training-set
                      (viewpoints:viewpoint-sequences viewpoint training-set))
                     (alphabet (viewpoints:viewpoint-alphabet viewpoint))
                     (model (make-instance 'ppm:ppm :alphabet alphabet)))
                (ppm:model-dataset model training-set :construct? t :predict? nil)
                model))
          viewpoints))

;; compute joint distribution of basic attribute predictions

(defun multiply-dataset-predictions (dataset-predictions)
  (multiply-sequence-predictions 
   (mapcar #'(lambda (x) (car (prediction-sets:prediction-set x))) ;; NB: there is only one sequence per dataset
           dataset-predictions)))

(defun multiply-sequence-predictions (sequence-predictions)
  (multiply-event-predictions 
   (mapcar #'(lambda (x) (prediction-sets:prediction-set x))
           sequence-predictions)))

(defun multiply-event-predictions (event-predictions)
  (let* ((p (mapcar #'(lambda (y)
                        (mapcar #'(lambda (x) (cadr (prediction-sets:event-prediction x)))
                                y))
                    event-predictions))
         (p (remove-if #'null p)))
    (apply #'mapcar #'* p)))
