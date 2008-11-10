;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-             
;;;; ======================================================================
;;;; File:       resampling.lisp
;;;; Author:     Marcus  Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2003-04-16 18:54:17 marcusp>                           
;;;; Time-stamp: <2008-11-10 17:21:44 marcusp>                           
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
  (utils:string-append apps:*root-dir* "data/models/"))
(defparameter *resampling-dir*
  (utils:string-append apps:*root-dir* "data/resampling/"))

;;;===========================================================================
;;; Dataset Prediction 
;;;===========================================================================
    
(defun dataset-prediction (dataset-id basic-attributes attributes
                           &key pretraining-ids (k 10) (models :both+)
                           resampling-indices)
  "Top-level Call -- returns the average cross-entropy of a model with the
   supplied parameters on <testing-sequence> where the long-term model has
   been trained on <training-sequence> and both <testing-sequence> and
   <training-sequence> are composed from <alphabet>. If <models> is 'ltm
   only a long-term model is used, else if it is 'stm only a short-term
   model is used and otherwise both models are used and their predictions
   combined."
  (let* ((mvs::*models* models)
         (dataset (get-event-sequences dataset-id))
         (viewpoints (get-viewpoints attributes))
         (pretraining-set (apply #'get-event-sequences pretraining-ids))
         (basic-viewpoints 
          (viewpoints:get-basic-viewpoints basic-attributes 
                                           (append dataset pretraining-set)))
         (k (if (eq k :full) (length dataset) k))
         (resampling-sets (get-resampling-sets dataset-id :k k))
         (resampling-id 0)
         (sequence-predictions)
         (resampling-indices (if (null resampling-indices) 
                                 (utils:generate-integers 0 (1- k))
                                 resampling-indices)))
    (dolist (resampling-set resampling-sets sequence-predictions)
      ;;(format t "~&~0,0@TResampling set ~A~%" resampling-id)
      (when (member resampling-id resampling-indices)
        (let* ((training-set (get-training-set dataset resampling-set))
               (training-set 
                (monodies-to-lists (append pretraining-set training-set)))
               (test-set 
                (monodies-to-lists (get-test-set dataset resampling-set)))
               (ltms (get-long-term-models viewpoints training-set
                                           pretraining-ids dataset-id
                                           resampling-id k))
               (mvs (make-mvs basic-viewpoints viewpoints ltms))
               (predictions 
                (mvs:model-dataset mvs test-set :construct? t :predict? t)))
          (push predictions sequence-predictions)))
      (incf resampling-id))))

(defun monodies-to-lists (monodies) (mapcar #'monody-to-list monodies))
(defun monody-to-list (monody) (coerce monody 'list))

(defun output-information-content (resampling-predictions &optional (detail 3))
  "Processes the output of DATASET-PREDICTION. <detail> is an integer
specifying the desired level of detail."
  (let* ((event-ics (information-content-profiles resampling-predictions))
         (melody-ics (mapcar #'mean event-ics))
         (overall-ics (mean melody-ics)))
    (case detail 
      (1 overall-ics)
      (2 (values overall-ics melody-ics))
      (3 (values overall-ics melody-ics event-ics)))))
    
(defun information-content-profiles (dataset-predictions) 
  "Processes the output of DATASET-PREDICTION, multiplying
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
                  (push probability fsp)))
              (let ((fr (gethash feature results)))
                (setf (gethash feature results)
                      (cons (list song-id (nreverse fsp)) fr))))))))
    (let ((information-contents))
      ;; For each song
      (dolist (key (sort keys #'<))
        (let* (;; get the probabilities for each feature 
               (sp (mapcar #'(lambda (x) 
                               (cadr (assoc key (cadr x))))
                           (utils:hash-table->alist results)))
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
;;; Conklin (1990), Conklin and Witten (1995), and Pearce (2005)
;;;===========================================================================

(defun conklin90 (&optional (dataset 2) (resampling-indices '(0)))
  (format 
   t "~&Simulation of the pitch-based features of Conklin (1990, Experiment 8, p. 115).~%")
  (let ((viewpoints '(cpint
                      (cpint ioi)
                      (cpintfiph contour)
                      (cpintfref cpintfip)
                      (cpintfib barlength))))
    (output-information-content 
     (dataset-prediction dataset '(cpitch) viewpoints
                         :resampling-indices resampling-indices)
     1)))

(defun conkwit95 (&optional (dataset 2) (resampling-indices '(1)))
  (format 
   t "~&Simulation of the experiments of Conklin & Witten (1995, Table 4).~%")
  (let ((systems '((cpitch)
                   (cpint)
                   ((cpint ioi))
                   ((cpint ioi) cpitch)
                   ((cpintfref cpint))
                   ((cpintfref cpint) (cpint ioi))
                   ((cpintfref cpint) (cpint ioi) cpitch)
                   ((cpintfref cpint) (cpint ioi) cpitch (cpintfref fib))))
        (system-id 1))
    (dolist (system systems)
      (let ((mean-ic 
             (output-information-content 
              (dataset-prediction dataset '(cpitch) system
                                  :resampling-indices resampling-indices)
              1)))
        (format t "~&System ~A; Mean Information Content: ~,2F ~%" system-id 
                mean-ic)
        (incf system-id)))))

(defun pearce05 (&optional (dataset-id 1))
  (format t "~&Simulation of the experiments of Pearce (2005, Table 9.1/9.8, p. 191/206).~%")
  (let ((systems '((A (cpitch))
                   (B (cpintfip (cpintfref dur-ratio) thrfiph))
                   (C (thrfiph cpintfip (cpint dur-ratio) (cpintfref dur) 
                       thrtactus (cpintfref fib) (cpitch dur) 
                       (cpintfref cpintfip) (cpint dur)))
                   (D (cpintfiph (cpintfref dur) (cpint inscale) 
                       (cpint dur-ratio) (cpintfref liph) thrfiph 
                       (cpitch dur) (cpintfref cpintfip) 
                       (cpintfref mode) (cpint dur))))))
    (dolist (system systems)
      (let ((mean-ic 
             (output-information-content 
              (dataset-prediction dataset-id '(cpitch) (cadr system) :k 10)
              1)))
        (format t "~&System ~A; Mean Information Content: ~,2F ~%" (car system) 
                mean-ic)))))


;;;===========================================================================
;;; Viewpoint Selection 
;;;===========================================================================

(defun select-viewpoints (dataset-id basic-attributes attributes
                          &key pretraining-ids (k 10) (models :both+)
                          resampling-indices start-state cache-file)
  (when cache-file (viewpoint-selection:load-vs-cache cache-file :cl-user))
  (viewpoint-selection:run-hill-climber
   attributes 
   start-state
   #'(lambda (x) 
       (output-information-content 
        (dataset-prediction dataset-id basic-attributes x 
                            :pretraining-ids pretraining-ids :k k 
                            :models models 
                            :resampling-indices resampling-indices)
        1))
   :desc)
  (when cache-file (viewpoint-selection:store-vs-cache cache-file :cl-user)))

  
;;;===========================================================================
;;; Constructing the Long term models 
;;;===========================================================================

(defun get-long-term-models (viewpoints training-set pretraining-ids
                             training-id resampling-id resampling-count)
  "Returns a vector of long-term models -- one for each viewpoint in
<viewpoints> -- trained on <training-set> and initialised with the
supplied keyword parameters."
  (mapcar #'(lambda (viewpoint)
                (let ((filename
                       (get-model-filename viewpoint pretraining-ids
                                           training-id resampling-id
                                           resampling-count))
                      (training-set
                       (viewpoint-sequences viewpoint training-set))
                      (alphabet (viewpoint-alphabet viewpoint)))
                  (get-model filename alphabet training-set)))
          viewpoints))

(defun get-model-filename (viewpoint pretraining-ids training-id resampling-id
                           resampling-count)
  "Returns the filename in *model-directory* containing the ppm model
for <viewpoint> in <dataset-id>."
  (string-append *model-dir*
                 (viewpoint-name viewpoint)
                 (if (null pretraining-ids) "_NIL"
                     (string-append
                      "_"
                      (subseq (apply #'string-append 
                                     (mapcar #'(lambda (x) (format nil "-~A" x))
                                             pretraining-ids)) 1)))
                 (if (null training-id) "_NIL" (format nil "_~A" training-id))
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

(defun get-resampling-sets (dataset-id &key (create? nil) (k 10))
  "Returns the resampling-sets for dataset <dataset-id>. If <create?> is
   null they are read from file, otherwise they are created." 
  (let* ((dataset-ids (if (consp dataset-id) dataset-id (list dataset-id)))
         (filename (get-resampling-sets-filename dataset-ids k)))
    (unless (and (null create?) (file-exists filename))
      (let* ((composition-count (apply #'+ (mapcar #'md:count-compositions 
                                                   dataset-ids)))
             (resampling-sets (create-resampling-sets composition-count k)))
        (write-resampling-sets-to-file resampling-sets filename)))
    (let ((resampling-sets (read-object-from-file filename :resampling)))
      resampling-sets)))
        
(defun write-resampling-sets-to-file (resampling-sets filename)
  "Writes <resampling-sets> to <file>." 
  (write-object-to-file resampling-sets filename :resampling)
  (format t "~%Written resampling set to ~A." filename))

(defun get-resampling-sets-filename (dataset-ids k)
  "Returns the filename in *resampling-sets-directory* containing the
   resampling-sets for <dataset-id>." 
  (string-append *resampling-dir*
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
  (cond ((= n 0) (values result (append list new-list)))
        ((< (random 1.0) (/ n (length list)))
         (random-select (cdr list) (- n 1) (cons (car list) result) new-list))
        (t (random-select (cdr list) n result (cons (car list) new-list)))))

