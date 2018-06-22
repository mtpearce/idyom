;;;; ======================================================================
;;;; File:       viewpoint-selection.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-10-02 18:54:17 marcusp>                           
;;;; Time-stamp: <2018-06-22 13:32:44 marcusp>                           
;;;; ======================================================================

(cl:in-package #:viewpoint-selection)

;;;========================================================================
;;; Top-level functions
;;;========================================================================

(defun dataset-viewpoint-selection (dataset-id target-viewpoints viewpoint-list
                                    &key pretraining-ids (k 10) resampling-indices (models :both+) 
                                      (ltmo mvs::*ltm-params*) (stmo mvs::*stm-params*)
                                      (texture :melody) voices
                                      (max-links 2)
                                      (dp nil) ; decimal-places of interest
                                      (method :hill-climber) ; search method: best-first or hill-climber
                                      (output-path nil)
                                      (separator " ")
                                      (overwrite nil))
  (let ((cache-filename (apps:dataset-modelling-filename dataset-id target-viewpoints
                                                         nil ; we don't mind which derived viewpoints are used
                                                         :extension ".cache"
                                                         :pretraining-ids pretraining-ids
                                                         :resampling-indices resampling-indices
                                                         :texture texture :voices voices
                                                         :k k
                                                         :models models)))
    (viewpoint-selection:initialise-vs-cache)
    (viewpoint-selection:load-vs-cache cache-filename :cl-user)
    (let* ((eval-function 
            #'(lambda (source-viewpoints)
                (prog1
                    (when (verify-viewpoint-system target-viewpoints source-viewpoints)
		      ;;(format t "~&Evaluating ~a ..." source-viewpoints)
                      (let* ((output (resampling:idyom-resample dataset-id target-viewpoints source-viewpoints
                                                                :pretraining-ids pretraining-ids
                                                                :resampling-indices resampling-indices
                                                                :k k
                                                                :models models
                                                                :ltmo ltmo :stmo stmo))
                             (filename (when output-path
                                         (apps:dataset-modelling-filename dataset-id target-viewpoints source-viewpoints
                                                                          :extension ".dat"
                                                                          :detail 3
                                                                          :pretraining-ids pretraining-ids
                                                                          :k k :resampling-indices resampling-indices
                                                                          :texture texture :voices voices
                                                                          :models models :ltmo ltmo :stmo stmo)))
                             (filepath (when output-path (ensure-directories-exist
                                                          (merge-pathnames filename (utils:ensure-directory output-path)))))
                             (ic (resampling:output-information-content output 1)))
                        (print filepath)
                        (format t "~&Mean information content for ~a~&is ~a" source-viewpoints ic)
                        (when (and filepath (not (and (probe-file filepath) (not overwrite))))
                          (resampling:format-information-content output filepath dataset-id 3 :separator separator))
                        ic))
                  (viewpoint-selection:store-vs-cache cache-filename :cl-user))))
           (start-state (if (and (< max-links 2) (> (length target-viewpoints) 1))
                            target-viewpoints
                            nil))
           (selected-state
            (case method
              (:hill-climber
               (viewpoint-selection:run-hill-climber viewpoint-list start-state eval-function :desc dp))
              (:best-first
               (viewpoint-selection:run-best-first viewpoint-list start-state eval-function :desc dp))
              (t 
               (format t "~&Unknown search method supplied to dataset-viewpoint-selection. Use :hill-climber or :best-first.~%")))))
      (viewpoint-selection:store-vs-cache cache-filename :cl-user)
      (format t "~&The selected viewpoint system with a mean IC of ~A is ~A~%" (record-weight selected-state) (record-state selected-state))
      (record-state selected-state))))
    
(defun verify-viewpoint-system (basic-viewpoints viewpoints)
  (let* ((typesets (reduce #'append (mapcar #'(lambda (v) (viewpoints:viewpoint-typeset (viewpoints:get-viewpoint v))) viewpoints)))
         (matches (mapcar #'(lambda (basic-viewpoint)
                              (let ((basic-viewpoint (viewpoints:get-viewpoint basic-viewpoint)))
                                (find basic-viewpoint typesets 
                                      :key #'(lambda (x) (viewpoints:get-viewpoint x))
                                      :test #'viewpoints:viewpoint-equal)))
                          basic-viewpoints)))
    (if (position nil matches) nil t)))


;;;========================================================================
;;; Caching of record weights 
;;;========================================================================

(defparameter *vs-cache* '())
(defparameter *vs-cache-dir* mvs:*ep-cache-dir*)

(defun vs-cache-path (filename) 
  (utils:string-append (namestring *vs-cache-dir*) filename))

(defun initialise-vs-cache () (setf *vs-cache* nil))

(defun store-vs-cache (filename package) 
  (utils:write-object-to-file *vs-cache* (vs-cache-path filename) package))

(defun load-vs-cache (filename package) 
  (setf *vs-cache* 
        (utils:read-object-from-file (vs-cache-path filename) package)))

(defun cached-weight (viewpoints)
  (nth 1 (find-if #'(lambda (x)
                      (= (length (union x viewpoints :test #'equal))
                         (length x)
                         (length viewpoints)))
                  *vs-cache*
                  :key #'car)))

(defun cache-state (viewpoints weight)
  (push (list viewpoints weight) *vs-cache*))

(defun caching-eval-function (function) 
  #'(lambda (x) 
      (let ((cached-weight (cached-weight x)))
        (if (null cached-weight) 
            (let ((weight (funcall function x)))
              (cache-state x weight)
              weight)
          cached-weight))))

;;;========================================================================
;;; States in the feature space 
;;;========================================================================

(defstruct record
  (state nil)
  (weight nil))

(defun record-equal (record1 record2)
  (let ((state1 (record-state record1))
        (state2 (record-state record2)))
    (and (null (set-difference state1 state2 :test #'equal))
         (null (set-difference state2 state1 :test #'equal)))))

(defun better-than-asc-0 (r1 r2 &key dp) 
  (better-than-asc r1 r2 :accessor #'car :dp dp))

(defun better-than-asc (r1 r2 &key (accessor #'identity) dp)
  (let ((w1 (unless (null r1) (funcall accessor (record-weight r1))))
        (w2 (unless (null r2) (funcall accessor (record-weight r2)))))
    (cond ((and (null w1) (null w2))
           '())
          ((null w1) 
           '())
          ((null w2)
           t)
          (t (> (round-off w1 dp) (round-off w2 dp))))))

(defun better-than-desc-1 (r1 r2 &key dp) 
  (better-than-desc r1 r2 :accessor #'cadr :dp dp))

(defun better-than-desc (r1 r2 &key (accessor #'identity) dp)
  (let ((w1 (unless (null r1) (funcall accessor (record-weight r1))))
        (w2 (unless (null r2) (funcall accessor (record-weight r2)))))
    (cond ((and (null w1) (null w2))
           '())
          ((null w1) 
           '())
          ((null w2)
           t)
          (t (< (round-off w1 dp) (round-off w2 dp))))))

(defun round-off (x dp) (if dp (utils:round-to-nearest-decimal-place x dp) x))

;;;========================================================================
;;; Best first search 
;;;========================================================================
 
(defun run-best-first (features start-state eval-function better-than dp)
  (print-header) 
  (let* ((eval-function (caching-eval-function eval-function))
         (start-weight (unless (null start-state) 
                         (funcall eval-function start-state)))
         (start-record (make-record :state start-state :weight start-weight))
         (better-than (if (eql better-than :asc) 
                          #'(lambda (x y) (better-than-asc x y :dp dp))
                          #'(lambda (x y) (better-than-desc x y :dp dp))))
         (unused 
          (sort (descendents start-record eval-function features better-than)
                better-than)))
    (print-record start-record)
    (prog1 (best-first start-record unused eval-function features unused 
                       start-record better-than)
      (print-double-separator))))

(defun best-first (current-record open-records eval-function features cache
                   current-best better-than)
  ;(format t "~&Open: ~A~%Current: ~A~%" open-records current-record)
  (if (null open-records) current-best
      (let* ((children 
              (descendents current-record eval-function features better-than))
             (new-open-records 
              (insert-by-weight children open-records better-than)))
        (best-first (car new-open-records)
                    (cdr new-open-records)
                    eval-function
                    features
                    (insert-by-weight children cache better-than)
                    (if (funcall better-than current-record current-best)
                        (progn (print-record current-record) current-record)
                        current-best)
                    better-than))))
             
(defun insert-by-weight (children sorted-list better-than)
  (if (null children) sorted-list
      (let ((head (car children)))
        (if (find head sorted-list :test #'record-equal)
            sorted-list
            (insert head (insert-by-weight (cdr children) sorted-list 
                                           better-than)
                    better-than)))))

(defun insert (item sorted-list better-than)
  (cond ((null sorted-list) 
         (list item))
        ((funcall better-than item (car sorted-list))
         (cons item sorted-list))
        (t 
         (cons (car sorted-list) (insert item (cdr sorted-list) better-than)))))

(defun descendents (current-record eval-function features better-than)
  (let ((current-state (record-state current-record))
        (records '()))
    (dolist (f features records)
      (unless (member f current-state :test #'equal)
        (let* ((state (cons f current-state))
               (record (make-record :state state
                                    :weight (funcall eval-function state))))
          (when (funcall better-than record current-record)
            (push record records)))))))


;;;========================================================================
;;; Forward and Backward selection hill climbing
;;;========================================================================

(defun run-hill-climber (features start-state eval-function better-than dp)
  ;(print-header) 
  (let* ((eval-function (caching-eval-function eval-function))
         (start-weight (unless (null start-state) 
                         (funcall eval-function start-state)))
         (start-record (make-record :state start-state :weight start-weight))
         (unused (remove-if #'(lambda (x) 
                                (member x start-state 
                                        :test #'viewpoints:attribute-equal))
                            features))
         (better-than (case better-than 
                        (:asc #'(lambda (x y) (better-than-asc x y :dp dp)))
                        (:desc #'(lambda (x y) (better-than-desc x y :dp dp)))
                        (:asc-0 #'better-than-asc-0)
                        (:desc-1 #'better-than-desc-1))))
    (prog1 (hill-climber start-record unused eval-function better-than)
      (print-double-separator))))

(defun hill-climber (current-record unused-features eval-function better-than)
  (flet ((new-unused (best-record)
           (let ((best-state 
                  (unless (null best-record) (record-state best-record))))
             (remove-if #'(lambda (x) (member x best-state :test #'equal))
                        unused-features)))
         (descendents (new-states eval-function)
           (let ((records '()))
             (dolist (state new-states)
               (let ((weight (funcall eval-function state)))
                 (push (make-record :state state :weight weight) records)))
             (sort records better-than))))
    (print-record2 current-record)
    (if (null unused-features) current-record
        (let* ((b-children
                (descendents (backward-states current-record) eval-function))
               (best-record (car b-children)))
          (if (funcall better-than best-record current-record)
              (hill-climber best-record (new-unused best-record) eval-function
                            better-than)
              (let* ((f-children (descendents (forward-states current-record 
                                                              unused-features)
                                              eval-function))
                     (best-record (car f-children)))
                (if (funcall better-than best-record current-record)
                    (hill-climber  best-record (new-unused best-record)
                                   eval-function better-than)
                    current-record)))))))

(defun forward-states (current-record unused)
  (let ((state (record-state current-record)))
    (mapcar #'(lambda (u) (cons u state)) unused)))

(defun backward-states (current-record)
  (let* ((state (record-state current-record))
         (length (length state)))
    (when (> length 2) (utils:combinations state (1- length)))))


;;;========================================================================
;;; Pretty printing
;;;========================================================================

(defun print-header ()
  (print-double-separator)
  (format t "~&   System ~1,73T    Score")
  (print-separator))

(defun print-separator ()
  (format t "~& ~A" (make-sequence 'string 87 :initial-element #\-)))

(defun print-double-separator ()
  (format t "~& ~A" (make-sequence 'string 87 :initial-element #\=)))

(defun print-record (record)
  (format t "~&   ~A ~1,73T    ~A" (record-state record) (record-weight record)))

(defun print-record2 (record)
  (format t "~&~%Selected system ~A, ~&mean IC = ~A~%~%" (record-state record) (record-weight record)))
