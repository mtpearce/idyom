;;;; ======================================================================
;;;; File:       segmentation.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2008-03-13 13:07:12 marcusp>
;;;; Time-stamp: <2023-04-22 08:58:37 marcusp>
;;;; ======================================================================

(cl:defpackage #:segmentation
  (:use #:cl)
  (:export #:*boundary* #:*no-boundary* #:peak-picker
           #:make-segmentation-results #:print-segmentation-results
           #:precision #:recall #:f1 #:fallout #:test-segmentation
           #:idyom-segmentation))

(cl:in-package #:segmentation) 

(defparameter *boundary* 1
  "An object used to represent a point where a boundary occurs.")

(defparameter *no-boundary* 0
  "An object used to represent a point where a boundary doesn't occur.")

(defun boundary-p (x) (eq x *boundary*))

;;;; Top-level

(defun idyom-segmentation (dataset-id target-viewpoints source-viewpoints
                           &key
                             ;; TODO: add remaining IDyOM parameters
                             pretraining-ids
                             (k 10) 
                             (models :both+)
                             ;; segmentation parameters
                             (threshold 1.28) (window-size nil)
                             (combination-function #'(lambda (ic e) (declare (ignore e)) (if (boundary-p ic) *boundary* *no-boundary*)))
                             (mean #'linearly-weighted-arithmetic-mean)
                             ;; output parameters
                             (print t))
  (multiple-value-bind (ic1 e1 ic2 e2 ic3 e3)
      (idyom:idyom dataset-id target-viewpoints source-viewpoints :models models :k k :pretraining-ids pretraining-ids :information-measure '(:ic :entropy))
    (declare (ignore ic1 e1 ic2 e2))
    (let* ((predicted (mapcar #'(lambda (ic e)
                                  (let ((icp (segmentation:peak-picker (mapcar #'+ ic (cons 0 (butlast e))) :k threshold :window-size window-size :mean mean))
                                        (ep (segmentation:peak-picker (cons 0 (butlast e)) :k threshold :window-size window-size :mean mean)))
                                    (mapcar combination-function icp ep)))
                              ic3 e3))
           (actual (ground-truth dataset-id))
           (mean-result (mapcar #'(lambda (p a)
                                    (multiple-value-list 
                                     (test-segmentation p a nil)))
                                predicted actual))
           (mean-p (apply #'utils:average (mapcar #'first mean-result)))
           (mean-r (apply #'utils:average (mapcar #'second mean-result)))
           (mean-f (apply #'utils:average (mapcar #'third mean-result))))
      (multiple-value-bind (p r f)
          (segmentation:test-segmentation (reduce #'append predicted) (reduce #'append actual) print)
        (when print
          (format t "~&Mean Precision = ~,2F; Mean Recall = ~,2F; Mean F_1 = ~,2F~%" mean-p mean-r mean-f))
        (list p r f mean-p mean-r mean-f)))))

  
;;;; Peak picking

(defun peak-picker (boundary-strengths &key (k 1.28) (window-size nil)
                                         (mean #'linearly-weighted-arithmetic-mean))
  "Given a list of boundary strength values (i.e., information
content) for each note in a piece, returns a list of binary values (0
or 1) corresponding to each note, where 1 = first note of a phrase, 0
otherwise. The first note is considered an implicit phrase boundary."
  (let* ((result (list *no-boundary* *boundary*))
         (length (length boundary-strengths))
         (m 0))
    (do* ((bs boundary-strengths (cdr bs)) 
          (n length (1- n))
          (bsn-1 (first bs) (first bs))
          (bsn (second bs) (second bs))
          (bsn+1 (third bs) (third bs))
          (bs- (list bsn-1) (append bs- (list bsn-1)))) 
         ((< n 3) (nreverse (cons *no-boundary* result)))
      ;; (format t "~&n = ~A; bsn-1 = ~A; bsn = ~A; bsn+1 = ~A; bs- = ~A~%"
      ;;         n bsn-1 bsn bsn+1 bs-)
      (incf m)
      (when (> m 1) ; first two notes are not boundary candidates
        (let ((window (typecase window-size
                        (number (first-n bs- window-size))
                        (t bs-))))
          (if (and (>= bsn bsn+1)
                   (> bsn bsn-1)
                   (> bsn (+ (* k (apply #'utils:sd window))
                             (funcall mean window))))
              (push *boundary* result)
              (push *no-boundary* result)))))))

(defun first-n (list n)
  (if (<= (length list) n)
      list
      (subseq list 0 n)))

(defun weighted-arithmetic-mean (numbers weights)
  (/ (apply #'+ (mapcar #'* numbers weights))
     (apply #'+ weights)))

(defun linearly-weighted-arithmetic-mean (numbers)
  (let ((weights nil))
    (do ((n (length numbers) (1- n))
         (k 1 (1+ k)))
        ((zerop n))
      (push k weights))
    (weighted-arithmetic-mean numbers (reverse weights))))

(defun exponentially-weighted-arithmetic-mean (numbers &key (alpha 0.5))
  (let ((weights nil))
    (do ((n (length numbers) (1- n)))
        ((zerop n))
      (push (expt (- 1 alpha) (1- n)) weights))
    (weighted-arithmetic-mean numbers (reverse weights))))

(defun logarithmically-weighted-arithmetic-mean (numbers &key (alpha 0.5))
  (let ((weights nil))
    (do ((n (1+ (length numbers)) (1- n)))
        ((= 1 n))
      (push (abs (log (- 1 alpha) n)) weights))
    (weighted-arithmetic-mean numbers (reverse weights))))


;;;; Ground truth for a given dataset

(defun ground-truth (dataset-id)
  "Returns the ground truth segmentation for <dataset-id>, if it
exists, using the 'phrase attribute, coded as follows: 1 = first note
of a phrase; 0 otherwise. The first note of a piece is considered
an implicit phrase boundary (1)."
  (viewpoints:viewpoint-sequences (viewpoints:get-viewpoint 'fiph) (md:get-music-objects dataset-id nil)))


;;;; Evaluation

(defun test-segmentation (predicted actual &optional (print t))
  "Test a segmentation <predicted> against the actual ground truth
segmentation <actual> and, optionally, print the results, depending on
the value of <print>."
  (let* ((result (make-segmentation-results predicted actual))
         (p (precision result))
         (r (recall result))
         (f (f1 result)))
    (when print
      (print-segmentation-results result t)
      (format t "~&Precision = ~,2F; Recall = ~,2F; F_1 = ~,2F~%" p r f))
    (values p r f)))

(defstruct (segmentation-results (:constructor %make-segmentation-results)
                                 (:print-object print-segmentation-results))
  ;;; a structure to store/print results
  (tp 0)
  (tn 0) 
  (fp 0)
  (fn 0))

(defun make-segmentation-results (predicted actual)
  (%make-segmentation-results :tp (true-positives predicted actual)
                              :tn (true-negatives predicted actual)
                              :fp (false-positives predicted actual)
                              :fn (false-negatives predicted actual)))

(defun print-segmentation-results (object stream)
  (let* ((row-labels (list "True" "False"))
         (column-labels (list "Positives" "Negatives"))
         (tp (segmentation-results-tp object))
         (tn (segmentation-results-tn object))
         (fp (segmentation-results-fp object))
         (fn (segmentation-results-fn object))
         (table (make-array '(2 2) :element-type 'fixnum 
                            :initial-contents (list (list tp tn) (list fp fn)))))
    (print-frequency-table table :row-labels row-labels
                           :column-labels column-labels 
                           :stream stream)))

(defun print-frequency-table (table &key row-labels column-labels (stream t))
  "Pretty prints a 2d frequency table TABLE onto STREAM." 
  (let ((nrows (car (array-dimensions table)))
        (ncols (cadr (array-dimensions table))))
    (flet ((row-sep (sizes) 
             (dotimes (i (apply #'+ (cons (length sizes) sizes)))
               (princ "-" stream))
             (terpri stream))
           (get-rows () 
             (let ((rows '())
                   (column-labels (mapcar #'(lambda (x) (format nil "~A" x)) 
                                          column-labels))
                   (row-labels (mapcar #'(lambda (x) (format nil "~A" x)) 
                                       row-labels))
                   (column-counts (make-hash-table)))
               (push (append (list "|" "" "|") column-labels (list "|" "")) rows)
               (dotimes (i nrows)
                 (let ((row '())
                       (row-count 0))
                   (push "|" row)
                   (push (nth i row-labels) row)
                   (push "|" row)
                   (dotimes (j ncols)
                     (let ((element (aref table i j))
                           (column-count (gethash j column-counts)))
                       (push (format nil "~A" element) row)
                       (incf row-count element)
                       (if (null column-count) 
                           (setf (gethash j column-counts) element)
                           (incf (gethash j column-counts) element))))
                   (push "|" row)
                   (push (format nil "~A" row-count) row)
                   (setf row-count 0)
                   (push (nreverse row) rows)))
               (let ((col-counts nil))
                 (dotimes (j ncols)
                   (push (gethash j column-counts) col-counts))
                 (setf col-counts (nreverse col-counts))
                 (push (append (list "|" "" "|") 
                               (mapcar #'(lambda (c) (format nil "~A" c)) 
                                       col-counts)
                               (list "|" (format nil "~A" 
                                                 (apply #'+ col-counts))))
                       rows))
               (nreverse rows)))
           (compute-sizes (rows)
             (apply #'mapcar #'(lambda (&rest column) 
                                 (apply #'max (mapcar #'(lambda (y) (length y)) 
                                                      column)))
                    rows))
           (format-row (row sizes) 
             (format stream "~{~VA ~}|~&"
                     (mapcan #'(lambda (r s) (list s r)) row sizes))))
    (let* ((rows (get-rows))
           (sizes (compute-sizes rows)))
      (row-sep sizes)
      (format-row (car rows) sizes)
      (row-sep sizes)
      (dolist (row (cdr rows))
        (when (string= (cadr row) "")
          (row-sep sizes))
        (format-row row sizes))
      (row-sep sizes)))))


;; Performance measures

(defun precision (results) 
  "Precision = tp / (tp + fp)."
  (let ((tp (segmentation-results-tp results))
        (fp (segmentation-results-fp results)))
    (if (and (zerop tp) (zerop fp)) 0 
        (float (/ tp (+ tp fp))))))

(defun recall (results) 
  "Recall = tp / (tp + fn)."
  (let ((tp (segmentation-results-tp results))
        (fn (segmentation-results-fn results)))
    (if (and (zerop tp) (zerop fn)) 0 
        (float (/ tp (+ tp fn))))))

(defun fallout (results) 
  "Fallout = fp / (fp + tn)."
  (let ((fp (segmentation-results-fp results))
        (tn (segmentation-results-tn results)))
    (float (/ fp (+ fp tn)))))

(defun f1 (results &optional (alpha 1))
  "F = ((a^2 + 1) * P * R) / (a^2 * p + r)"
  (let ((p (precision results))
        (r (recall results))
        (a^2 (expt alpha 2)))
    (if (or (= p 0) (= r 0)) 0
        (float (/ (* (1+ a^2) p r) (+ (* a^2 p) r))))))

(defun true-positives (predicted actual) 
  (flet ((true-positive (predicted actual)
           (if (and (= actual 1) (and predicted (= predicted 1)))
               1 
               0)))
    (apply #'+ (mapcar #'true-positive predicted actual))))

(defun true-negatives (predicted actual)
  (flet ((true-negative (predicted actual)
           (if (and (= actual 0) (and predicted (= predicted 0)))
               1 
               0)))
    (apply #'+ (mapcar #'true-negative predicted actual))))

(defun false-positives (predicted actual) 
  (flet ((false-positive (predicted actual) 
           (if (and (= actual 0) (not (= predicted 0)))
               1 
               0)))
    (apply #'+ (mapcar #'false-positive predicted actual))))

(defun false-negatives (predicted actual) 
  (flet ((false-negative (predicted actual) 
           (if (and (= actual 1) (not (= predicted 1)))
               1 
               0)))
    (apply #'+ (mapcar #'false-negative predicted actual))))
