;;;; ======================================================================
;;;; File:       segmentation.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2008-03-13 13:07:12 marcusp>
;;;; Time-stamp: <2020-02-06 15:35:02 marcusp>
;;;; ======================================================================

(cl:defpackage #:segmentation
  (:use #:cl)
  (:export #:*boundary* #:*no-boundary* #:peak-picker
           #:make-segmentation-results #:print-segmentation-results
           #:precision #:recall #:f1 #:fallout #:test-segmentation
           #:notated-segmentation #:notated-segmentations
           #:idyom-segmentation))

(cl:in-package #:segmentation) 

(defparameter *boundary* 1
  "An object used to represent a point where a boundary occurs.")

(defparameter *no-boundary* 0
  "An object used to represent a point where a boundary doesn't occur.")

;;; Top-level

(defun idyom-segmentation (dataset-id target-viewpoints source-viewpoints
                           &key (k 1.28) (window-size nil)
                             (mean #'linearly-weighted-arithmetic-mean))
  (multiple-value-bind (d1 d2 d3)
      (idyom:idyom dataset-id target-viewpoints source-viewpoints :models :both+)
    (declare (ignore d1 d2))
    (let ((predicted (mapcar #'(lambda (x) (segmentation:peak-picker x :k k :window-size window-size :mean mean)) d3))
          (actual (segmentation::ground-truth dataset-id)))
      (segmentation:test-segmentation (reduce #'append predicted) (reduce #'append actual)))))

  
;;; Peak picking

(defun peak-picker (boundary-strengths &key (k 1.28) (window-size nil)
                                         (mean #'linearly-weighted-arithmetic-mean))
  (let* ((result (list *no-boundary* *no-boundary*))
         (length (length boundary-strengths))
         (m 0))
    (do* ((bs boundary-strengths (cdr bs)) 
          (n length (1- n))
          (bsn-1 (first bs) (first bs))
          (bsn (second bs) (second bs))
          (bsn+1 (third bs) (third bs))
          (bs- (list bsn-1) (cons bsn-1 bs-)))
         ((< n 3) (nreverse (cons *no-boundary* result)))
      ;;(format t "~&n = ~A; bsn-1 = ~A; bsn = ~A; bsn+1 = ~A; bs- = ~A~%"
      ;;        n bsn-1 bsn bsn+1 bs-)
      (incf m)
      (when (> m 1) ; first two notes are not boundary candidates
        (let ((window (typecase window-size
                        (number (first-n bs- window-size))
                        (t bs-))))
          (if (and (>= bsn bsn+1)
                   (> bsn bsn-1)
                   (> bsn (+ (* k (standard-deviation window))
                             (funcall mean window))))
              (push *boundary* result)
              (push *no-boundary* result)))))))

(defun first-n (list n)
  (if (<= (length list) n)
      list
      (subseq list 0 n)))

(defun arithmetic-mean (numbers)
  (/ (apply #'+ numbers) (length numbers)))

(defun weighted-arithmetic-mean (numbers weights)
  (/ (apply #'+ (mapcar #'* numbers weights))
     (apply #'+ weights)))

(defun linearly-weighted-arithmetic-mean (numbers)
  (let ((weights nil))
    (do ((n (length numbers) (1- n))
         (k 1 (1+ k)))
        ((zerop n))
      (push k weights))
    ;(print (list numbers weights))
    (weighted-arithmetic-mean numbers weights)))

(defun exponentially-weighted-arithmetic-mean (numbers &key (alpha 0.5))
  (let ((weights nil))
    (do ((n (length numbers) (1- n)))
        ((zerop n))
      (push (expt (- 1 alpha) (1- n)) weights))
    ;(print (list numbers weights))
    (weighted-arithmetic-mean numbers weights)))

(defun logarithmically-weighted-arithmetic-mean (numbers &key (alpha 0.5))
  (let ((weights nil))
    (do ((n (1+ (length numbers)) (1- n)))
        ((= 1 n))
      (push (abs (log (- 1 alpha) n)) weights))
    ;(print (list numbers weights))
    (weighted-arithmetic-mean numbers weights)))

(defun standard-deviation (numbers)
  (let ((xbar (arithmetic-mean numbers))
        (n (length numbers)))
    (sqrt (* (/ 1 (1- n))
             (reduce #'+ (mapc #'(lambda (x) (expt (- x xbar) 2)) numbers))))))


;;;; Ground truth for a given dataset

(defun ground-truth (dataset-id)
  (let* ((phrase (viewpoints:viewpoint-sequences (viewpoints:get-viewpoint 'phrase) (md:get-music-objects dataset-id nil)))
         (phrase (mapcar #'(lambda (x) (cons 0 (cdr x))) phrase)))
    (mapcar #'(lambda (x)
                (substitute 0 -1 x))
            phrase)))


;;;; Evaluation

(defun test-segmentation (predicted actual)
  (let* ((result (make-segmentation-results predicted actual))
         (p (precision result))
         (r (recall result))
         (f (f1 result)))
    (print-segmentation-results result t)
    (format t "~&Precision = ~,2F; Recall = ~,2F; F_1 = ~,2F~%" p r f)))

(defun notated-segmentations (sequences)
  (mapcar #'notated-segmentation sequences))

(defun notated-segmentation (sequence)
  (viewpoints:viewpoint-sequence (viewpoints:get-viewpoint 'liph) sequence))

;;; structure to store/print results

(defstruct (segmentation-results (:constructor %make-segmentation-results)
                                 (:print-object print-segmentation-results))
  (tp 0)
  (tn 0) 
  (fp 0)
  (fn 0))

(defun make-segmentation-results (generated actual)
  (%make-segmentation-results :tp (true-positives generated actual)
                              :tn (true-negatives generated actual)
                              :fp (false-positives generated actual)
                              :fn (false-negatives generated actual)))

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
    (float (/ tp (+ tp fp)))))

(defun recall (results) 
  "Recall = tp / (tp + fn)."
  (let ((tp (segmentation-results-tp results))
        (fn (segmentation-results-fn results)))
    (float (/ tp (+ tp fn)))))

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
    (float (/ (* (1+ a^2) p r) (+ (* a^2 p) r)))))

(defun true-positives (generated actual) 
  (flet ((true-positive (generated actual) 
           (if (and generated (= generated actual 1))
               1 
               0)))
    (apply #'+ (mapcar #'true-positive generated actual))))

(defun true-negatives (generated actual)
  (flet ((true-negative (generated actual) 
           (if (or (and (null generated) (= actual 0)) 
                   (and generated (= generated actual 0)))
               1 
               0)))
    (apply #'+ (mapcar #'true-negative generated actual))))

(defun false-positives (generated actual) 
  (flet ((false-positive (generated actual) 
           (if (and generated (= generated 1) (= actual 0))
               1 
               0)))
    (apply #'+ (mapcar #'false-positive generated actual))))

(defun false-negatives (generated actual) 
  (flet ((false-negative (generated actual) 
           (if (and (or (null generated) (= generated 0)) (= actual 1))
               1 
               0)))
    (apply #'+ (mapcar #'false-negative generated actual))))
