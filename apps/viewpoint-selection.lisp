;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-             
;;;; ======================================================================
;;;; File:       viewpoint-selection.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2003-10-02 18:54:17 marcusp>                           
;;;; Time-stamp: <2005-11-27 16:55:43 marcusp>                           
;;;; ======================================================================

(cl:in-package #:viewpoint-selection)

;;;========================================================================
;;; Caching of record weights 
;;;========================================================================

(defparameter *vs-cache* '())
(defparameter *vs-cache-dir* mvs:*ep-cache-dir*)

(defun vs-cache-path (filename) (utils:string-append *vs-cache-dir* filename))

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

(defun better-than-asc-0 (r1 r2) 
  (better-than-asc r1 r2 :accessor #'car))

(defun better-than-asc (r1 r2 &key (accessor #'identity))
  (let ((w1 (unless (null r1) (funcall accessor (record-weight r1))))
        (w2 (unless (null r2) (funcall accessor (record-weight r2)))))
    (cond ((and (null w1) (null w2))
           '())
          ((null w1) 
           '())
          ((null w2)
           t)
          (t (> w1 w2)))))

(defun better-than-desc-1 (r1 r2) 
  (better-than-desc r1 r2 :accessor #'cadr))

(defun better-than-desc (r1 r2 &key (accessor #'identity))
  (let ((w1 (unless (null r1) (funcall accessor (record-weight r1))))
        (w2 (unless (null r2) (funcall accessor (record-weight r2)))))
    (cond ((and (null w1) (null w2))
           '())
          ((null w1) 
           '())
          ((null w2)
           t)
          (t (< w1 w2)))))

;;;========================================================================
;;; Best first search 
;;;========================================================================
 
(defun run-best-first (features start-state eval-function better-than)
  (print-header) 
  (let* ((eval-function (caching-eval-function eval-function))
         (start-weight (unless (null start-state) 
                         (funcall eval-function start-state)))
         (start-record (make-record :state start-state :weight start-weight))
         (better-than (if (eql better-than :asc) 
                          #'better-than-asc
                          #'better-than-desc))
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

(defun run-hill-climber (features start-state eval-function better-than)
  (print-header) 
  (let* ((eval-function (caching-eval-function eval-function))
         (start-weight (unless (null start-state) 
                         (funcall eval-function start-state)))
         (start-record (make-record :state start-state :weight start-weight))
         (unused (remove-if #'(lambda (x) 
                                (member x start-state 
                                        :test #'viewpoints:attribute-equal))
                            features))
         (better-than (case better-than 
                        (:asc #'better-than-asc)
                        (:desc #'better-than-desc)
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
    (print-record current-record)
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
  (format t "~%   System ~1,55T Score")
  (print-separator))

(defun print-separator ()
  (format t "~% ~A" (make-sequence 'string 77 :initial-element #\-)))

(defun print-double-separator ()
  (format t "~% ~A" (make-sequence 'string 77 :initial-element #\=)))

(defun print-record (record)
  (format t "~%   ~A ~1,55T ~A" (record-state record) (record-weight record)))
