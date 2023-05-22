;;;; ======================================================================
;;;; File:       ppm-ui.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2002-07-02 18:54:17 marcusp>                           
;;;; Time-stamp: <2023-05-22 14:15:47 marcusp>                           
;;;; ======================================================================
;;;;
;;;; 
;;;; DESCRIPTION 
;;;;
;;;; Direct user interface to the PPM* code. 
;;;; 
;;;; CL-USER> (ppm-predict '((l e t l e t t e r t e l e)) :order-bound 1)
;;;; (((R) 1) ((T) 4) ((E) 5) ((L) 3))
;;;;
;;;; CL-USER> (ppm-predict '((a g c g a c g a g)) :order-bound 2)
;;;; (((C G) 2) ((G A) 2) ((G C) 1) ((A C) 1) ((A G) 2))
;;;;
;;;; CL-USER> (ppm-predict '((a b r a c a d a b r a)) :order-bound 3)
;;;; (((D A B) 1) ((C A D) 1) ((R A C) 1) ((B R A) 2) ((A D A) 1) ((A C A) 1)
;;;;  ((A B R) 2))
;;;;
;;;; CL-USER> (ppm-predict '((m i s s i s s i p p i)) :order-bound 4)
;;;; (((S I P P) 1) ((S I S S) 1) ((S S I P) 1) ((S S I S) 1) ((I P P I) 1)
;;;;  ((I S S I) 3) ((M I S S) 1))
;;;; 
;;;; CL-USER> (ppm-predict '((a s s a n i s s i m a s s a)
;;;;                         (m i s s i s s i p p i))
;;;;                       :order-bound 2)
;;;; (((P I) 1) ((P P) 1) ((M I) 1) ((M A) 1) ((I P) 1) ((I M) 1) ((I S) 3)
;;;;  ((N I) 1) ((S I) 3) ((S A) 2) ((S S) 5) ((A N) 1) ((A S) 2))

(in-package :ppm)

;;; Predict sequences using a PPM model

(defun ppm-predict (sequences &key (alphabet nil)
                                (exclusion t) (escape :c) (mixtures t) (update-exclusion nil) (order-bound nil)
                                write ps (counts '(0 1)) depth (sentinel? t)
                                (detail 4))
  "Build a PPM* model with the specified parameters and predict the
  supplied sequences. The <alphabet> is computed unless specified. If
  <write> is non-null, the result is written to the console. When <ps>
  specifies a filename, a postscript representation of the suffix tree
  will be written to the file specified with the <counts> and <depth>
  parameters passed to WRITE-MODEL-TO-POSTSCRIPT. <detail> controls
  the level of detail printed: 1 = overall mean IC; 2 = mean IC for
  each sequence; 3 = IC for each note; 4 = full distributions for each
  event."
  (let* ((alphabet (if alphabet alphabet (get-alphabet sequences)))
         (model (make-instance 'ppm :alphabet alphabet :exclusion exclusion
                               :escape escape :mixtures mixtures
                               :update-exclusion update-exclusion :order-bound order-bound))
         (result (ppm:model-dataset model sequences :construct? t :predict? t :sentinel? sentinel?)))
    (prog1 (get-result result detail)
      (when write (write result :right-margin 110))
      (when ps (ppm:write-model-to-postscript model ps :counts counts :depth depth)))))

(defun get-alphabet (sequences)
  (flet ((sp (x y)
           (cond ((and (numberp x) (numberp y))
                  (<= x y))
                 ((and (symbolp x) (symbolp y))
                  (string<= x y))
                 ((and (stringp x) (stringp y))
                  (string<= x y))
                 ((and (characterp x) (characterp y))
                  (char<= x y)))))
    (sort (remove-duplicates
           (reduce #'append (mapcar #'(lambda (x) (remove-duplicates x :test #'equal)) sequences))
           :test #'equal)
          #'sp)))

(defun get-result (data detail)
  (if (= detail 4)
      data
      (let ((data-3
             (mapcar #'(lambda (x)
                         (mapcar #'(lambda (y)
                                     (codelength (get-probability (car y) (cadr y))))
                                 (cdr x)))
                     data)))
        (if (= detail 3)
            data-3
            (let ((data-2 (mapcar #'(lambda (x) (apply #'utils:average x)) data-3)))
              (if (= detail 2)
                  data-2
                  (apply #'utils:average data-2)))))))


;;; Train a PPM model on a set of sequences

(defun build-model (sequences alphabet)
  "Train a PPM* model on <sequences> composed from the specified <alphabet>."
  (let ((model (make-instance 'ppm :alphabet alphabet :escape :c :mixtures t
                              :update-exclusion nil :order-bound nil)))
    (ppm:model-dataset model sequences :construct? t :predict? nil)
    model))


;;; Print n-gram frequences for a given order

(defun ngram-frequencies (sequences alphabet order)
  (let ((results '())
        (model (build-model sequences alphabet)))
    (labels ((g-n-f (node prefix) 
               (let ((nlabels (generate-labels model node prefix))
                     (frequency (frequency model node)))
                 (dolist (nlabel nlabels)
                   (when (= (length nlabel) order)
                     (push (list nlabel frequency) results)))
                 (when (< (length prefix) order)
                   (let ((new-prefix (append prefix (label model node))))
                     (mapc #'(lambda (c) (g-n-f c new-prefix))
                           (list-children model node)))))))
      (g-n-f (ppm:get-root) nil))
    (remove-if #'(lambda (x) 
                   (find *sentinel* (car x)))
               results)))

(defun generate-labels (model node prefix)
  (let ((label (label model node))
        (results '()))
    (dotimes (i (length label) (nreverse results))
      (push (append prefix (subseq label 0 (1+ i))) results))))

(defun label (model node) 
  (instantiate-label model (get-label model node)))

(defun frequency (model node) (get-count model node nil))

