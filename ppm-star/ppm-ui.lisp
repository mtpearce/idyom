;;;; ======================================================================
;;;; File:       ppm-ui.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2002-07-02 18:54:17 marcusp>                           
;;;; Time-stamp: <2014-06-04 16:05:23 marcusp>                           
;;;; ======================================================================
;;;;

;; PPM* Statistical Model: Example Usage
;;
;; CL-USER> (ppm-star::test-model '((a b r a c a d a b r a)) '(a b c d r) 3)
;; (((D A B) 1) ((C A D) 1) ((R A C) 1) ((B R A) 2) ((A D A) 1) ((A C A) 1)
;;  ((A B R) 2))
;;
;; CL-USER> (ppm-star::test-model '((l e t l e t t e r t e l e)) '(e l t r) 1)
;; (((R) 1) ((T) 4) ((E) 5) ((L) 3))
;;
;; CL-USER> (ppm-star::test-model '((a g c g a c g a g)) '(a c g) 2)
;; (((C G) 2) ((G A) 2) ((G C) 1) ((A C) 1) ((A G) 2))
;; 
;; CL-USER> (ppm-star::test-model '((m i s s i s s i p p i)) '(i m p s) 4)
;; (((S I P P) 1) ((S I S S) 1) ((S S I P) 1) ((S S I S) 1) ((I P P I) 1)
;;  ((I S S I) 3) ((M I S S) 1))
;; 
;; CL-USER> (ppm-star::test-model '((a s s a n i s s i m a s s a)) '(a i m n s) 2)
;; (((M A) 1) ((I M) 1) ((I S) 1) ((N I) 1) ((S I) 1) ((S A) 2) ((S S) 3)
;;  ((A N) 1) ((A S) 2))
;; 
;; CL-USER> (ppm-star::test-model '((a s s a n i s s i m a s s a)
;;                                  (m i s s i s s i p p i))
;;                                '(a s n i m p)
;;                                2)
;; (((P I) 1) ((P P) 1) ((M I) 1) ((M A) 1) ((I P) 1) ((I M) 1) ((I S) 3)
;;  ((N I) 1) ((S I) 3) ((S A) 2) ((S S) 5) ((A N) 1) ((A S) 2))
;;
;; CL-USER> (ppm-star::test-model '((a b r a c a d a b r a)
;;                                  (l e t l e t t e r t e l e)
;;                                  (a s s a n i s s i m a s s a)
;;                                  (m i s s i s s i p p i)
;;                                  (w o o l o o b o o l o o))
;;                                '(a b c d e i l m n o p r s t w)
;;                                3)
;; (((O B O) 1) ((O L O) 2) ((O O B) 1) ((O O L) 2) ((W O O) 1) ((P P I) 1)
;;  ((M I S) 1) ((M A S) 1) ((I P P) 1) ((I M A) 1) ((I S S) 3) ((N I S) 1)
;;  ((S I P) 1) ((S I S) 1) ((S I M) 1) ((S A N) 1) ((S S I) 3) ((S S A) 2)
;;  ((T E L) 1) ((T E R) 1) ((T T E) 1) ((T L E) 1) ((E L E) 1) ((E R T) 1)
;;  ((E T T) 1) ((E T L) 1) ((L O O) 2) ((L E T) 2) ((D A B) 1) ((C A D) 1)
;;  ((R T E) 1) ((R A C) 1) ((B O O) 1) ((B R A) 2) ((A N I) 1) ((A S S) 2)
;;  ((A D A) 1) ((A C A) 1) ((A B R) 2))
;;
(in-package :ppm)

(defun get-alphabet-hash-table (sequence &key (seed (make-hash-table)))
 (sequence::dosequence (item sequence seed)
   do (cond
        ((typep item 'sequence)
         (setf seed (get-alphabet-hash-table item :seed seed)))
        ((not (gethash item seed))
         (setf (gethash item seed) T)))))

(defun get-ppm-alphabet (sequence)
 (let ((alphabet))
   (maphash #'(lambda (key val)
                (declare (ignore val))
                (push key alphabet))
            (get-alphabet-hash-table sequence))
   alphabet))

(defun test-model (sequences alphabet order)
  (ngram-frequencies (build-model sequences alphabet) order))

(defun build-model (sequences alphabet) 
  (let ((model (ppm:make-ppm alphabet :escape :c :mixtures t 
                             :update-exclusion nil :order-bound nil)))
    (ppm:model-dataset model sequences :construct? t :predict? nil)
    model))

(defun ngram-frequencies (model order)
  (let ((results '()))
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
      (g-n-f (root-node) nil))
    (remove-if #'(lambda (x) 
                   (find ppm-star::*sentinel* (car x)))
               results)))

(defun generate-labels (model node prefix)
  (let ((label (label model node))
        (results '()))
    (dotimes (i (length label) (nreverse results))
      (push (append prefix (subseq label 0 (1+ i))) results))))

(defun root-node () ppm::*root*)
(defun label (model node) 
  (ppm::instantiate-label model (ppm::get-label model node)))
(defun frequency (model node) (ppm::get-count model node))

;; (defun print-model (model filename)
;;   (ppm::write-model-to-postscript model :filename filename))
