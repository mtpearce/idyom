(in-package :ppm)

(defun get-alphabet-hash-table (sequence &key (seed (make-hash-table)))
 (sequence::dosequence (item sequence seed)
   do (cond
        ((typep item 'sequence)
         (setf seed (get-alphabet-hash-table item :seed seed)))
        ((not (gethash item seed))
         (setf (gethash item seed) T)))))

(defun get-alphabet (sequence)
 (let ((alphabet))
   (maphash #'(lambda (key val)
                (declare (ignore val))
                (push key alphabet))
            (get-alphabet-hash-table sequence))
   alphabet))

(defun test-model (order)
  (ngram-frequencies 
   (build-model '((a b r a c a d a b r a)) '(a b c d r)) 
   order))

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
  results))

(defun generate-labels (model node prefix)
  (let ((label (label model node))
        (results '()))
    (dotimes (i (length label) (nreverse results))
      (push (append prefix (subseq label 0 (1+ i))) results))))

(defun root-node () ppm::*root*)
(defun list-children (model node) (ppm::list-children model node))
(defun label (model node) 
  (ppm::instantiate-label model (ppm::get-label model node)))
#+nil (defun length-of-label (model node)
  ;; Silly name, but clashes with struct accessor otherwise
  (ppm::get-length model (ppm::get-label model node)))
(defun frequency (model node) (ppm::get-count model node))

;; (defun print-model (model filename)
;;   (ppm::write-model-to-postscript model :filename filename))
