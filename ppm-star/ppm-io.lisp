;;;; ======================================================================
;;;; File:       ppm-io.lisp
;;;; Author:     Marcus  Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-16 16:59:20 marcusp>
;;;; Time-stamp: <2019-04-29 13:18:34 marcusp>
;;;; ======================================================================

(cl:in-package :ppm)

(defmethod write-model-to-postscript ((m ppm) filename &key (counts '(0 1)) (depth nil))
  "Prints the suffix tree of <m> to a postscript file <path>."
  (labels ((get-node-count (node)
             (let ((node-count '()))
               (when (member 1 counts :test #'=)
                 (push (utils:string-append "UX count: "
                                            (format nil "~D" (get-count m node t)))
                       node-count))
                              (when (member 0 counts :test #'=)
                 (push (utils:string-append "Count: "
                                            (format nil "~D" (get-count m node nil)))
                       node-count))
               node-count))
           (list-node-children (node)
             (when (or (null depth) 
                       (and (branch-record-p (get-record m node))
                            (<= (branch-record-depth (get-record m node)) 
                                depth)))
               (list-children m node)))
           (label->string (node)
             (let ((label (instantiate-label m (get-label m node))))
               (cons (utils:list->string label) (get-node-count node)))))
    (let ((psgraph:*fontsize* 14)
          (psgraph:*second-fontsize* 12)
          (psgraph:*boxradius* 10) 
          (psgraph:*boxedge* 10)
          (psgraph:*boxgray* "0")
          (psgraph:*edgegray* "0")
          (psgraph:*extra-x-spacing* 90)
          (psgraph:*extra-y-spacing* 20))
      (with-open-file (*standard-output* filename :direction :output
                                         :if-exists :supersede)
        (psgraph:psgraph *standard-output* (ppm:get-root)
                          #'list-node-children
                          #'label->string 
                          t nil #'eq nil)))))

(defun get-model (filename alphabet dataset &key (order-bound nil)
                           (mixtures t) (escape :c) (update-exclusion nil))
  "Returns a PPM model initialised with the supplied parameters. If
   <filename> exists the model is read from the designated file otherwise
   it is constructed from the database and written to <filename>." 
  (unless (utils:file-exists filename)
    (let ((model (make-ppm alphabet)))
      (model-dataset model dataset :construct? t :predict? nil)
      (write-model-to-file model filename)
      (format t "~&Written PPM* model to ~A.~%" filename)))
  (read-model-from-file filename :order-bound order-bound :mixtures mixtures
                        :escape escape :update-exclusion update-exclusion))

(defun read-model-from-file (filename &key (order-bound nil)
                                      (mixtures t) (escape :c)
                                      (update-exclusion nil))
  "Returns the suffix tree stored in <filename>."
  (let* ((model (utils:read-object-from-file filename :ppm))
         (leaves (utils:alist->hash-table (nth 1 (assoc 'leaves model))))
         (branches (utils:alist->hash-table (nth 1 (assoc 'branches model))))
         (dataset (alist->dataset (nth 1 (assoc 'dataset model))))
         (alphabet (nth 1 (assoc 'alphabet model))))
    (make-ppm alphabet :leaves leaves :branches branches :dataset dataset
              :order-bound order-bound :mixtures mixtures :escape escape
              :update-exclusion update-exclusion)))
              

(defmethod write-model-to-file ((m ppm) filename)
  "Writes the suffix tree of <m> to <filename>."
  (let* ((leaves (utils:hash-table->alist (ppm-leaves m)))
         (branches (utils:hash-table->alist (ppm-branches m)))
         (dataset (dataset->alist m))
         (alphabet (ppm-alphabet m))
         (model (list (list 'leaves leaves)
                      (list 'branches branches)
                      (list 'dataset dataset)
                      (list 'alphabet alphabet))))
    (utils:write-object-to-file model filename :ppm)))

(defmethod dataset->alist ((m ppm))
  "Returns (ppm-dataset <m>) in the form of an alist."
  (mapcar #'(lambda (item)
              (list (car item)
                    (utils:hash-table->alist (nth 1 item))))
          (utils:hash-table->alist (ppm-dataset m))))

(defun alist->dataset (alist)
  "Returns a hash-table corresponding to <alist> which is suitable for
   storing the dataset of a PPM model."
  (utils:alist->hash-table (mapcar #'(lambda (item)
                                 (list (car item)
                                       (utils:alist->hash-table (nth 1 item))))
                             alist)))

