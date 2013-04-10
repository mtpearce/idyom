(cl:in-package :ppm)

(defmethod write-model-to-postscript ((m ppm) filename)
                                      ;(depth nil))
  "Prints the suffix tree of <m> to a postscript file <path>."
  (labels ((get-node-count (node)
             (list (string-append "Count0: "
                                  (format nil "~D" (get-count m node)))
                   (string-append "Count1: "
                                  (format nil "~D" (get-count m node t)))))
           (list-node-children (node)
             ;(when (or (null depth) 
             ;          (and (branch-record-p (get-record m node))
             ;               (<= (branch-record-depth (get-record m node)) 
             ;                   depth)))
               (list-children m node));)

           (list->string (list)
             (reduce #'(lambda (&optional s1 s2) (string-append s1 s2))
                     (mapcar #'(lambda (symbol)
                                 (format nil "~A " (if (symbolp symbol)
                                                       (symbol-name symbol)
                                                       symbol)))
                             list)))
           (label->string (node)
             (let ((label (instantiate-label m (get-label m node))))
               (cons (list->string label) (get-node-count node)))))
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
        (psgraph:psgraph *standard-output* *root*
                          #'list-node-children
                          #'label->string 
                          t nil #'eq nil)))))

(defun get-model (filename alphabet dataset &key (order-bound nil)
                           (mixtures t) (escape :c) (update-exclusion nil))
  "Returns a PPM model initialised with the supplied parameters. If
   <filename> exists the model is read from the designated file otherwise
   it is constructed from the database and written to <filename>." 
  (unless (file-exists filename)
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
  (let* ((model (read-object-from-file filename :ppm))
         (leaves (alist->hash-table (nth 1 (assoc 'leaves model))))
         (branches (alist->hash-table (nth 1 (assoc 'branches model))))
         (dataset (alist->dataset (nth 1 (assoc 'dataset model))))
         (alphabet (nth 1 (assoc 'alphabet model))))
    (make-ppm alphabet :leaves leaves :branches branches :dataset dataset
              :order-bound order-bound :mixtures mixtures :escape escape
              :update-exclusion update-exclusion)))
              

(defmethod write-model-to-file ((m ppm) filename)
  "Writes the suffix tree of <m> to <filename>."
  (let* ((leaves (hash-table->alist (ppm-leaves m)))
         (branches (hash-table->alist (ppm-branches m)))
         (dataset (dataset->alist m))
         (alphabet (ppm-alphabet m))
         (model (list (list 'leaves leaves)
                      (list 'branches branches)
                      (list 'dataset dataset)
                      (list 'alphabet alphabet))))
    (write-object-to-file model filename :ppm)))

(defmethod dataset->alist ((m ppm))
  "Returns (ppm-dataset <m>) in the form of an alist."
  (mapcar #'(lambda (item)
              (list (car item)
                    (hash-table->alist (nth 1 item))))
          (hash-table->alist (ppm-dataset m))))

(defun alist->dataset (alist)
  "Returns a hash-table corresponding to <alist> which is suitable for
   storing the dataset of a PPM model."
  (alist->hash-table (mapcar #'(lambda (item)
                                 (list (car item)
                                       (alist->hash-table (nth 1 item))))
                             alist)))

(defun write-object-to-file (object filename &optional (package :cl-user)
                             (fun #'prin1))
  (let ((gzipped-filename (add-file-suffix filename ".gz")))
    (if (probe-file gzipped-filename) (delete-file gzipped-filename))
    (with-open-file (s filename :direction :output :if-exists :overwrite
                       :if-does-not-exist :create)
      (with-standard-io-syntax
          (let ((*package* (find-package package)))
            (funcall fun object s))))
    (gzip filename)))
      
(defun read-object-from-file (filename &optional (package :cl-user))
  (let ((gzipped-filename (add-file-suffix filename ".gz")))
    (if (probe-file gzipped-filename)
        (read-object-from-gzipped-file gzipped-filename package)
        (if (probe-file filename) 
            (with-open-file (s filename :direction :input)
              (with-standard-io-syntax
                  (let ((*package* (find-package package)))
                    (read s nil))))
            (format t "~%~A does not exist." filename)))))

(defun file-exists (filename &key (suffix ".gz"))
  (or (probe-file filename)
      (probe-file (add-file-suffix filename suffix))))

(defun read-object-from-gzipped-file (filename &optional (package :cl-user))
  (let ((output-filename (remove-file-suffix filename)))
    (gunzip filename)
    (with-open-file (s output-filename :direction :input)
      (prog1
          (with-standard-io-syntax
              (let ((*package* (find-package package)))
                (read s nil)))
        (gzip output-filename)))))
       
(defun remove-file-suffix (filename &optional (suffix ".gz"))
  (reverse (subseq (reverse filename) (length suffix))))

(defun add-file-suffix (filename &optional (suffix ".gz"))
  (string-append filename suffix))

(defun gunzip (filename)
  #+(and x86-64 linux) (shell-command "/bin/gunzip" (list filename))
  #-(and x86-64 linux) (shell-command "/usr/bin/gunzip" (list filename)))

(defun gzip (filename)
  #+(and x86-64 linux) (shell-command "/bin/gzip" (list filename))
  #-(and x86-64 linux) (shell-command "/usr/bin/gzip" (list filename)))

(defun hash-table->alist (hashtab)
  (let ((alist '()))
    (maphash #'(lambda (key value) (setq alist (cons (list key value) alist)))
             hashtab)
    alist))

(defun alist->hash-table (alist &key (test #'eql))
  (let ((hashtable (make-hash-table :test test)))
    (mapc #'(lambda (x) (setf (gethash (car x) hashtable) (cadr x)))
          alist)
    hashtable))

(defun shell-command (command args) 
  #+cmu (ext:run-program command args)
  #+sbcl (sb-ext:run-program command args)
  #+allegro (excl:run-shell-command 
             (apply #'string-append command 
                    (mapcar #'(lambda (x) (format nil " ~A" x)) args)))
  ) 

(defun string-append (&rest args)
  "Concatenates its string arguments <args>."
  (apply #'concatenate 'string args))
