(cl:in-package #:jackdaw)

(defparameter *log-level* 0)

(defmacro get-or-create-and-set (key hashtable default)
  "Default is not evaluated if KEY is found in HASHTABLE."
  `(multiple-value-bind (value hit?)
       (gethash ,key ,hashtable)
     (if (not hit?)
	 (values (setf (gethash ,key ,hashtable) ,default) hit?)
	 (values value hit?))))

;; https://stackoverflow.com/questions/26045442/copy-hash-table-in-lisp
(defun copy-hash-table (hash-table)
  (let ((ht (make-hash-table 
             :test (hash-table-test hash-table)
             :rehash-size (hash-table-rehash-size hash-table)
             :rehash-threshold (hash-table-rehash-threshold hash-table)
             :size (hash-table-size hash-table))))
    (loop for key being each hash-key of hash-table
       using (hash-value value)
       do (setf (gethash key ht) value)
       finally (return ht))))

(defun logm (msg args &optional level)
  (when (>= *log-level* level) (apply #'format (cons t (cons msg args))) (format t "~%")))

(defun infom (msg &rest args)
  (apply #'logm (list msg args 1)))

(defun debugm (msg &rest args)
  (apply #'logm (list msg args 2)))

(defun symbol-index-map (symbols &key (test #'eq))
  (let ((map (make-hash-table :test test)))
    (loop for s in symbols for i below (length symbols) do
	 (setf (gethash s map) i))
    map))

(defun dictionary (keys values &key (test #'eql))
  (let ((dict (make-hash-table :test test)))
    (loop for key in keys for value in values do
	 (setf (gethash key dict) value))
    dict))
    
