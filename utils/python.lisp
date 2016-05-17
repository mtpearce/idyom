(cl:in-package #:python)

(defun alist->dict (alist &key (file nil) (dict-name "d") 
			      (key-format-fn #'cl-primitive->python)
			      (value-format-fn #'cl-primitive->python))
  (let ((dict (format nil "~A = {~A}" dict-name
			(alist->dict-items alist key-format-fn value-format-fn))))
    (when file (with-open-file (stream file :direction :output :if-exists :supersede)
		 (format stream dict)))
    dict))

(defun alist->dict-items (alist key-format-fn value-format-fn)
  (let* ((item (pop alist))
	 (other-items 
	  (if alist 
	      (format nil ", ~A" (alist->dict-items alist key-format-fn value-format-fn))
	      ""))
	 (key (car item))
	 (value (cdr item)))
    (format nil "~A:~A~A" 
	    (funcall key-format-fn key) 
	    (funcall value-format-fn value)
	    other-items)))
			  

(defun alist-value-map (alist fn)
  (let* ((item (pop alist))
	 (key (car item))
	 (value (cdr item)))
    (acons (funcall fn key) value (alist-value-map alist fn))))

(defun cl-primitive->python (obj)
  (cond 
    ((typep obj 'integer) (format nil "~A" obj))
    ((or (typep obj 'float) (typep obj 'ratio)) (format nil "~F" obj))
    ((null obj) (format nil "null"))
    ((and (typep obj 'boolean) obj) (format nil "true"))
    ((or (typep obj 'symbol) (typep obj 'string)) (format nil "\"~A\"" obj))
    (t (format nil "\"~A\"" obj))))

(defun plist->dict (plist &key (key-format-fn #'cl-primitive->python) 
			      (value-format-fn #'cl-primitive->python))
  (format nil "{~{~{~A~^:~}~^, ~}}" (loop for (key value) on plist by #'cddr collect
					(list (apply key-format-fn (list key)) 
					      (apply value-format-fn (list value))))))

(defun list->list (list &key (item-format-fn #'cl-primitive->python))
  (format nil "[~{~D~^, ~}]" (mapcar item-format-fn list)))

