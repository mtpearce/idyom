(cl:in-package #:latent-variables)

(defparameter *latent-state* ())
(defconstant +default-state+ 'default-state)

(defun merge-parameters (params-a params-b)
  (loop for param-a in params-a collect
       (loop for param-b in params-b collect
	    (append param-a param-b))))

(defun joint-distribution (dist-a dist-b)
  (let ((joint-distribution))
    (dolist (param-a dist-a)
      (dolist (param-b dist-b)
	(push (* dist-a dist-b) joint-distribution)))
    joint-distribution))

(defun get-latent-variable (attribute)
    (if (atom attribute)
	(make-instance 
	 (find-symbol (symbol-name attribute) (find-package :latent-variables)))
	(let ((variables (mapcar #'get-latent-variable attribute)))
	  (make-instance 'linked
			 :links variables
			 :interpretation-parameters
			 (reduce #'union (mapcar #'interpretation-parameters variables))
			 :category-parameters
			 (reduce #'union (mapcar #'category-parameters variables))))))

(defun get-latent-category (variable)
  (let ((parameters (category-parameters variable)))
    (mapcar #'(lambda (p) (getf *latent-state* p)) parameters)))

(defun get-latent-interpretation (variable)
  (let ((parameters (interpretation-parameters variable)))
    (mapcar #'(lambda (p) (getf *latent-state* p)) parameters)))

