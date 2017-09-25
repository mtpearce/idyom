(cl:in-package #:latent-variables)

(defparameter *latent-state* ())

(defun get-latent-variables (attributes)
  (mapcar #'get-latent-variable attributes))

(defun get-latent-variable (attribute)
    (if (atom attribute)
	(make-instance 
	 (find-symbol (symbol-name attribute) (find-package :latent-variables)))
	(let* ((links (mapcar #'get-latent-variable attribute))
	       (links (stable-sort links #'(lambda (x y)
					     (string< (latent-variable-name x)
						      (latent-variable-name y))))))
	    (make-instance 'linked
			   :links links))))

(defun %interpretation-set-symbol (variable)
  (intern (string-upcase (format nil "~A-interpretation-set-p" (latent-variable-name variable)))
	  (find-package 'keyword)))

(defun %category-set-symbol (variable)
  (intern (string-upcase (format nil "~A-category-set-p" (latent-variable-name variable)))
	  (find-package 'keyword)))

(defun latent-category-set-p (variable)
  (get-latent-state-value (%variable-set-symbol variable "category")))

(defun latent-interpretation-set-p (variable)
  (get-latent-state-value (%variable-set-symbol variable "interpretation")))

(defun get-latent-state-value (parameter)
  (getf *latent-state* parameter))

(defun get-latent-category (variable)
  (let ((parameters (category-parameters variable)))
    (mapcar #'get-latent-state-value parameters)))

(defun get-latent-interpretation (variable)
  (let ((parameters (interpretation-parameters variable)))
    (mapcar #'get-latent-state-value parameters)))

