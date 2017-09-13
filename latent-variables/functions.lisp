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

(defun get-latent-category (variable)
  (let ((parameters (category-parameters variable)))
    (mapcar #'(lambda (p) (getf *latent-state* p)) parameters)))

(defun get-latent-interpretation (variable)
  (let ((parameters (interpretation-parameters variable)))
    (mapcar #'(lambda (p) (getf *latent-state* p)) parameters)))

