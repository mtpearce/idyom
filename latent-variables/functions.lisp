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
  (get-latent-state-value (%category-set-symbol variable)))

(defun latent-interpretation-set-p (variable)
  (get-latent-state-value (%interpretation-set-symbol variable)))

(defun get-latent-state-value (attribute)
  (getf *latent-state* attribute))

(defun get-latent-category (variable)
  (let ((attributes (category-attributes variable)))
    (mapcar #'get-latent-state-value attributes)))

(defun get-latent-interpretation (variable)
  (let ((attributes (interpretation-attributes variable)))
    (mapcar #'get-latent-state-value attributes)))
  
(defun get-category-subsets (dataset latent-variable)
  "Collect subsequences of compositions in <dataset> into an ALIST whose CARs 
correspond to unique categories and whose CDRs correspond to lists of all
 subsequences of that category in <dataset>, where a subsequence is defined 
as the longest lists of subsequent events with the same event category found 
with the get-event-category method of <latent-variable>."
  (let ((category-subsets))
    (dolist (music-sequence dataset category-subsets)
      (let* ((category (get-event-category (elt music-sequence 0) latent-variable))
	     (subset (assoc category category-subsets :test #'equal)))
	(if (null subset)
	    (setf category-subsets (acons category (list music-sequence) category-subsets))
	    (push music-sequence (cdr subset)))))))
