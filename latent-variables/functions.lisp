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

(defun get-latent-state-value (parameter)
  (getf *latent-state* parameter))

(defun get-latent-category (variable)
  (let ((parameters (category-parameters variable)))
    (mapcar #'get-latent-state-value parameters)))

(defun get-latent-interpretation (variable)
  (let ((parameters (interpretation-parameters variable)))
    (mapcar #'get-latent-state-value parameters)))

(defun partition-dataset (dataset latent-variable &optional categories partitioned-dataset)
  "Collect subsequences of compositions in <dataset> into an ALIST whose CARs 
correspond to unique categories and whose CDRs correspond to lists of all
 subsequences of that category in <dataset>, where a subsequence is defined 
as the longest lists of subsequent events with the same event category found 
with the get-event-category method of <latent-variable>."
  (let* ((composition (car dataset))
	 (remaining-compositions (cdr dataset))
	 (partitioned-dataset (partition-composition partitioned-dataset
						     (coerce composition 'list)
						     latent-variable)))
    (if (null remaining-compositions)
	partitioned-dataset
	(partition-dataset remaining-compositions latent-variable
			   categories partitioned-dataset))))

(defun partition-composition (partitioned-dataset composition latent-variable
			    &optional subsequence category)
  (let* ((event (car composition))
	 (remaining-events (cdr composition))
	 (category (if (null category) (lv:get-event-category event latent-variable)
		       category)))
    (if (and (equal (lv:get-event-category event latent-variable)
		    category)
	     (not (null remaining-events)))
	;; While category does not change, accumulate events into subsequence
	(partition-composition partitioned-dataset remaining-events
			     latent-variable (cons event subsequence) category)
	(let* ((item (assoc category partitioned-dataset :test #'equal))
	       (result (reverse (cons event subsequence))))
	  (if (null item)
	      (setf partitioned-dataset
		    (acons category (list result) partitioned-dataset))
	      (push result (cdr item)))
	  (if (null remaining-events)
	      partitioned-dataset
	      (partition-composition partitioned-dataset remaining-events
				     latent-variable))))))

