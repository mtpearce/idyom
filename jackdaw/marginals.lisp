(cl:in-package #:marginals)

(defun make ()
  (make-hash-table :test #'equal))

(defun parameters (marginal)
  (loop for param being the hash-keys of marginal collect param))

(defun probability (marginal parameter)
  (gethash parameter marginal))

(defun marginal-parameter (parameter parameters &optional prefix)
  (append prefix (mapcar (lambda (s) (gethash s parameter)) parameters)))

(defun update (marginal parameter parameters probability
			&optional prefix)
  (let ((marginal-parameter (marginal-parameter parameter parameters prefix)))
    (multiple-value-bind (p exists?)
	(gethash marginal-parameter marginal)
      (let ((marginal-p (if exists?
			    (probabilities:add probability p)
			    probability)))
	(setf (gethash marginal-parameter marginal) marginal-p)
	(values marginal-parameter exists? marginal-p)))))
