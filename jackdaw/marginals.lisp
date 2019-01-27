(cl:in-package #:marginals)

(defun make ()
  (list nil (make-hash-table :test #'equal)))

(defun parameters (marginal)
  (car marginal))

(defun probability (marginal parameter)
  (gethash parameter (second marginal)))

(defun update (marginal parameter parameters probability
			&optional prefix)
  (let ((marginal-parameter
	 (append prefix
		 (mapcar (lambda (s) (gethash s parameter)) parameters)))
	(table (cadr marginal)))
    (multiple-value-bind (p param-set?)
	(gethash marginal-parameter table)
      (unless param-set?
	(setf (car marginal)
	      (cons marginal-parameter (car marginal))))
      (let ((marginal-p (if param-set?
			    (probabilities:add probability p)
			    probability)))
	(setf (gethash marginal-parameter table) marginal-p)
	(values marginal-parameter marginal-p)))))
