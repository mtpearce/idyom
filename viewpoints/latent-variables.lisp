(cl:in-package #:viewpoints)

(defparameter *hidden-state* ())

(defmacro with-hidden-state (hidden-state &body body)
  "<hidden-state> must be a pair-list"
  `(let* (; Create a local copy of the hidden state
	  (updated-hidden-state (copy-list *hidden-state*))
	  (properties (loop for i in (utils:generate-integers 0 (length ,hidden-state))
			 if (evenp i) collect (elt ,hidden-state i))))
     (loop for property in properties do (setf (getf updated-hidden-state property)
					       (getf, hidden-state property)))
     (let (;; Shadow the global hidden-state with the local copy
	   (*hidden-state* updated-hidden-state))
       ,@body)))

(defun get-hidden-state-parameter (param)
  (getf *hidden-state* param))


						  
