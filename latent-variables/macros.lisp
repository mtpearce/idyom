(cl:in-package #:latent-variables)

(defmacro with-latent-state (latent-state &body body)
  "<latent-state> must be a pair-list"
  `(let ((properties (loop for i below (length ,latent-state)
			if (evenp i) collect (elt ,latent-state i)))
	 ;; Create a local copy of the latent state
	 (updated-latent-state (copy-list *latent-state*)))
     (loop for property in properties do (setf (getf updated-latent-state property)
					       (getf, latent-state property)))
     ;; Shadow the global latent-state with the local copy
     (let ((*latent-state* updated-latent-state))
       ,@body)))

(defmacro with-latent-variable-state ((latent-state variable) &body body)
  (let ((latent-state `(utils:make-plist (latent-state-parameters ,variable)
					 ,latent-state)))
    `(with-latent-state ,latent-state ,@body)))

(defmacro with-latent-category ((category variable) &body body)
  (let ((latent-state `(utils:make-plist (category-parameters ,variable)
					 ,category)))
    `(with-latent-state ,latent-state ,@body)))

(defmacro with-latent-interpretation ((interpretation variable) &body body)
  (let ((latent-state `(utils:make-plist (interpretation-parameters ,variable)
					,interpretation)))
    `(with-latent-state ,latent-state ,@body)))

(defmacro define-latent-variable (name category-parameters interpretation-parameters)
  `(progn 
     (defclass ,name (latent-variable) ())
     (defmethod category-parameters ((v ,name))
       ',(utils:sort-symbols category-parameters))
     (defmethod interpretation-parameters ((v ,name))
       ',(utils:sort-symbols interpretation-parameters))))

