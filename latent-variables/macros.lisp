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
  (let ((latent-state `(utils:make-plist (latent-state-attributes ,variable)
					 ,latent-state))
	(category-set-symbol `(%category-set-symbol ,variable))
	(interpretation-set-symbol `(%interpretation-set-symbol ,variable)))
    `(with-latent-state (append ,latent-state
				(list ,category-set-symbol t ,interpretation-set-symbol t))
       ,@body)))

(defmacro with-latent-category ((category variable) &body body)
  (let ((latent-state `(utils:make-plist (category-attributes ,variable)
					 ,category))
	(category-set-symbol `(%category-set-symbol ,variable)))
    `(with-latent-state (append ,latent-state (list ,category-set-symbol t)) ,@body)))

(defmacro with-latent-interpretation ((interpretation variable) &body body)
  (let ((latent-state `(utils:make-plist (interpretation-attributes ,variable)
					 ,interpretation))
	(interpretation-set-symbol `(%interpretation-set-symbol ,variable)))
    `(with-latent-state (append ,latent-state (list ,interpretation-set-symbol t)) ,@body)))

(defmacro define-latent-variable (name category-attributes interpretation-attributes)
  `(progn 
     (defclass ,name (latent-variable) ())
     (defmethod category-attributes ((v ,name))
       ',(utils:sort-symbols category-attributes))
     (defmethod interpretation-attributes ((v ,name))
       ',(utils:sort-symbols interpretation-attributes))))

