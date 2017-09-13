(cl:in-package #:latent-variables)

(defmethod latent-variable-attribute ((v latent-variable))
  (type-of v))

(defmethod latent-variable-attribute ((l linked))
  (mapcar #'latent-variable-attribute (latent-variable-links l)))

(defmethod category-parameters ((l linked))
  (let ((parameters (reduce #'union
			    (mapcar #'category-parameters
				    (latent-variable-links l)))))
    (utils:sort-symbols (copy-list parameters))))

(defmethod interpretation-parameters ((l linked))
  (let ((parameters (reduce #'union (mapcar #'interpretation-parameters
					    (latent-variable-links l)))))
    (utils:sort-symbols (copy-list parameters))))

(defmethod latent-state-parameters ((v latent-variable))
  (utils:sort-symbols (copy-list (append (category-parameters v)
					 (interpretation-parameters v)))))

(defmethod latent-variable-name ((l latent-variable))
  (string-downcase (symbol-name (latent-variable-attribute l))))

(defmethod latent-variable-name ((l linked))
  (format nil "~{~A~^-~}" (latent-variable-attribute l)))

(defmethod get-category-parameter (category parameter (v latent-variable))
  (let ((param-category (utils:make-plist (category-parameters v) category)))
    (getf param-category parameter)))

(defmethod get-interpretation-parameter (interpretation parameter (v latent-variable))
  (let ((param-interpretation (utils:make-plist (interpretation-parameters v)
						interpretation)))
    (getf param-interpretation parameter)))

(defmethod get-category (latent-state (v latent-variable))
  (mapcar #'(lambda (param) (getf latent-state param))
	  (category-parameters v)))

(defmethod get-interpretation (latent-state (v latent-variable))
  (mapcar #'(lambda (param) (getf latent-state param))
	  (interpretation-parameters v)))

(defmethod get-link-category ((l latent-variable) category (link latent-variable))
  category)

(defmethod get-link-category ((l linked) category (link latent-variable))
  (unless (member (type-of link) (latent-variable-links l) :key #'type-of)
    (warn "Attempt to get category of latent variable ~a from category of ~a while
~2:*~a is not a link of ~a" (latent-variable-name link) (latent-variable-name l)))
  (let ((annotated-latent-state (utils:make-plist (category-parameters l)
						  category)))
    (mapcar #'(lambda (p) (getf annotated-latent-state p))
	    (category-parameters link))))
	 

(defmethod get-event-category (event (v latent-variable))
  (let ((attribute-names (mapcar #'(lambda (attrib)
				      (symbol-name attrib))
				  (category-parameters v))))
    (loop for name in attribute-names collect
	 (apply (find-symbol name (find-package :md)) (list event)))))

(defmethod create-category ((v latent-variable) &rest parameters)
  (mapcar (lambda (parameter) (getf parameters parameter))
	  (category-parameters v)))

(defmethod create-latent-state ((v latent-variable) category
				&rest interpretation-parameters)
  (let ((parameters (append (utils:make-plist (category-parameters v) category)
			    interpretation-parameters)))
    (mapcar (lambda (param) (getf parameters param))
	    (latent-state-parameters v))))


(defmethod get-link-categories ((variable latent-variable) (link latent-variable))
  (remove-duplicates
   (mapcar (lambda (category)
	     (get-link-category variable category link))
	   (categories variable))
   :test #'equal))

(defmethod set-link-categories ((variable latent-variable))
  "Nothing needs to be done here.")
  
(defmethod set-link-categories ((variable linked))
  "This method determines the unique categories of <variable> that are represented in 
the categories of <variable> and sets the *category* slots of <link>."
  (dolist (link (latent-variable-links variable))
    (setf (categories link)
	  (get-link-categories variable link))))

(defmethod initialise-prior-distribution (training-data
					  (v latent-variable))
  "Retrieves the prior distribution based on <training-data> using the 
GET-PRIOR-DISTRIBUTION method of <v> and sets the *prior-distribution* slot 
of <v> as well as the *categories* slot of <v>."
  (let ((categories (mapcar #'car training-data))
	(training-data (mapcar #'cdr training-data)))
    (setf (prior-distribution v)
	  (get-prior-distribution training-data categories v))
    (setf (categories v) categories)))
  
(defmethod get-prior-distribution (training-data categories
				   (v latent-variable))
  "This method implements a default strategy for estimating the prior distribution of
a latent variable <v> based on <training-data> which is a list whose elements are lists
of all events sequences in the category at the same serial position in <categories>.
The relative frequency of each category is determed, each interpretation
 of each category is assigned a probability of relative to the category's relative frequency and
 the resulting distribution is re-normalised."
  (let* ((category-counts (mapcar #'length training-data))
	 (total-observations (apply #'+ category-counts))
	 (latent-states) (distribution))
    (loop for category in categories for count in category-counts do
	 (let ((rel-freq (/ count total-observations)))
	   (loop for latent-state in (get-latent-states category v) do
		(push latent-state latent-states)
		(push rel-freq distribution))))
    (mapcar #'cons
	    latent-states
	    (let ((scaling (apply #' + distribution)))
	      (mapcar #'(lambda (p) (/ p scaling)) distribution)))))

(defmethod get-latent-states (category (v latent-variable))
  (list category))

(defmethod get-latent-states (category (l linked))
  (let* ((links (latent-variable-links l))
	 ;; Obtain the category of each individual variable as encoded in the linked
	 ;; category
	 (categories (mapcar #'(lambda (link) (get-link-category l category link))
			     links))
	 ;; Obtain the latent states for each variable
	 (latent-states (mapcar #'(lambda (var category) (get-latent-states category var))
				links categories))
	 ;; Obtain all possible combinations of latent-states
	 (combined-latent-states (apply #'utils:cartesian-product latent-states))
	 (combined-latent-states (mapcar (lambda (combined-latent-state)
					   (apply #'append
						  (mapcar (lambda (latent-state link)
							    (utils:make-plist
							     (latent-state-parameters link)
							     latent-state))
							  combined-latent-state links)))
					 combined-latent-states)))
    (loop for latent-state in combined-latent-states collect
	 (loop for param in (latent-state-parameters l) collect
	      (getf latent-state param)))))

