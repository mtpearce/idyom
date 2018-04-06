(cl:in-package #:latent-variables)

(defmethod latent-variable-attribute ((v latent-variable))
  "Return the symbol representing the name of the class that defines 
latent variable V."
  (type-of v))

(defmethod latent-variable-attribute ((l linked))
  "Return a list of the latent variable attributes of each of the constituent
links of linked latent variable L."
  (mapcar #'latent-variable-attribute (latent-variable-links l)))

(defmethod category-parameters ((l linked))
  "Return the category parameters of linked latent variable L.
These are derived from the category parameters of the constituent links of L
by concatenating them into a list and sorting that list alphabetically."
  (let ((parameters (apply #'append
			   (mapcar #'category-parameters
				   (latent-variable-links l)))))
    (utils:sort-symbols (copy-list parameters))))

(defmethod latent-state-parameters ((v latent-variable))
  "Return the latent-state parameters of linked latent variable L.
These are derived from the latent-state parameters of the constituent links of L
by concatenating them into a list and sorting that list alphabetically."
  (utils:sort-symbols (copy-list (remove-duplicates (append (category-parameters v)
							    (interpretation-parameters v))))))

(defmethod latent-variable-name ((l latent-variable))
  "Return a string representation of the latent-variable attribute of 
latent variable L."
  (string-downcase (symbol-name (latent-variable-attribute l))))

(defmethod latent-variable-name ((l linked))
  "Return a string representation of the list of latent-variable attributes of the 
constituent links of L by concatenating them in a string separated by dashes."
  (format nil "~{~A~^-~}" (latent-variable-attribute l)))

(defmethod get-category-parameter (category parameter (v latent-variable))
    "Return the value of category parameter PARAMETER as encoded in CATEGORY,
which is a category of latent variable V."
  (let ((param-category (utils:make-plist (category-parameters v) category)))
    (getf param-category parameter)))

(defmethod get-latent-state-parameter (latent-state parameter (v latent-variable))
  "Return the value of PARAMETER, which is a latent-state parameter of V, in latent 
state LATENT-STATE, which is a latent-state of V."
  (let ((param-interpretation (utils:make-plist (latent-state-parameters v)
						latent-state)))
    (getf param-interpretation parameter)))

(defmethod get-category (latent-state (v latent-variable))
  "Return only the values of the category parameters encoded in LATENT-STATE,
which is a latent state of latent variable V."
  (let ((latent-state-plist (utils:make-plist (latent-state-parameters v)
					      latent-state)))
  (mapcar #'(lambda (param) (getf latent-state-plist param))
	  (category-parameters v))))

(defmethod get-link-category ((l latent-variable) category (link latent-variable))
  category)

(defmethod get-link-category ((l linked) category (link latent-variable))
  "Return the category of LINK, which is a constituent link of linked latent variable L, 
as encoded in CATEGORY, which is a category of L."
  (let ((annotated-latent-state (utils:make-plist (category-parameters l)
						  category)))
    (mapcar #'(lambda (p) (getf annotated-latent-state p))
	    (category-parameters link))))

(defmethod get-event-category (event (v latent-variable))
  "Return the category of latent variable V for EVENT.
By default the category is determined by retrieving the values of the event attributes 
corresponding to the category parameters of V.
Specialisers of this method can be written to define more complex behaviors for certain
types of latent variables."
  (let ((attribute-names (mapcar #'(lambda (attrib)
				      (symbol-name attrib))
				  (category-parameters v))))
    (loop for name in attribute-names collect
	 (apply (find-symbol name (find-package :md)) (list event)))))

(defmethod create-category ((v latent-variable) &rest parameters)
  "Create a category of V based on parameters provided as a set of keyword arguments
to this function."
  (mapcar (lambda (parameter) (getf parameters parameter))
	  (category-parameters v)))

(defmethod create-latent-state ((v latent-variable) category &rest interpretation-parameters)
  "Create a latent state for latent variable V based on a category of V and 
some interpretation parameters provided as keyword arguments to this function."
  (let ((parameters (append (utils:make-plist (category-parameters v) category)
			    interpretation-parameters)))
    (mapcar (lambda (param) (getf parameters param))
	    (latent-state-parameters v))))

(defmethod initialise-prior-distribution (category-training-sets (v latent-variable))
  "Calculate the prior distribution of V with GET-PRIOR-DISTRIBUTION based on
TRAINING-DATA, which is an ALIST whose keys correspond to categories (labels), and
whose values correspond to list of training items associated with the category."
  (let* ((categories (mapcar #'car category-training-sets))
	 (training-sets (mapcar #'cdr category-training-sets)))
    (setf (prior-distribution v)
	  (get-prior-distribution training-sets categories v))
    (setf (categories v) categories)))

(defmethod get-prior-distribution (training-data categories (v latent-variable))
  "Default strategy for estimating the prior distribution. 
Estimate the prior distribution of latent variable V. TRAINING-DATA is a list 
of lists containing training items. CATEGORY is a list of categories. Each category 
indicates the category of all training items in the corresponding list of training 
items in TRAINING-DATA. The prior probability of that category is estimated by the number
of corresponding training items divided by the total number of training items.
The probability of each latent-state is estimated by this probability, divided by the total
number of latent states.
Return an ALIST whose keys correspond to latent states and whose values correspond to
the prior probability of the corresponding latent state.
Specialisers of this method may be written to define custom strategies for estimating the
prior distribution for specific types of latent variables."
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
  "Return a single latent state of latent variable V representing 
category CATEGORY of V.
Specialisers for this method may be written to implement more complex behavior for
certain classes of latent variables."
  (list (create-latent-state v category)))

(defmethod get-latent-states (category (l linked))
  "Return the Cartesian product of the latent states associated with 
<category> of the links of <l>."
  (let* ((links (latent-variable-links l))
	 ;; Obtain the category of each individual variable as encoded in the linked
	 ;; category
	 (categories (mapcar #'(lambda (link) (get-link-category l category link))
			     links))
	 ;; Obtain the latent states for each variable
	 (latent-states (mapcar #'(lambda (var category) (get-latent-states category var))
				links categories))
	 ;; Obtain all possible combinations of latent-states
	 (latent-state-sets (apply #'utils:cartesian-product latent-states)))
    (loop for latent-state-set in latent-state-sets collect
	 (combine-link-latent-states l latent-state-set))))

(defmethod print-latent-variable ((v latent-variable))
  (let ((categories (categories v))
	(prior (prior-distribution v)))
    (format t "Category: (~{~A~^, ~}). Interpretation: ~{~A~^, ~}~%"
	    (category-parameters v)
	    (interpretation-parameters v))
    (loop for c in categories do
	 (let ((latent-states (get-latent-states c v)))
	   (format t "P(~A) = ~A~%" c (apply #'+ (mapcar (lambda (l)
							   (cdr (assoc l prior :test #'equal)))
						       latent-states)))
	   (loop for l in latent-states do
		(format t "----P(~A) = ~A~%" l (cdr (assoc l prior :test #'equal))))))))
						       
