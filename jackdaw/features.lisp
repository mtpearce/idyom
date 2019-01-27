(cl:in-package #:features)

(defun horizontal-argument-symbols (horizontal)
  (loop for s in horizontal collect
       (intern (format nil "PREVIOUS-~A" (symbol-name s)))))

(defun def-normal-feature (name horizontal vertical f)
  (let ((horizontal-args (horizontal-argument-symbols horizontal)))
    `(make-normal ',name ',horizontal ',vertical
	     (lambda (,@horizontal-args)
	       (lambda (,@vertical)
		 ,f)))))

(defun def-recursive-feature (name horizontal vertical f
			      init-horizontal init-vertical init-f)
  (let ((horizontal-args (horizontal-argument-symbols (cons name horizontal)))
	(init-horizontal-args (horizontal-argument-symbols init-horizontal)))
  `(make-recursive ',name ',horizontal ',vertical
	   (lambda (,@horizontal-args)
	     (lambda (,@vertical)
	       ,f))
	   ',init-horizontal ',init-vertical
	   (lambda (,@init-horizontal-args)
	     (lambda (,@init-vertical)
	       ,init-f)))))

(defun def-feature (name type arguments)
  (case (intern (symbol-name type) :features)
    (normal
     (destructuring-bind (horizontal vertical f)
	 arguments
       (def-normal-feature name horizontal
	 vertical f)))
    (recursive
     (destructuring-bind (horizontal vertical f init-h init-v init-f)
	 arguments
       (def-recursive-feature name horizontal
	 vertical f
	 init-h
	 init-v init-f)))
    (t (warn "Feature type ~A not recognised." type))))

(defmacro featurelet (feature-definitions &body body)
  `(let (,@(loop for def in feature-definitions collect
	       (let ((name (car def))
		     (def (cadr def)))
		 `(,name ,(def-feature name (car def) (cdr def))))))
     ,@body))

(defclass feature ()
  ((identifier :initarg :identifier :reader identifier)
   (function :initarg :function :reader f)
   (table :initarg :table :accessor table :initform (make-hash-table :test #'equal))
   (model-accessor :accessor model-accessor :initform nil)
   (%model-table :accessor %model-table :initform nil)
   (models :accessor models :initform nil)
   (representational? :initarg :representational?
		      :accessor representational? :initform nil)
   (observation-function :initarg :observation-function
			 :accessor observation-function :initform nil)
   (horizontal-args :initarg :horizontal-args :reader horizontal-args)
   (vertical-args :initarg :vertical-args :reader vertical-args)))

(defclass normal (feature) ())

(defclass recursive (normal)
  ((init-function :initarg :init-function :reader init-function)
   (vertical-init-args :initarg :vertical-init-args :reader vertical-init-args)
   (horizontal-init-args :initarg :horizontal-init-args :reader horizontal-init-args))
  (:documentation "A recursive feature is a higher-order feature that derives
its next function from its own value in the previous event."))

(defmethod observable? ((f normal))
  (not (null (observation-function f))))

(defmethod modeled? ((f normal))
  (not (null (model-accessor f))))

(defmethod observe ((f normal) event)
  (funcall (observation-function f) event))

(defmethod get-model-accessor ((f recursive) positions spawn-model)
  (when (eq (car positions) 0)
    (warn "Conditioning recursive ~A feature on itself."
	  (identifier f)))
  (let* ((table (%model-table f))
	 (conditionals (mapcar (lambda (p) (elt (vertical-args f) p)) positions))
	 (init-positions (mapcar (lambda (id) (position id (vertical-init-args f)))
					 conditionals)))
    (when (some #'null init-positions)
      (warn "Conditioning model of ~A on a feature that is not in its init arguments."
	    (identifier f)))
    (lambda (arguments first?)
      (let* ((positions (if first? init-positions positions))
	     (key (mapcar (lambda (p) (elt arguments p)) positions)))
	(multiple-value-bind (model found?)
	    (gethash key table (funcall spawn-model))
	  (unless found?
	    (push model (models f))
	    (setf (gethash key table) model))
	  model)))))

(defmethod get-model-accessor ((f normal) positions spawn-model)
  (let ((table (%model-table f)))
    (lambda (arguments first?)
      (declare (ignorable first?))
      (let ((key (mapcar (lambda (p) (elt arguments p)) positions)))
	(multiple-value-bind (model found?)
	    (gethash key table (funcall spawn-model))
	  (unless found?
	    (push model (models f))
	    (setf (gethash key table) model))
	  model)))))

(defmethod add-model ((f normal)
		      &key conditionals
			(spawn-model
			 (lambda ()
			   (ppm:make-ppm nil :escape :c :mixtures t
					 :update-exclusion nil
					 :order-bound nil))))
  (let ((positions (mapcar (lambda (c)
			     (position (identifier c) (vertical-args f)))
			   conditionals)))
    (when (some #'null positions)
      (warn "Conditioning model of ~A on a feature that is not in its arguments."
	    (identifier f)))
    (setf (%model-table f) (make-hash-table :test #'equal))
    (setf (model-accessor f)
	  (get-model-accessor f positions spawn-model))))

(defmethod identifiers ((f normal) &rest rest)
  (cons (identifier f) (unless (null rest) (identifiers rest))))

(defmethod make-observable ((f normal) &optional (observation-function #'identity))
  "OBSERVATION-FUNCTION is a function that takes an observed event as arguments and
returns one value."
  (setf (observation-function f) observation-function))

(defmethod hide ((f normal) &rest more-features)
  (setf (observation-function f) nil)
  (unless (null more-features)
    (apply #'hide more-features)))

(defmethod make-representational ((f normal))
  "Representational features not part of the probabilistic generative model.
The relation between its values and inputs must be surjective. That is,
any given output value must correspond to no more than one combination of input 
values. Representation features may not be modeled and other features may not
condition on them."
  (setf (representational? f) t))

;;;;;;;;;;;; Off the shelve higher-order functions ;;;;;;;;;;;;;;;

(defun generate (domain)
  (lambda () (lambda () domain)))

(defun repeat-previous ()
  (lambda (previous) (lambda () (list previous))))

;;;;;;;;;;;; Feature constructors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-normal (identifier horizontal vertical function)
  "Create a feature with identifier IDENTIFIER (must be symbol or distinguishable
by #'EQ), HORIZONTAL and VERTICAL are lists of feature identifiers. They specify
horizontal and vertical arguments. FUNCTION is a nested function whose outer 
lambda list corresponds to the horizontal arguments and whose inner lambda list 
corresponds to vertical arguments."
  (make-instance
   'normal :identifier identifier :function function
   :horizontal-args horizontal :vertical-args vertical))

(defun make-recursive (identifier horizontal vertical function
		       horizontal-init vertical-init init-function)
  "Create a recursive feature. First arguments are the same as those of MAKE-NORMAL,
but FUNCTION receives as its first outer argument the feature's own value in the
previous moment, followed by arguments indicated in HORIZONTAL.
HORIZONTAL-INIT and VERTICAL-INIT are lists of feature identifiers and specify
arguments to INIT-FUNCTION analogously to how normal features are specified."
  (make-instance
   'recursive :identifier identifier
   :function function :init-function init-function
   :horizontal-args horizontal :vertical-args vertical
   :horizontal-init-args horizontal-init
   :vertical-init-args vertical-init))
