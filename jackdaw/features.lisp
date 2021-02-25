(cl:in-package #:features)

(defun actual-last (list)
  (car (last list 1)))

(defmacro featurelet (event feature-definitions &body body)
  `(let (,@(loop for def in feature-definitions collect
		(let* ((name (car def))
		       (spec (actual-last def))
		       (doc (when (eq (length spec) 3)
			      (second spec))))
		  ;; Perhaps useful later.
		  (declare (ignorable doc))
		  `(,name ,(def-feature name event (car spec) (cdr spec))))))
     ,@body))

(defclass feature ()
  ((identifier :initarg :identifier :reader identifier)
   (function :initarg :function :reader f)
   (table :initarg :table :accessor table :initform (make-hash-table :test #'equal))
   (model-arguments-accessor :accessor model-arguments-accessor)
   (%model-table :accessor %model-table :initform nil)
   ;; The keys to %MODELS-TABLES.
   (model :accessor model :initform nil)
   (models :accessor models :initform nil)
   (observed? :initarg :observed? :accessor observed? :initform t)
   (observation-function :initarg :observation-function
			 :accessor observation-function :initform nil)
   (horizontal-args :initarg :horizontal-args :reader horizontal-args)
   (vertical-args :initarg :vertical-args :reader vertical-args)
   ;; Must be subset of VERTICAL-ARGS. TODO: make vertical-args the union of
   ;; feature-args and model-args to support model conditioning on any feature.
   (conditioning :initarg :conditioning :accessor conditioning)
   (model-args :initarg :model-args :accessor model-args)))

(defclass normal (feature) ())

(defclass recursive (normal)
  ((init-function :initarg :init-function :reader init-function)
   (init-observation-function :initarg :init-observation-function
			      :accessor init-observation-function :initform nil)
   (vertical-init-args :initarg :vertical-init-args :reader vertical-init-args)
   (horizontal-init-args :initarg :horizontal-init-args :reader horizontal-init-args))
  (:documentation "A recursive feature is a higher-order feature that derives
its next function from its own value in the previous event."))

(defun horizontal-argument-symbols (horizontal)
  (loop for s in horizontal collect
       (intern (format nil "PREVIOUS-~A" (symbol-name s)))))

(defun def-normal-feature (name event horizontal-args vertical-args f observation)
  (let ((horizontal-args (horizontal-argument-symbols horizontal-args))
	(horizontal (mapcar #'kw-symbol horizontal-args))
	(vertical (mapcar  #'kw-symbol vertical-args)))
    `(make-normal ',name ',horizontal ',vertical
		  (lambda (,@horizontal-args)
		    (declare (ignorable ,@horizontal-args))
		    (lambda (,@vertical-args)
		      (declare (ignorable ,@vertical-args))
		      ,f))
		  ,(unless (null observation)
			   `(lambda (,@horizontal-args)
			      (declare (ignorable ,@horizontal-args))
			      (lambda (,event ,@vertical-args)
				(declare (ignorable ,event ,@vertical-args))
				,observation))))))

(defun def-recursive-feature (name event horizontal-args vertical-args f
			      init-horizontal-args init-vertical-args init-f observation
			      init-observation)
  (let ((horizontal-args (horizontal-argument-symbols (cons name horizontal-args)))
	(init-horizontal-args (horizontal-argument-symbols init-horizontal-args))
	(horizontal (mapcar #'kw-symbol horizontal-args))
	(vertical (mapcar  #'kw-symbol vertical-args))
	(init-horizontal (mapcar #'kw-symbol init-horizontal-args))
	(init-vertical (mapcar  #'kw-symbol init-vertical-args)))
    `(make-recursive ',name ',horizontal ',vertical
		     (lambda (,@horizontal-args)
		       (declare (ignorable ,@horizontal-args))
		       (lambda (,@vertical-args)
			 (declare (ignorable ,@vertical-args))
			 ,f))
		     ',init-horizontal ',init-vertical
		     (lambda (,@init-horizontal-args)
		       (declare (ignorable ,@init-horizontal-args))
		       (lambda (,@init-vertical-args)
			 (declare (ignorable ,@init-vertical-args))
			 ,init-f))
		     ,(unless (null observation)
			      `(lambda (,@horizontal-args)
				(declare (ignorable ,@horizontal-args))
				(lambda (,event ,@vertical-args)
				  (declare (ignorable ,event ,@vertical-args))
				  ,observation)))
		     ,(unless (null init-observation)
			      `(lambda (,@init-horizontal-args)
				(declare (ignorable ,@init-horizontal-args))
				(lambda (,event ,@init-vertical-args)
				  (declare (ignorable ,event ,@init-vertical-args))
				  ,init-observation))))))  

(defun kw-symbol (s)
  (intern (symbol-name s) :keyword))

(defun def-feature (name event type arguments)
  "Define either a normal or a recursive feature. If
the feature is recursive and INIT-OBSERVATION is NIL,
use OBSERVATION for INIT-OBSERVATION."
  (let ((name (kw-symbol name)))
    (case (intern (symbol-name type) :features)
      (normal
       (destructuring-bind (horizontal vertical f &optional observation)
	   arguments
	 (def-normal-feature name event horizontal
			     vertical f observation)))
      (recursive
       (destructuring-bind (horizontal vertical f init-h init-v init-f
				       &optional observation init-observation)
	   arguments
	 (def-recursive-feature name event horizontal
				vertical f
				init-h init-v init-f
				observation
				(if (null init-observation)
				    observation init-observation))))
      (t (warn "Feature type ~A not recognised." type)))))

(defmethod modeled? ((f feature))
  (not (null (model f))))

(defmethod observable? ((f feature))
  (not (null (observation-function f))))

(defmethod hide ((feature feature) &rest more-features)
  (setf (observed? feature) nil)
  (unless (null more-features)
    (apply #'hide more-features)))

(defmethod observe ((feature feature) &rest more-features)
  (setf (observed? feature) t)
  (unless (null more-features)
    (apply #'observe more-features)))

(defmethod printable ((f feature))
  (list :model-keys (loop for key being the hash-keys of (%model-table f) collect key)
	:models (loop for key being the hash-keys of (%model-table f)
		   collect (models::printable (gethash key (%model-table f))))))

(defmethod from-printable (printable (f feature))
  (setf (slot-value f 'models)
	(mapcar #'models::from-printable (getf printable :models)))
  (loop for key in (getf printable :model-keys)
     for model in (models f) do
       (setf (gethash key (%model-table f)) model)))

(defmethod get-model ((f feature) conditioning first?)
  (multiple-value-bind (model found?)
      (get-or-create-and-set
       conditioning (%model-table f)
       (apply #'make-instance (append (list (model f))
				      (model-args f)
				      (list :conditioning conditioning))))
    (unless found? (push model (models f))) model))

(defmethod create-arguments-accessor ((f feature) positions)
  "Access the subset of feature arguments designated as model arguments."
  (lambda (arguments first?)
    (declare (ignorable first?))
    (mapcar (lambda (p) (elt arguments p)) positions)))

(defmethod create-arguments-accessor ((f recursive) positions)
  "Access the subset of recursive feature arguments designated as model arguments."
  (let ((init-positions
	 (mapcar (lambda (id) (position id (vertical-init-args f)))
		 (conditioning f))))
    (when (some #'null init-positions)
      (error "Conditioning model of ~A on a feature that is not in its init arguments."
	    (identifier f)))
    (lambda (arguments first?)
      (mapcar (lambda (p) (elt arguments p)) (if first? init-positions positions)))))

(defmethod set-model ((f feature) model-class &key conditioning model-args)
  "Add a model of MODEL-CLASS to F. 

CONDITIONING is a list of feature instances.
MODEL-ARGS is a set of keyword arguments.

CONDITIONING is used to select the model (features on which the model is 
conditioned).

MODEL-ARGS are keyword arguments passed to each instance of MODEL-CLASS.
MODEL-FEATURE args is also passed to each instance of MODEL-CLASS as keyword
arguments."
  (let* ((conditioning (identifiers conditioning))
	 (positions
	  (mapcar (lambda (a) (position a (vertical-args f))) conditioning)))
    (when (some #'null positions)
      (error "Conditioning model of ~A on a feature that is not in its arguments."
	     (identifier f)))
    (setf (model-args f) model-args)
    (setf (conditioning f) conditioning)
    (setf (%model-table f) (make-hash-table :test #'equal))
    (setf (model-arguments-accessor f) (create-arguments-accessor f positions))
    (setf (model f) model-class)))

(defmethod all-arguments ((f feature))
  (append (horizontal-args f) (vertical-args f)))

(defmethod all-arguments ((f recursive))
  (append (call-next-method) (horizontal-init-args f) (vertical-init-args f)))

(defun identifiers (features)
  (mapcar #'f:identifier features))

;;;;;;;;;;;; Off the shelve higher-order functions ;;;;;;;;;;;;;;;

(defun generate (domain)
  (lambda () (lambda () domain)))

(defun repeat-previous ()
  (lambda (previous) (lambda () (list previous))))

;;;;;;;;;;;; Feature constructors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-normal (identifier horizontal vertical function &optional observation)
  "Create a feature with identifier IDENTIFIER (must be symbol or distinguishable
by #'EQ), HORIZONTAL and VERTICAL are lists of feature identifiers. They specify
horizontal and vertical arguments. FUNCTION is a nested function whose outer 
lambda list corresponds to the horizontal arguments and whose inner lambda list 
corresponds to vertical arguments."
  (make-instance
   'normal :identifier identifier :function function
   :horizontal-args horizontal :vertical-args vertical
   :observation-function observation
   :observed? (not (null observation))))

(defun make-recursive (identifier horizontal vertical function
		       horizontal-init vertical-init init-function
		       &optional observation init-observation)
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
   :vertical-init-args vertical-init
   :observation-function observation
   :init-observation-function init-observation
   :observed? (not (or (null observation) (null init-observation)))))

