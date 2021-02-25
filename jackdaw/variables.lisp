(defvar +inactive+ '*)
(defvar +singleton+ (list +inactive+))
(defvar +ngram-filler+ '*)

(defclass generative-model (graphs::dag)
  ((training? :accessor training? :initform nil)
   (edge-table :reader edge-table :type 'hastable)
   (variables :reader variables :type 'list)
   (observed :reader observed :initform nil :type 'list)
   (constraints :reader constraints :type 'hashtable)
   (distributions :reader distributions :type 'hashtable)))

(defun constraint-argument (v)
    "Given a keyword representation of a variable, return
its congruency constraint function symbol. For example,
given :X, return $X"
  (intern (format nil "$~A" (symbol-name v))))

(defun getarg (key state)
  "Given a keyword representation of a variable, obtain
its value from a model state."
  (gethash key state))
;;(getf state key))

(defun apriori (v)
  (intern (format nil "^~A" (symbol-name v)) :keyword))

(defun basename (s)
  "Given an a priori version of a variable name, return its
stem. For example, if S is :^X, (BASENAME S) is :X."
  (intern (subseq (symbol-name s) 1) :keyword))

(defun get-horizontal-arguments (arglist)
  (loop for arg in arglist
     if (eq (elt (symbol-name arg) 0) #\^)
       collect (basename arg)))

(defun get-vertical-arguments (arglist)
  (loop for arg in arglist
     if (not (eq (elt (symbol-name arg) 0) #\^))
     collect (intern (symbol-name arg) :keyword)))

;; Constraint definition macros

(defmacro v (self parents constraint)
  (declare (ignorable self)) ;; Remove?
  `(lambda (model %state)
     (declare (ignorable model))
     (let (,@(loop for v in parents collect
		  (list (constraint-argument v) `(getarg ,v %state))))
       ,constraint)))

(defmacro recursive (self parents
		     constraint initialization-constraint)
  (let ((previous-self (apriori self)))
    (unless (member previous-self parents)
      (error "Recursive variable ~A must contain ~A in its parents."
	     self previous-self))
    `(v ,self ,parents
       (if (eq ,(constraint-argument previous-self) +inactive+)
	   ,initialization-constraint
	   ,constraint))))

(defmacro one-shot (self parents constraint)
  `(recursive ,self ,parents (list ,(constraint-argument (apriori self))) ,constraint))

(defmacro accumulator (self parents constraint initialization-constraint)
  `(recursive ,self ,parents
	      (mapcar (lambda (s) (cons s ,(constraint-argument (apriori self))))
		      ,constraint)
	      (mapcar #'list ,initialization-constraint)))

(defmacro ngram-accumulator (self parents constraint initialization-constraint n)
  `(recursive ,self ,parents
	      (mapcar (lambda (s)
			(cons s (subseq ,(constraint-argument (apriori self)) 0 (1- ,n))))
		      ,constraint)
	      (mapcar (lambda (s)
			(cons s (loop repeat (1- ,n) collect +ngram-filler+)))
		      ,initialization-constraint)))

;; Model definition 

(defmacro defmodel (class superclasses direct-slots variable-specs distribution-specs)
  (let* ((edges (make-hash-table))
	 (variables) (constraints) (distributions))
    (loop for specification in variable-specs do
	 (destructuring-bind (type v parents &rest args)
	     specification
	   (push v variables)
	   (setf (gethash v edges) parents)
	   (push (macroexpand `(,type ,v ,parents ,@args))
		 constraints)))
        (loop for specification in distribution-specs do
	 (destructuring-bind (type v parents &rest args)
	     specification
	   (setf (gethash v edges)
		 (union (gethash v edges) parents))
	   (push `(,v (make-instance ',type :arguments (list ,@parents)
				     :variable ,v ,@args))
		 distributions)))
;;	   (setf (gethash v constraints)
;;		 (macroexpand `(,type ,v ,parents ,@args)))))
    `(defclass ,class ,superclasses
       ((distributions :initform (list ,@(apply #'append (reverse distributions))))
	(variables :initform (list ,@(reverse variables)))
	(edge-table :initform ,edges)
	(constraints :initform (list ,@(reverse constraints)))
	,@direct-slots))))

;; Model mechanics

(defmethod graphs:edges ((m generative-model) variable)
  (gethash variable (edge-table m)))

(defmethod constraint ((m generative-model) variable)
  "Return the congruency constraint associated with VARIABLE in model M."
  (let ((p (position variable (variables m))))
    (elt (constraints m) p)))

(defmethod distribution ((m generative-model) variable)
  (getf (distributions m) variable (make-instance 'uniform)))

(defmethod marginal-params ((m generative-model))
  (let* ((horizontal-dependencies
	 (mapcar (lambda (v) (get-horizontal-arguments (graphs:edges m v))) (variables m))))
    (remove-duplicates (apply #'append horizontal-dependencies))))

(defmethod a-priori-congruent ((m generative-model) variable parents-state)
  (funcall (constraint m variable) m parents-state))

(defmethod a-posteriori-congruent ((m generative-model) variable parents-state moment a-priori)
  "Return the a posteriori congruent states of VARIABLE."
  (let ((value (getf moment variable 'notfound)))
    (if (member variable (observed m))
	(progn
	  (when (eq value 'notfound)
	    (warn "Observed attribute ~A not set in moment." variable))
	  (list value))
	a-priori)))

(defmethod generate-states ((m generative-model) vertices previous-state moment)
  (let* ((parent-states (if (null (cdr vertices))
			    (list previous-state)
			    (generate-states m (cdr vertices) previous-state moment)))
	 (vertex (car vertices))
	 (training? (training? m))
	 (new-states))
    (dolist (parents-state parent-states new-states)
      (let* ((probability (gethash 'probability parents-state))
	     (a-priori (a-priori-congruent m vertex parents-state))
	     (a-posteriori (a-posteriori-congruent m vertex parents-state moment a-priori))
	     (distribution (unless training?
			     (probabilities (distribution m vertex)
							parents-state
							a-priori))))
	(when (eq (length a-priori) 0)
	  (warn "~A has no a priori congruent states." vertex))
	(when (and training? (> (length a-posteriori) 1))
	  (warn "Don't know what to do with multiple a posteriori 
congruent states of ~A during training." vertex))
	(when (eq (length a-posteriori) 0)
	  (warn "~A has no a posteriori congruent states." vertex))
	(observe (distribution m vertex) parents-state a-posteriori training?)
	(dolist (s a-posteriori)
	  (let ((s-probability (unless training? (gethash s distribution)))
		(new-state (copy-hash-table parents-state)))
	    (setf (gethash vertex new-state) s)
	    (unless training?
	      (setf (gethash 'probability new-state) (* probability s-probability)))
	    (push new-state new-states)))))))

(defmethod moment ((m temperley) moment congruent-states)
  ;; Todo implement more efficient state generation
  (call-next-method))

(defmethod generate-states ((m temperley) vertices previous-state moment)
  ;; Todo implement more efficient state generation
  (call-next-method))

(defmethod rotate-state ((m generative-model) state &key (keep-trace? t))
  "\"Rotate\" a state. In the a priori version of a state, every parameter
:X is renamed :^X and variables of the form :^X in STATE are dropped."
  (let ((new-state (make-hash-table)))
    (setf (gethash 'probability new-state) (gethash 'probability state))
    (when keep-trace?
      (let ((trace (make-hash-table)))
	(dolist (key (cons 'trace (marginal-params m)))
	  (setf (gethash key trace) (gethash key state)))
	(setf (gethash 'trace new-state) trace)))
    (dolist (variable (marginal-params m) new-state)
      (setf (gethash (apriori variable) new-state) (gethash variable state)))))

(defun trace-back (state variable &optional trace)
  (let ((new-trace (cons (gethash variable state) trace))
	(previous-state (gethash 'trace state)))
    (if (null previous-state)
	new-trace
	(trace-back previous-state variable new-trace))))

(defmethod root-state ((m generative-model))
  (let ((state (make-hash-table)))
    (setf (gethash 'probability state) 1)
    (dolist (variable (mapcar #'apriori (marginal-params m)) state)
      (setf (gethash variable state) +inactive+))))

(defun marginalize (states variables &optional (marginal (make-hash-table :test #'equal)))
  (dolist (state states)
    (let* ((state-prob (gethash 'probability state))
	   (trace (gethash 'trace state))
	   (key (loop for v in variables collect (gethash v state)))
	   (marginal-prob (car (gethash key marginal (cons 0 trace)))))
      (setf (gethash key marginal) (cons (+ state-prob marginal-prob) trace)))))

(defun marginal->states (marginal variables)
  (let ((states))
    (maphash (lambda (state-key p)
	       (let ((state (make-hash-table)))
		 (setf (gethash 'probability state) (car p))
		 (setf (gethash 'trace state) (cdr p))
		 (loop for v in variables
		    for s in state-key do
		      (setf (gethash v state) s))
		 (push state states)))
	     marginal)
    states))

(defmethod moment ((m generative-model) moment congruent-states)
  (let ((marginal (make-hash-table :test #'equal))
	(marginal-variables (mapcar #'apriori (marginal-params m))))
    (dolist (previous-state congruent-states)
      (let* ((new-states (model-congruent-states m previous-state moment))
	     (marginal-states (mapcar (lambda (s) (rotate-state m s)) new-states)))
	(marginalize marginal-states marginal-variables marginal)))
    (marginal->states marginal marginal-variables)))

(defmethod model-congruent-states ((m generative-model) previous-state moment)
  "Call GENERATE-STATES on topologically sorted list of variables, from which
inactive variables have been pruned."
  (generate-states m (get-vertical-arguments
		      (graphs:topological-sort m)) previous-state
		      moment))

;;;;;;;;;;;;;;;;;;; Probability distributions ;;;;;;;;;;;;;;;;;;;


(defclass distribution ()
  ((arguments :initarg :arguments :reader arguments :initform nil)
   (variable :initarg :variable :reader dist-var)))
(defclass bernouilli (distribution)
  ((p :initarg :p :accessor p)
   (symbols :initarg :symbols :accessor symbols)))
(defclass categorical (distribution)
  ((category-counts :accessor category-counts :initform (make-hash-table :test #'equal))
   (p :reader p :initform (make-hash-table :test #'equal))))
(defclass uniform (distribution) ())

(defmethod initialize-instance :after ((d categorical) &key parameters)
  "Parameters must be supplied as an ALIST: a list with items (PARAM . PROB). 
The context of a parameter is (CDR PROB), and corresponds to a list of states 
corresponding to variables that D is conditioned on. If D is not conditioned 
on anything, the context may be set to NIL. This means that each parameter 
must be a list of length 1 (the CDR of which is NIL)."
  (dolist (parameter parameters)
    (setf (gethash (car parameter) (parameters d)) (cdr parameter)))
  (let ((contexts (remove-duplicates (mapcar #'cdar parameters) :test #'equal)))
    (dolist (context contexts)
      (let ((sum 0))
	(maphash (lambda (param v) (when (equal (cdr param) context)
				     (incf sum v)))
		 (parameters d))
	(when (> (abs (- sum 1)) 1.0e-10)
	  (warn "Parameters of ~A sum to ~A, not to approximately 1.0, for context ~A."
		(dist-var d) sum context))))))

(defmethod initialize-ppm ((d idyom-ppm))
  (make-instance 'ppm:ppm
		 :escape (escape d)
		 :order-bound (order-bound d)
		 :mixtures (mixtures d)
		 :update-exclusion (update-exclusion d)
		 :alphabet (alphabet d)))

(defmethod observe ((d distribution) state symbol training?)
  "Ignore observations by default.")

(defmethod observe ((d categorical) state symbol training?)
  (when training?
    (let* ((arguments (mapcar (lambda (v) (getarg v state)) (arguments d)))
	   (counts (category-counts d))
	   (new-arg-count (1+ (gethash arguments counts 0)))
	   (new-s-count (1+ (gethash (cons symbol arguments) counts 0))))
      (setf (gethash arguments counts) new-arg-count)
      (setf (gethash (cons symbol arguments) counts) new-s-count)
      (setf (gethash (cons symbol arguments) (parameters d))
	    (/ new-s-count new-arg-count)))))

(defmethod observe ((d idyom-ppm) state symbol training?)
  (let* ((location-key (getarg (dist-var d) state)) ; previous state of self
	(model-key (mapcar (lambda (v) (getarg v state))
			   (remove (dist-var d) (arguments d))))
	(model (gethash model-key (category-models d)
			(initialize-ppm d)))
	(location (gethash location-key (locations d))))
    (remhash location-key (locations d))
    (ppm::add-event-to-model-dataset model symbol)
    (let* ((next-location (ppm::ukkstep model nil location symbol t)))
      (when training? (ppm::increment-counts model next-location))
      (ppm:increment-event-front model)
      (setf (gethash symbol (locations d)) next-location))))

(defmethod next-sequence ((d distribution) training?)
  "Called after each training sequence. May be used to update
model state.")

(defmethod next-sequence ((d idyom-ppm) training?)
  (loop for k being the hash-keys of (category-models d) do
       (let ((model (gethash k (category-models d))))
	 (when training? (ppm:initialise-virtual-nodes model))
	 (ppm:increment-sequence-front model))))
       
(defmethod probability ((d uniform) arguments symbol)
  1)


(defmethod probability ((d bernouilli) arguments symbol)
  (when (not (null arguments))
    (warn "It looks like you're conditioning a Bernouilli distribution on something,
the implementation does not support this."))
  (if (equal symbol (car (symbols d)))
      (p d)
      (progn
	(unless (equal symbol (cadr (symbols d)))
	  (warn "Generating (1 - p) probability for unfamiliar symbol."))
	(- 1 (p d)))))


(defmethod probability ((d categorical) arguments symbol)
  (gethash (cons symbol arguments) (parameters d)))

(defmethod probabilities ((d distribution) parents-state congruent-states)
  (let* ((table (make-hash-table :test #'equal))
	 (arguments (mapcar (lambda (v) (getarg v parents-state)) (arguments d)))
	 (probabilities (mapcar (lambda (s) (probability d arguments s))
				congruent-states))
	 (sum (apply #'+ probabilities)))
    (loop for s in congruent-states for p in probabilities do
	 (setf (gethash s table)
	       (/ p sum)))
    table))

(defmethod probabilities ((d idyom-ppm) parents-state congruent-states)
  (let* ((table (make-hash-table :test #'equal))
	 (location-key (getarg (dist-var d) parents-state)) ; previous state of self
	 (model-key (mapcar (lambda (v) (getarg v parents-state))
			    (remove (dist-var d) (arguments d))))
	 (model (gethash model-key (category-models d)))
	 (location (gethash location-key (locations d)))
	 (alphabet (mapcar #'car congruent-states)))
    (ppm:set-alphabet model alphabet)
    (dolist (p (ppm::get-distribution model location))
      (let ((symbol (car p))
	    (probability (cadr p)))
      (setf (gethash (cons symbol location-key) table) probability)))))

(defun copy-hash-table (hash-table)
  (let ((ht (make-hash-table 
             :test (hash-table-test hash-table)
             :rehash-size (hash-table-rehash-size hash-table)
             :rehash-threshold (hash-table-rehash-threshold hash-table)
             :size (hash-table-size hash-table))))
    (loop for key being each hash-key of hash-table
       using (hash-value value)
       do (setf (gethash key ht) value)
       finally (return ht))))

(defclass tactus-interval (distribution)
  ((t0 :initarg :t0 :accessor t0)))
(defclass beat-deviation (distribution)
  ((subdivision :initarg :subdivision :reader subdivision)
   (phase :initarg :phase :reader beat-phase :initform 1)
   (p :initarg :p :reader p)))
(defclass bar-phase (distribution)
  ((duple :initarg :duple :reader duple)
   (triple :initarg :triple :reader triple)))
(defclass tactus-phase (distribution)
  ((zero :initarg :zero :reader zero
	 :documentation
	 "Probability of a phase of zero. Must be > 0 and <= 1.")))
(defclass onset (distribution)
  ((p :initarg :p :reader p)))

(defclass idyom-ppm (categorical)
  ((alphabet :reader alphabet :initform nil)
   (escape :reader escape :initform :c)
   (mixtures :reader mixtures :initform t)
   (update-exclusion :reader update-exclusion :initform nil)
   (order-bound :reader order-bound :initform nil)
   (category-models :reader category-models :initform (make-hash-table :test #'equal))
   (locations :reader locations :initform (make-hash-table)))
  (:documentation "PPM model that's also a jackdaw-native
 sequence model. The overridden fields serve to set some defaults."))

(defmethod probability ((d tactus-interval) arguments symbol)
  (let ((previous (car arguments)))
    (if (eq previous +inactive+)
	(elt (t0 d) (- symbol 9)) ;(min-tactus (model d)))
	(exp (- (expt (* 0.5 (- symbol previous )) 2))))))

(defmethod probability ((d onset) arguments symbol)
  (let ((p (elt (p d) (car arguments))))
    (if symbol p (- 1 p))))

(defmethod probability ((d bar-phase) arguments symbol)
  (let ((grouping (car arguments)))
    (case grouping
      (2 (elt (append (duple d) (list (- 1 (apply #'+ (duple d))))) symbol))
      (3 (elt (append (triple d) (list (- 1 (apply #'+ (triple d))))) symbol)))))

(defmethod probability ((d beat-deviation) arguments symbol)
  (if (eq symbol '*) 1
      (let* ((period (car arguments))
	     (center (beat-location period (subdivision d) (beat-phase d)))
	     (deviation (abs (- center symbol))))
	(elt (p d) deviation))))

(defmethod probability ((d tactus-phase) arguments symbol)
  (let ((interval (car arguments))
	(p-zero (zero d)))
    (if (eq symbol 0) p-zero
	(/ (- 1 p-zero) (1- interval)))))

(defun beat-location (period subdivision phase)
  (floor (/ (* phase period) subdivision)))

(defun beat-locations (max-beat-dev s period phase &optional (previous 0))
  (let ((center (beat-location period s phase)))
    (loop for p
       from (max (1+ previous) (- center max-beat-dev))
       to (min (- period (- s phase)) (+ center max-beat-dev))
       collect p)))

(defmodel temperley (generative-model)
  ((observed :initform '(:n))
   (tacti :initarg :tacti :reader tacti
	  :initform (loop for $t from 9 below 23 collect $t))
   (max-beat-dev :initarg :max-beat-dev :reader max-beat-dev :initform 0));3))
  ((one-shot :U (:^u) '(2 3))
   (one-shot :L (:^l) '(2 3))
   (recursive :T (:^t :^tph)
	      (if (eq (1+ $^tph) $^t) (tacti model) (list $^t))
	      (tacti model))
   (recursive :BPH (:^bph :tph :u)
	      (if (eq $tph 0)
		  (list (mod (1+ $^bph) $u))
		  (list $^bph))
	      (loop for bph below $u collect bph))
   (recursive :TPH (:^tph :^t :t)
	      (list (mod (1+ $^tph) $^t))
	      (loop for tph below $t collect tph))
   (recursive :DB (:^db :tph :t :l)
	      (cond ((eq $l 3) +singleton+)
		    ((eq $tph 0) (beat-locations (max-beat-dev model) 2 $t 1))
		    (t (list $^db)))
	      (if (eq $l 3) +singleton+ (beat-locations (max-beat-dev model) 2 $t 1)))
   (recursive :TB1 (:^tb1 :tph :t :l)
	      (cond ((eq $l 2) +singleton+)
		    ((eq $tph 0) (beat-locations (max-beat-dev model) 3 $t 1))
		    (t (list $^tb1)))
	      (if (eq $l 2) +singleton+ (beat-locations (max-beat-dev model) 3 $t 1)))
   (recursive :TB2 (:^tb2 :tb1 :tph :t :l)
	      (cond ((eq $l 2) +singleton+)
		    ((eq $tph 0) (beat-locations (max-beat-dev model) 3 $t 2 $tb1))
		    (t (list $^tb2)))
	      (if (eq $l 2) +singleton+ (beat-locations (max-beat-dev model) 3 $t 2 $tb1)))
   (v :BS (:db :tb1 :tb2 :tph :bph)
     (list (cond ((and (eq $tph 0) (eq $bph 0)) 3)
		 ((eq $tph 0) 2)
		 ((member $tph (list $db $tb1 $tb2)) 1)
		 (t 0))))
   (recursive :N (:^n) (list t nil) (list t)))
  ((bernouilli :U nil :symbols '(2 3) :p .24)
   (bernouilli :L nil :symbols '(2 3) :p .22)
   (tactus-interval :T (:^t) :t0 '(.1 .2 .3 .23 .13 .03 .006 .002 .001
				   .0006 .0002 .0001 .00005 .00005))
   (bar-phase :BPH (:u) :duple '(.65) :triple '(.33 .667))
   (tactus-phase :TPH (:t) :zero .6)
   (beat-deviation :DB (:t) :subdivision 2 :p '(.32 .24 .08 .02))
   (beat-deviation :TB1 (:t) :subdivision 3 :p '(.32 .24 .08 .02))
   (beat-deviation :TB2 (:t) :subdivision 3 :phase 2 :p '(.32 .24 .08 .02))
   (onset :N (:bs) :p '(.01 .48 .74 .95))))

(defmodel symbolic-temperley (generative-model)
  ((observed :initform '(:n))
   (tacti :initarg :tacti :reader tacti
	  :initform '(4 6 8 12))
   (max-beat-dev :initarg :max-beat-dev :reader max-beat-dev :initform 0));3))
  ((one-shot :U (:^u) '(2 3))
   (one-shot :L (:^l) '(2 3))
   (one-shot :T (:^t) (tacti model)) ; changed to one-shot
   (recursive :BPH (:^bph :tph :u)
	      (if (eq $tph 0)
		  (list (mod (1+ $^bph) $u))
		  (list $^bph))
	      (loop for bph below $u collect bph))
   (recursive :TPH (:^tph :^t :t)
	      (list (mod (1+ $^tph) $^t))
	      (loop for tph below $t collect tph))
   (v :BS (:t :l :tph :bph) ; $t and $l replace the beat locations
     (list (cond ((and (eq $tph 0) (eq $bph 0)) 3)
		 ((eq $tph 0) 2)
		 ((eq (mod $tph (/ $t $l)) 0) 1)
		 (t 0))))
   (recursive :N (:^n) (list t nil) (list t)))
  ((bernouilli :U nil :symbols '(2 3) :p .24)
   (bernouilli :L nil :symbols '(2 3) :p .22)
   (tactus-interval :T (:^t) :t0 '(.25 .25 .25 .25))
   (bar-phase :BPH (:u) :duple '(.65) :triple '(.33 .667))
   (tactus-phase :TPH (:t) :zero .6)
   (onset :N (:bs) :p '(.01 .48 .74 .95))))

;; Symbolic:
;; - limit tactus intervals
;; - zero beat deviation
;; - tactus interval may not change
;; 


(defmodel downbeat-distance (generative-model)
  ((observed :initform '(:B '(:IOI))) ;; observed :variable :input
   (ioi-domain :initarg :ioi-domain :reader ioi-domain
	       :initform '(1 2 3 4))
   (meter-domain :initarg :meter-domain :reader meter-domain
		 :initform '((2 3) (3 2))))
  ((one-shot :Meter (:^meter) (meter-domain model))
   (recursive :B (:^b :meter)
	      (loop for ioi in (ioi-domain model) collect
		   (+ (mod $^b (car $meter)) ioi))
	      (loop for phase below (car $meter) collect phase)
	      :observe (+ (mod $^b (car $meter)) @ioi)
	      :observe-init @ioi))
  ())

(defmodel test (generative-model)
  ()
  ((ngram-accumulator :X (:^X) '(a b c) '(a b c) 1))
  ())
