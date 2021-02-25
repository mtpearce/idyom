;;;; A glimpse of what could be

;; "mirror" features define plausibility constraints.
;; "global" features are features that are generated once at the beginning
;; and subsequently only generate their previous value.
;; All words between quotes in the above are up for terminology improvement.
;;
;; Mechanisms required:
;;
;; Specifying
;; 
;; - a model's variables
;; - which are inherited
;; - which are latent
;; - a variable's model
;; - a variable's generative function (top-down)
;; - a variable's plausibility constraints (bottom-up)
;; - a variable's "input" / observable features
;;
;; What is needed for this?
;;
;; Separating generative models from feature graphs?
;;
;; A macro building on defclass for defining models
;; A macros that define different kinds of features (all building on normal and
;; recursive).
;; A macro for defining
;; Macros:
;; *global* unpacks into a recursive feature definition which generates a domain
;; and recursively recreates the value it first generated
;; *mirror* inpacks into a normal feature definition which simply mirrors the value
;; of its argument
;; A feature is recursive if generate OR observe is recursive.
;; If generate XOR observe is recursive, the non-recursive function is
;; extended into a recursive one by simply copying its function to the initialisation
;; step.
;;
;; A variables dependencies are the union of its model, generate and observe dependencies.

(defmodel meter ()
  ()
  ()
  (:input pickup ioi))

(defmodel phase-lcm-model (meter-model)
  ((phases :accessor phases
	   :initialisation (lambda (m) (apply #'lcm (domain period)))))
  (:variables meter phase pickup) ; meter inherited from parent model
  (:input pickup ioi) ;
  ((phase
    :model 'models:ppm-model () &key order-bound (mixtures t)
    :possibility
    (normal (phase) (meter)
	    
	    (let* ((period (cadr meter)))
	      (loop for ioi below (phases model)
		 collect 
		   (multiple-value-bind (cycles phase)
		       (truncate (+ previous-phase ioi) period)
		     (list phase cycles)))))
    :plausibility
    (normal (phase) (meter)
	    (let* ((period (cadr meter))
		   (ioi (mod (funcall (observe-ioi model) event)
			     (phases model))))
	      (multiple-value-bind (cycles phase)
		  (truncate (+ previous-phase ioi) period)
		(list (list (phase cycles))))))
   (pickup
    "Possible if first PICKUP matches first PHASE.
Plausible if first PICKUP matches observed pickup."
    :possibility (global () (phase) (list phase))
    :latent t
    :plausibility
    (normal () (phase meter)
	    (let ((period (cadr meter))
		  (pickup (funcall (observe-pickup model) event)))
	      (list (mod (scale-ioi model pickup period) (phases model))))))))

(defmodel phase-model (meter-model)
  ((phases :accessor phases
	   :initialisation (lambda (m) (apply #'lcm (domain period)))))
  (:inherit meter) ; features inherited from parent model
  (:input period meter)
  ((phase
    :model 'models:phase-model (meter) &key order-bound (mixtures t)
    :generate
    (normal () ()
	    (loop for phase below (phases model) collect phase))
    :observe
    (normal () (meter)
	    (let ((period (cadr meter)))
	      (loop for bar-position below period collect
		   (* bar-position (/ (phases model) period))))))
   (phase-observer
    "Plausible if PREVIOUS-PHASE + IOI modulo PERIOD match PHASE."
    :generate (mirror phase)
    :observe
    (normal (phase) (meter)
	    (let* ((period (cadr meter))
		   (ioi (funcall (observe-ioi model) event))
		   (scaled-ioi (scale-ioi model ioi period)))
	      (list (mod (+ previous-phase scaled-ioi) (phases model))))))
   (pickup
    "Plausible if initial PHASE matches observed pickup."
    :generate (global () (phase) (list phase))
    :latent t
    :observe
    (normal () (phase meter)
	    (let ((period (cadr meter))
		  (pickup (funcall (observe-pickup model) event)))
	      (list (mod (scale-ioi model pickup period) (phases model))))))))
  
  

(defmodel meter (symbolic-temporal)
  ;; Observable attributes.
  ;; This makes (observe attribute) and (domain attribute)
  ;; available in feature definitions.
  (period meter)
  ((meter
    "Generate METER-DOMAIN."
    (global () () (meter-domain model)
	    :latent t
	    :model models:zeroth-order
	    :plausibility
	    (observe meter)
  (:documentation "lalala"))

(defmodel phase-model (meter)
  ((phases :accessor phases)
   (multiple-observations? :initform t))
  :inherited-features '(meter)
  (phase
   (model-with models:phase-model (meter)
	       &key order-bound (mixtures t))
   (normal () (meter)
	   (loop for phase below (phases model) collect phase)
	   :plausibility
	   (let ((period (cadr meter)))
	     (loop for bar-position below period collect
		  (* bar-position (/ (phases model) period))))))
  (phase-observer
   "Plausible if PREVIOUS-PHASE + IOI modulo PERIOD match PHASE."
   (mirror (phase)
	   (phase) (meter)
	   :plausibility
	   (let* ((period (cadr meter))
		  (ioi (funcall (observe-ioi model) event))
		  (scaled-ioi (scale-ioi model ioi period)))
	     (list (mod (+ previous-phase scaled-ioi) (phases model))))))
  (pickup
   "Plausible if initial PHASE matches observed pickup."
   (global () (phase meter) (list phase)
	   :latent t
	   :plausibility
	   (let ((period (cadr meter))
		 (pickup (funcall (observe-pickup model) event)))
	     (list (mod (scale-ioi model pickup period) (phases model)))))))

(defmodel empirical-phase-model (phase-model)
  ()
  ((pickup-phase
    "Generate 0 if PREVIOUS-PICKUP-PHASE is 0, 
otherwise mirror PHASE.

Plausible if PREVIOUS-PICKUP-PHASE is 0, or if PREVIOUS-PHASE +
SCALED-IOI matches PHASE or (PHASES MODEL).

The effect is that PICKUP-PHASE is becomes implausible as soon as
phase skips the downbeat implied by a given interpretation."
    (recursive () (phase period)
	       (if (eq previous-pickup-phase 0)
		   '(0)
		   (list phase))
	       () (phase period) (list phase)
	       :plausibility
	       (if (eq previous-pickup-phase 0)
		   0
		   (let* ((ioi (funcall (observe-ioi model) event))
			  (scaled-ioi (scale-ioi model ioi period))
			  (next (+ previous-pickup-phase scaled-ioi)))
		     (if (= next (phases model)) 0 next)))
	       :initial-plausibility phase))))

(make-instance 'phase-model :phase-model-order-bound 0)

;; Constraints are higher order functions that values of variables they're
;; conditioned on as arguments.
;; Keywords are nice, but how to access arguments to constraints?
;;   (v-arg :kw) and (h-arg :kw) not very elegant.
;;   but symbols have constraints: t cannot be used
;;   making defining the order and using lambda functions with
;;   positional arguments is not great either.


(defmodel temperley
    (list :U :L
	  :T (list :^tph :tph)
	  :BPH (list :^bph :tph :u)
	  :TPH (list :^TPH :^T :T)
	  :DB (list :^DB :TPH :T :L)
	  :TB1 (list :^TB1 :TPH :T :L)
	  :TB2 (list :^TB2 :TB1 :TPH :T :L)
	  :BS (list :DB :TB1 :TB2 :TPH :BPH)
	  :N (list :^N :BS))
    :domains
    (list :u (list 2 3)
	  :l (list 2 3)
	  :t (loop for t from 9 below 23 collect t)
	  :n (list t nil))
    :constraints
          :u (one-shot) ;; absence of constraint means domain
	  :l (one-shot) ;; idem
	  :t (recursive (if (eq (1+ $^tph) $^t)
			    (domain :t model)
			    (list $^t)))
	  ;; absence of initialization indicates domain
	  :bph (recursive (if (eq $tph 0)
			      (mod (1+ $^bph $u))
			      (list $^bph))
			  (loop for bph below $u collect bph))
	  :db (recursive (cond ((eq $l 3) +singleton+)
			       ((eq $tph 0) (beat-locations 2 period 1))
			       (t (list $db)))
			 (beat-locations 2 period 1))
	  :tb1 (recursive (cond ((eq $l 2) +singleton)
				((eq $tph 0) (beat-locations 3 period 1))
				(t (list $^tb1)))
			  (beat-locations 3 period 1))
	  :tb2 (recursive (cond ((eq $l 2) +singleton)
				((eq $tph 0) (beat-locations 3 period 2 $tb1))
				(t (list $^tb2)))
			  (beat-locations 3 period 2 $tb1))
	  :tph (recursive (mod (1+ $^tph) period)
			  (loop for tph below period collect tph))
	  :bs (variable (cond ((and (eq $tph 0) (eq $bph 0)) 3)
			 ((eq $tph 0) 2)
			 ((member $tph (list $db $tb1 $tb2)))
			 (t 0)))
	  :n (recursive (list t nil) (list t)))






