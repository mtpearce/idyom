(cl:in-package #:generative-models)

(defclass temporal (generative-model) ())

(defclass sequential ()
  ((order-bound :initarg :order-bound :accessor order-bound :initform nil)))

(defclass symbolic-temporal (temporal)
  ((resolution :initarg :resolution :accessor resolution :initform 16)))

(defclass idyom-temporal (symbolic-temporal)
  ((timebase :initarg :timebase :accessor timebase :initform 96)))

(defclass meter (symbolic-temporal)
  ((period-domain :accessor period-domain)
   (meter-domain :initarg :meter-domain :accessor meter-domain)
   (ioi-domain :initarg :ioi-domain :accessor ioi-domain)
   (observe-ioi :initarg :observe-ioi :accessor observe-ioi)
   (observe-period :initarg :observe-period :accessor observe-period)
   (observe-meter :initarg :observe-meter :accessor observe-meter)
   (observe-pickup :initarg :observe-pickup :accessor observe-pickup)))

(defclass temperley-meter (generative-model)
  ((observe-pip :initarg :observe-pip :accessor observe-pip)
   (observe-ut :initarg :observe-ut :accessor observe-ut)
   (observe-lt :initarg :observe-lt :accessor observe-lt)
   (observe-uph :initarg :observe-uph :accessor observe-uph)
   (observe-ti :initarg :observe-ti :accessor observe-ti)
   (ti-domain :initarg :ti-domain :accessor ti-domain)))
  
(defclass bar-position (meter)
  ((meter-period-domain :accessor meter-period-domain)))

(defclass downbeat-distance (meter) ())

(defclass bar-dist (bar-position idyom-temporal) ())

(defclass bar-position-cycles (bar-position) ())

(defclass phase-model (meter sequential)
  ((observe-ioi :initarg :observe-ioi :accessor observe-ioi)
   (phases :accessor phases)
   (ppm-escape :initarg :ppm-escape
	       :accessor ppm-escape :initform :c :initarg :ppm-escape)
   (ppm-update-exclusion? :initarg ppm-update-exclusion?
			  :accessor ppm-update-exclusion? :initform nil)
   (ppm-mixtures :initarg :ppm-mixtures
		 :accessor ppm-mixtures :initform t)
   (ppm-order-bound :initarg :ppm-order-bound
		    :accessor ppm-order-bound :initform nil)
   (multiple-observations? :allocation :class :initform t)) ; model property
  (:documentation "Probability of first onset is implicitly one. Since
observed feature IOI is not present in first event, a NIL symbol should be added
during testing. Disadvantage is that the number of phase interpretations quickly 
increases."))

(defclass empirical-phase-model (phase-model) ())

(defclass diatonic-tonal (generative-model)
  ((octave :initarg :octave :accessor octave :initform 12 :type integer)
   (modes :initarg :modes :accessor modes :initform '(0 9) :type (list integer))
   (observe-pitch :initarg :observe-pitch :accessor observe-pitch)
   (observe-key-signature :initarg :observe-key-signature
			  :accessor observe-key-signature)
   (observe-mode :initarg :observe-mode :accessor observe-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;                   ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; GENERATIVE-MODEL  ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;                   ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-domains ((model generative-model) dataset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;                  ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; IDYOM-TEMPORAL   ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;                  ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod multiplier ((model idyom-temporal))
  (/ (timebase model) (resolution model)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;                  ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; DIATONIC-TONAL   ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;                  ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod scale-degrees ((model diatonic-tonal))
  (loop for sd below (octave model) collect sd))

(defmethod key-signatures ((model diatonic-tonal))
  (loop for ks below (octave model) collect (- ks 5)))

(defmethod tonic ((model diatonic-tonal) pitch scale-degree mode)
  ;; Given that a note with PITCH is interpreted as SCALE-DEGREE of MODE,
  ;; return the tonic or key signature
  )

(defmethod tonal-features ((model diatonic-tonal))
  (f:featurelet event
      ((mode
	(recursive () () (list previous-mode)
		   () () (modes model)
		   previous-mode
		   (funcall (observe-mode model) event)))
       (scale-degree
	(normal () (mode) (scale-degrees model)
		(let* ((keysig
			(funcall (observe-key-signature model) event))
		       (pitch
			(funcall (observe-pitch model) event))
		       (tonic (if (> keysig 0)
				  (mod (+ (* keysig 7) mode)
				       (octave model))
				  (mod (+ (* (- keysig) 5) mode)
				       (octave model)))))
		  (mod (- pitch tonic) (octave model))))))
    (values mode scale-degree)))

(defmethod create-features ((model diatonic-tonal))
  (multiple-value-bind (mode scale-degree)
      (tonal-features model)
    (setf (latent model) (list mode))
    (f:set-model scale-degree 'models:idyom-ppm :conditioning (list mode))
    (list mode scale-degree)))

;      ;      ;      ;      ;      ;      ;      ;      ;   ;
;     ;     ;     ;     ;     ;     ;     ;     ;     ;     ;
;    ;    ;    ;    ;    ;    ;    ;    ;    ;    ;    ;    ;
;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;   ;
;  ;  ;  ;  ;  ;  ;  ;  ;  ;  ;  ;  ;  ;  ;  ;  ;  ;  ;  ;  ;
; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; 
;;;;;;;;;;;;; * ;;;;;;;;;;;;;;;;; * ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; * ;;;;;;;;;;;;          ;;;;;;;;;;;;;;; * ;;;;;;;;;;;;;
; * ;;;;;;; * ;;;;;; METER    ;; * ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;          ;;;;;;;;; * ;;;;;;;;; * ;;;;;;;
;; * ;;;;;;;;; * ;;;;;;;;;;;;;;;; * ;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; We are now assuming that meter and period are intrinsically
;; inseperable.
;;
;; Perhaps confusing: there are still separate observation functions
;; for meter and period.
;;
;; TODO: make dataset a field of a new class EMPIRICAL-MODEL.
;; Set domains in an :after method of INITIALIZE-INSTANCE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-domains ((model meter) dataset)
  "Set METER-DOMAIN and IOI-DOMAIN.

Assume that meter is observable in the first event and
does not change before the end of the sequence."
  (when (slot-boundp model 'meter-domain)
    (warn "Overriding manually configured meter domain."))
  (when (slot-boundp model 'ioi-domain)
    (warn "Overriding manually configured IOI domain."))
  (let ((meters) (iois))
    (sequence:dosequence (sequence dataset)
      (let ((meter (funcall (observe-meter model) (elt sequence 0)))
	    (period (funcall (observe-period model) (elt sequence 0))))
	(unless (member (list meter period) meters :test #'equal)
	  (push (list meter period) meters)))
      (sequence:dosequence (event sequence)
	(let ((ioi (funcall (observe-ioi model) event)))
	  (unless (eq 0 ioi)
	    (pushnew ioi iois)))))
    (setf (ioi-domain model) (sort iois #'<))
    (setf (meter-domain model) meters))
  (call-next-method))

(defmethod meter-features ((model meter))
  (f:featurelet event
      ((meter
	(recursive () () (list previous-meter)
		   () () (meter-domain model)
		   (if (multiple-observations? model)
		       (list previous-meter)
		       previous-meter)
		   (let ((meter (list (funcall (observe-meter model) event)
				      (funcall (observe-period model) event))))
		     (if (multiple-observations? model)
			 (list meter)
			 meter))))
       (period
	"PERIOD as encoded in METER."
	(normal () (meter) (list (cadr meter)))))
    (values meter period)))

(defmethod observe-pickup ((model meter))
  "Pickup is observed only in the first event. It corresponds
to its IOI."
  (observe-ioi model))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; DOWNBEAT-DISTANCE   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;; Implementation of the model described in chapter 4 of the            ;;
;; dissertation. Similar to frontiers (bar-dist), and the               ;;
;; bar-position-cycles model, but uses simplified downbeat distance.    ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defmethod downbeat-distance-features ((model downbeat-distance))
    (multiple-value-bind (meter period)
	(meter-features model)
      (flet ((params (previous period)
	       (loop for ioi in (ioi-domain model) collect
		    (+ (mod previous period) ioi))))
	(f:featurelet event
	    ((pickup
	      (recursive () (meter)
			 (list previous-pickup)
			 () (period)
			 (loop for bp below period collect bp)
			 previous-pickup
			 (funcall (observe-pickup model) event)))
	     (downbeat-distance
	      "Generate downbeat-distances based on IOI-DOMAIN."
	      (recursive (pickup) (meter period)
			 (params previous-downbeat-distance period)
			 (pickup) (meter period)
			 (params previous-pickup period)
			 (let ((ioi (funcall (observe-ioi model) event)))
			   (+ (mod previous-downbeat-distance period) ioi))
			 (let ((ioi (funcall (observe-ioi model) event)))
			   (+ (mod previous-pickup period) ioi)))))
	  (values meter period pickup downbeat-distance)))))

(defmethod create-features ((model downbeat-distance))
  (multiple-value-bind (meter period pickup downbeat-distance)
      (downbeat-distance-features model)
    (setf (latent model) (list meter pickup))
    (f:set-model meter 'models::dd-meter)
    (f:set-model downbeat-distance 'models::idyom-ppm
		 :conditioning (list meter))
    (list meter period pickup downbeat-distance)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; BAR-POSITION-CYCLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;; Bar position model generating bar-position, elapsed cycles tuples,   ;;
;; resulting in predictive distributions that map one-to-one to points  ;;
;; in time.                                                             ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defmethod bar-position-cycles-features ((model bar-position-cycles))
  (labels ((bar-position-and-cycles (previous-bar-position ioi period)
	     (multiple-value-bind (cycles bar-position)
		 (truncate (+ previous-bar-position ioi) period)
	       (list bar-position cycles))))
    (multiple-value-bind (meter period)
	(meter-features model)
      (f:featurelet event
	  ((pickup
	    "[Latent] Generate a pickup once.

Mirrors first bar position on initialisation.

Plausible if generated pickup matches observed pickup.
Latent feature."
	    (recursive () (bar-position-cycles) (list previous-pickup)
		       () (bar-position-cycles) (list bar-position-cycles)
		       (funcall (observe-pickup model) event)))
	   (bar-position-cycles
	    "Generate bar position and elapsed cycles based on IOI-DOMAIN."
	    (recursive () (meter period)
		       (loop for ioi in (ioi-domain model) collect
			    (bar-position-and-cycles (car previous-bar-position-cycles)
						     ioi period))
		       () (meter period)
		       (loop for ioi in (cons 0 (ioi-domain model)) collect
			    (bar-position-and-cycles 0 ioi period))))
	   (bar-position-cycles-observer
	    "[Observe] Mirror BAR-POSITION-CYCLES.

Plausible if observed IOI matches current BAR-POSITION-CYCLES given the previous
BAR-POSITION-CYCLES. Observer feature necessary to work with IOI observations?"
	    (normal (bar-position-cycles) (bar-position-cycles period)
		    (list bar-position-cycles)
		    (let ((ioi (funcall (observe-ioi model) event)))
		      (bar-position-and-cycles
		       (car previous-bar-position-cycles) ioi period)))))
	(values meter period pickup bar-position-cycles
		bar-position-cycles-observer)))))

(defmethod create-features ((model bar-position-cycles))
  (multiple-value-bind (meter period pickup bar-position-cycles
					  bar-position-cycles-observer)
      (bar-position-cycles-features model)
    (setf (latent model) (list meter pickup))
    (f:set-model bar-position-cycles 'models::idyom-ppm
		 :conditioning (list meter))
    (list meter period pickup bar-position-cycles
	    bar-position-cycles-observer)))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; BAR-DIST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Re-implementation of old bar-position x bar-dist model that was
;; presented in Van der Weij et al. (2017).
;;
;; The model is conceptually similar to BAR-POSITION-CYCLES, but 
;; there is a seperate feature that generates the pickup. 
;;
;; Bardist was implemented as follows: Old bar-dist:
;; Calculate barnum of current event. If this is the first event
;; return its barnum.
;; If not, calculate barnum of last before current and return
;; barnum current - barnum last
;;
;; So, to translate to an IOI based model:
;; For the first IOI, calculate the bar-dist from time zero.
;; For subsequent iois, count the number of bar crossings.
;;
;; While equivalent to the old model, this model, unlike the old
;; model, is based on IOIs, rather than absolute onsets.
;; Some jumping through hoops is required to achieve this.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defmethod bar-dist-features ((model bar-dist))
  (labels ((get-phase-bar-dist (previous-phase ioi barlength)
	     (multiple-value-bind (bar-dist phase)
		 (truncate (+ previous-phase ioi) barlength)
	       (list phase bar-dist)))
	   (generate-phase-bar-dists
	       (previous-phase barlength &optional (iois (ioi-domain model)))
	     (loop for ioi in iois collect
		  (get-phase-bar-dist previous-phase ioi barlength))))
    (f:featurelet event
	((phase-meter
	  "[Latent] Generate all meters in all possible phases.

The possible phases of a meter are the integers in the interval (0, PERIOD]."
	  (recursive
	   () () (list previous-phase-meter)
	   () ()
	   (let ((phase-meters))
	     (dolist (meter (meter-domain model) phase-meters)
	       (dotimes (pos (/ (cadr meter) (multiplier model)))
		 (push (list (* pos (multiplier model)) meter)
		       phase-meters))))
	   previous-phase-meter
	   (let ((meter (list (funcall (observe-meter model) event)
			      (funcall (observe-period model) event))))
	     (list 0 meter))))
	 (phase-bar-dist
	  "[Observe] Based on IOI-DOMAIN, generate the parameters of the
PHASE-BAR-DIST distribution."
	  (recursive
	   ;; Recursive function
	   () (t0 barlength meter)
	   (generate-phase-bar-dists (car previous-phase-bar-dist) barlength
				     ;; BEWARE: allowing zero iois for compatibility
				     ;; old model
				     (cons 0 (ioi-domain model)))
	   ;; Initialisation function
	   () (t0 barlength meter)
	   (generate-phase-bar-dists t0 barlength
				     (cons 0 (ioi-domain model)))
	   ;; Recursive observation function
	   (let ((interval (funcall (observe-ioi model) event)))
	     (get-phase-bar-dist (car previous-phase-bar-dist) interval
				 barlength))
	   ;; Initialisation observation function
	   (let ((interval (funcall (observe-ioi model) event)))
	     (get-phase-bar-dist t0 interval barlength))))
	 ;; For convenience in analysis and feature definitions
	 (t0
	  "Calculate the first bar-position based on the phase encoded in
PHASE-METER (PHASE in old terminology)."
	  (normal
	   () (phase-meter)
	   (multiple-value-bind (phase meter)
	       (values-list phase-meter)
	     (let ((barlength (cadr meter)))
	       (list (mod (- barlength phase) barlength))))))
	 (meter
	  "METER-PERIOD as encoded in PHASE-METER."
	  (normal () (phase-meter) (list (cadr phase-meter))))
	 (barlength
	  "PERIOD as encoded in METER (BARLENGTH for continuity
with old terminology)."
	  (normal () (meter) (list (cadr meter)))))
      (values meter barlength phase-meter t0 phase-bar-dist))))

(defmethod create-features ((model bar-dist))
  (multiple-value-bind (meter barlength phase-meter
	      t0 phase-bar-dist)
      (bar-dist-features model)
    (setf (latent model) (list phase-meter))
    ;;(f:set-model phase-meter #'models::make-old-phase-metre-prior)
    (f:set-model phase-bar-dist 'models:idyom-ppm
		 :conditioning (list barlength meter))
    (list meter barlength phase-meter t0 phase-bar-dist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; PHASE-MODEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Same as bar-position, but with different representation.

(defmethod scale-ioi ((model phase-model) time period)
  (* (/ time period) (phases model)))

(defmethod set-phase-based-params ((model phase-model) phases)
  (setf (phases model) phases)
  (setf (f::model-args (feature model :phase))
	(list :phases phases :order-bound (order-bound model))))

(defmethod set-domains :after ((model phase-model) dataset)
  (let* ((periods (mapcar #'cadr (meter-domain model)))
	 (phases (apply #'lcm periods)))
    (set-phase-based-params model phases)))

(defmethod printable ((model phase-model))
  (list :phases (phases model)
	:meter-domain (meter-domain model)
	:model (call-next-method)))

(defmethod from-printable (printable (model phase-model))
  (let ((phases (getf printable :phases))
	(model-printable (getf printable :model)))
    (setf (meter-domain model) (getf printable :meter-domain))
    (call-next-method model-printable model)
    (set-phase-based-params model phases)))

(defmethod phase-model-features ((model phase-model))
  "Attributes used: ioi, pickup, meter, period"
  (multiple-value-bind (meter)
      (meter-features model)
    (f:featurelet event
	((phase
	  "Generate phases.

Generate all phases below the LCM of periods.

Latent feature."
	  (normal () (meter)
		  ;; Generate all phases
		  (loop for phase below (phases model) collect phase)
		  ;; make only phases that fit the grid plausible
		  ;; (may be called a hack)
		  (let ((period (cadr meter)))
		    (loop for bar-position below period collect
			 (* bar-position (/ (phases model) period))))))
	 (phase-observer
	  "Mirror phase.

Plausible if PREVIOUS-PHASE + IOI modulo PERIOD match PHASE. Observer necessary to 
work with IOI observations"?
	  (normal (phase) (phase meter)
		  (list phase) ; possibility
		  (let* ((period (cadr meter)) ; plausibility
			 (ioi (funcall (observe-ioi model) event))
			 (scaled-ioi (scale-ioi model ioi period)))
		    (list (mod (+ previous-phase scaled-ioi) (phases model))))))
	 (pickup
	  "Mirror initial phase.

Latent feature that can be observed in training to generate correct interpretation.

Plausible if initial PHASE matches observed pickup."
	  (recursive () (phase meter) (list previous-pickup)
		     () (phase meter) (list phase) 
		     (list previous-pickup)
		     (let ((period (cadr meter))
			   (pickup (funcall (observe-pickup model) event)))
		       (list (mod (scale-ioi model pickup period) (phases model)))))))
      (values pickup meter phase phase-observer))))
    
(defmethod create-features ((model phase-model))
  (multiple-value-bind (pickup meter phase phase-observer)
      (phase-model-features model)
    (setf (latent model) (list pickup meter))
    (f:set-model meter 'models::categorical-once)
    (f:set-model phase 'models::ppm-phase-model 
		 :conditioning (list meter)
		 :model-args (list :update-exclusion? (ppm-update-exclusion? model)
				   :escape (ppm-escape model)
				   :mixtures (ppm-mixtures model)
				   :order-bound (ppm-order-bound model)))
    (list pickup meter phase phase-observer)))

(defclass idyom-phase-model (phase-model)
  ((observe-barlength :initarg :observe-barlength :accessor observe-barlength)
   (observe-pulses :initarg :observe-pulses :accessor observe-pulses)
   (observe-bioi :initarg :observe-bioi :accessor observe-bioi))
  (:documentation "Phase model that works with IDyOM's CHARM 
music representation by implementing PHASE-MODEL's observation
functions in terms of CHARM music attributes."))


(defmethod observe-ioi ((model idyom-phase-model))
  (observe-bioi model))

(defmethod observe-pickup ((model idyom-phase-model))
  "Pickup is observed only in the first event. It corresponds
to its BIOI."
  (observe-ioi model))

(defmethod observe-meter ((model idyom-phase-model))
  (observe-pulses model))

(defmethod observe-period ((model idyom-phase-model))
  (observe-barlength model))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;                 ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; EMPIRICAL-PHASE ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;                 ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod empirical-phase-features ((model empirical-phase-model))
  (multiple-value-bind (pickup period meter phase phase-observer)
      (phase-model-features model)
    (f:featurelet event
	((pickup-phase
	  "Generate 0 if PREVIOUS-PICKUP-PHASE is 0, 
otherwise mirror PHASE.

Plausible if PREVIOUS-PICKUP-PHASE is 0, or if PREVIOUS-PHASE +
SCALED-IOI matches PHASE or (PHASES MODEL). The difference with 
PHASE-OBSERVER is that PICKUP-PHASE does not perform the modulo 
operation.

PICKUP-PHASE is becomes implausible when PHASE skips over the first 
downbeat implied by the given interpretation."
	  (recursive () (phase period)
		     (if (eq previous-pickup-phase 0)
			 '(0)
			 (list phase))
		     () (phase period) (list phase)
		     (if (eq previous-pickup-phase 0)
			 0
			 (let* ((ioi (funcall (observe-ioi model) event))
				(scaled-ioi (scale-ioi model ioi period))
				(next (+ previous-pickup-phase scaled-ioi)))
			   (if (= next (phases model)) 0 next)))
		     phase)))
      (values pickup pickup-phase period meter phase phase-observer))))

(defmethod create-features ((model empirical-phase-model))
  (multiple-value-bind (pickup pickup-phase period meter phase phase-observer)
      (empirical-phase-features model)
    ;; Features that *can* be observed, but must be hidden must be set as latent
    (setf (latent model) (list pickup period meter))
    ;(f:set-model phase (lambda (args) (models:make-ppm args)) :model-args (list meter))
    ;;:model-args (list meter))
    (list pickup pickup-phase period meter phase phase-observer)))

;; TODO: tonal and metrical

(defun beat-locations (s period phase &optional (previous 0))
  (let ((center (floor (/ (* phase period) s)))
	(max-deviation 4))
    (loop for p
       from (max (1+ previous) (- center max-deviation))
       to (min (- period (- s phase)) (+ center max-deviation)))))

(defmethod temperley-meter-features ((model temperley-meter))
  (f:featurelet event
      ((u "Upper level (L3)." (one-shot () () '(2 3)))
       (l "Lower level (L1)." (one-shot () () '(2 3)))
       (period "Tactus interval"
	       (recursive (tph) ()
			  (if (eq (1+ previous-tph) previous-period)
			      (tacti model)
			      (list previous-period))
			  (tacti model)))
       (bph "Bar level (L3) phase with respect to tactus level (L2)."
	    (recursive () (tph u)
		       (if (eq tph 0)
			   (mod (1+ previous-bph) u)
			   (list previous-bph))
		       (loop for bph below u collect bph)))
       (db "Duple beat positions"
	   (recursive () (period l tph)
		      (cond
			((eq l 3) +singleton)
			((eq tph 0) (beat-locations 2 period 1))
			(t (list previous-db)))
		      (beat-locations 2 period 1)))
       (tb1 "First triple beat positions"
	    (recursive () (period l tph)
		       (cond
			 ((eq l 2) +singleton)
			 ((eq tph 0) (beat-locations 3 period 1))
			 (t (list previous-db)))
		       (beat-locations 3 period 1)))
       (tb2 "First triple beat positions"
	    (recursive () (tb1 period l tph)
		       (cond
			 ((eq l 2) +singleton)
			 ((eq tph 0) (beat-locations 3 period 2 tb1))
			 (t (list previous-db)))
		       (beat-locations 3 period 2 tb1)))
       (tph "Tactus phase."
	    (recursive (period) (period)
		       (mod (1+ previous-tph) period)
		       (period) (period)
		       (loop for tph below period collect tph)))
       (bs "Beat salience."
	   (normal () (ut lt uph pip ti)
		   (multiple-value-bind (b p)
		       (truncate pip ti)
		     (list (if (eq p 0)
			       (if (eq (mod (+ b uph) ut) 0) 3 2)
			       (if (eq (mod p (/ ti lt)) 0) 1 0))))))
       (n "Note onset?"
	  (recursive () (bs) (list t nil) (list t))))
    (values period tph bph db tb1 tb2 bs n)))

(defmethod create-features ((model temperley-meter))
  (multiple-value-bind (pip ut lt uph ti tph n-obs a bs n)
      (temperley-meter-features model)
    ;;(f:set-model ut 'models::bernouilli :model-args '(:init-uniform? t))
    ;;(f:set-model lt 'models::bernouilli :model-args '(:init-uniform? t))
    ;;(f:set-model a 'models::bernouilli :model-args '(:init-uniform? t))
    ;;(f:set-model uph 'models::categorical :conditioning (list ut)
		;; :model-args '(:init-uniform? t))
    (f:set-model n 'models::bernouilli :conditioning (list bs)
		  :model-args '(:init-uniform? t))
    (list pip ut lt uph ti tph n-obs a bs n)))
