(cl:in-package #:mvs)

;;; Test the abstract mvs; does it load the models correctly etc.
;;; Test the generative mvs; does model-sequence produce the right results (compare to results produced by previous implementation

;;; Test the models on toy domains:
;;; - two metres with two or three phases
;;; - a generative viewpoint that decodes alphabet substitution codes and a language model

(5am:def-suite abstract-mvs)
(5am:in-suite abstract-mvs)

(5am:def-fixture toy-dataset ()
  
  (let* ((metre (lv:get-latent-variable 'metre))
	 (metres '((2 1) (3 1)))
	 (categories (mapcar (lambda (m)
			       (apply #'lv::create-category
				      (append (list metre)
				      (utils:make-plist '(:barlength :pulses) m))))
			     metres))
	 (rhythms '(((0 2 1 1 2 2)
		     (0 1 1 1 1 2 2)
		     (0 3 1 1 1 2)
		     (0 1 2 2 1 1 2 2))
		    ((2 1 2 1 2 1 3)
		     (0 3 1 1 1 3 3)
		     (2 1 1 1 1 2 1 1 1 1 3))))
	 (event-sequences (mapcar (lambda (r m)
				    (mapcar (lambda (s)
					      (viewpoints::iois->onset-events s
									      :barlength
									      (first m)
									      :pulses
									      (second m)))
					    r))
				  rhythms metres)))
	   (&body)))

;(5am:def-fixture validation-set ()
;;  (let ((utils:load-object-from-file ))))

(defmacro with-adjusted-phase ((event-sequence period phase) &body body)
  `(progn
     (dolist (e ,event-sequence)
       (setf (md:onset e) (+ (mod (- ,period ,phase) ,period)
			     (md:onset e))))
     ,@body
     (dolist (e ,event-sequence)
       (setf (md:onset e) (- (md:onset e) (mod (- ,period ,phase) ,period))))))

(5am:test viewpoints-and-alphabets
  (let ((bioi-alphabet (0 1 2 3)
  
(5am:test abstract-mvs
  (5am:with-fixture toy-dataset ()
    (flet ((interpret-rhythm (rhythm training-viewpoint)
	     (viewpoints:viewpoint-sequence training-viewpoint rhythm)))
      (let* ((targets (viewpoints:get-viewpoints '(onset)))
	     (reference-sources (viewpoints:get-viewpoints '((posinbar bardist-train))))
	     (latent-variables (lv:get-latent-variables '(metre)))
	     (sources (viewpoints:get-viewpoints '((bardist metpos))))
	     (training-viewpoints (mapcar (lambda (vp)
					    (viewpoints:get-viewpoint
					     (viewpoints:training-viewpoint vp)))
					  sources))
	     (latent-variable (lv:get-latent-variable 'metre))
	     (abstract-ltms (loop for vp in training-viewpoints collect
				 (loop for rhythm-set in event-sequences 
				      for category in categories collect
				      (cons category
					    (ppm:build-model (mapcar
							      #'(lambda (r)
								  (interpret-rhythm r vp))
							      rhythm-set)
							     nil)))))
	     (ltms (loop for rhythm-set in event-sequences 
		      for category in categories collect
			(loop for vp in reference-sources collect
			     (ppm:build-model (mapcar
					       #'(lambda (r)
						   (interpret-rhythm r vp))
					       rhythm-set)
					      nil))))
	     (event-sequence (viewpoints::iois->onset-events '(0 3 2 1 3 3))))
	;; Set the the required BIOI alphabet
	(setf (viewpoints:viewpoint-alphabet (viewpoints:get-viewpoint 'bioi))
	      '(0 1 2 3))
	  ;; For each viewpoint
	(loop for vp in sources
	   for lv in latent-variables do
	     (progn
	       ;; Set latent-variable (per viewpoint lvs required for get-short-term-models)
	       (setf (viewpoints:latent-variable vp) lv)
	       ;; Set categories (for now assuming they all use the same categories)
	       ;; (required for viewpoint alphabet)
;;	       (setf (lv:categories lv) categories)
	       ;; Set the viewpoint alphabet for each category
	       (loop for category in categories do
		    (lv:with-latent-category (category lv)
		      (setf (viewpoint-alphabet vp) (range (lv::get-category-parameter
							    category :barlength lv)))))))
	;; Set categories for mvs-latent-variable
	(setf (lv:categories latent-variable) categories)
	(let ((abstract-mvs (make-mvs targets sources abstract-ltms
				      :class 'abstract-mvs
				      :class-args
				      (list :latent-variable latent-variable
					    :viewpoint-latent-variables
					    (apply #'vector latent-variables))))
	      (mvss (mapcar (lambda (ltm) (make-mvs targets reference-sources ltm)) ltms)))
	  (loop for category in categories
	     for mvs in mvss do
	       (let* ((barlength (lv::get-category-parameter category :barlength
							     latent-variable))
		      (pulses (lv::get-category-parameter category :pulses
							  latent-variable))
		      (mvs::*models* :ltm)
		      (latent-states (lv:get-latent-states category latent-variable)))
		 (dolist (latent-state latent-states)
		   (lv:with-latent-variable-state (latent-state latent-variable)
		     (dolist (e event-sequence)
			 (setf (md:barlength e) barlength)
			 (setf (md:pulses e) pulses))
		     ;;(print (viewpoints:viewpoint-sequence (first sources) event-sequence))
		     (let ((abstract-results (model-sequence abstract-mvs event-sequence
							     :predict? t))
			   (phase-corrected event-sequence))
		       (with-adjusted-phase (phase-corrected barlength
							     (car (last latent-state)))
			 (let ((mvs-results (model-sequence mvs phase-corrected
							    :predict? t)))
			   (flet ((event-probabilities (prediction-sets)
				    (let* ((predictions (first prediction-sets))
					   (event-predictions (event-predictions predictions))
					   (probabilities 
					    (mapcar #'cdr event-predictions)))
				      ;; The first prediction may diverge due to differences
				      ;; in the alphabet derived from BIOI which is
				      ;; transformed with the abstract viewpoint in the
				      ;; abstract-mvs but remains unaltered in the normal
				      ;; case.
				      (cdr probabilities))))
			     (5am:is (every (lambda (pair) (apply #'equal pair))
					    (mapcar #'list
						    (event-probabilities abstract-results)
						    (event-probabilities mvs-results))))))))
		     (dolist (mvs (list abstract-mvs mvs))
		       (operate-on-models mvs #'increment-sequence-front)
		       (operate-on-models mvs #'reinitialise-ppm :models 'stm)))))))))))

	  
    
    
