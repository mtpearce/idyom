(cl:in-package #:latent-variables)

(5am:def-suite latent-variables)
(5am:in-suite latent-variables)

;; A latent variable with multiple interpretations per category, like metre.
(define-latent-variable latent1 (:barlength :pulses) (:phase :barlength))

;; The latent states of a category (a metre) correspond to the different
;; possible phases of that category.
(defmethod get-latent-states (category (v latent1))
  (let ((barlength (get-category-attribute category :barlength v)))
    (loop for phase below barlength collect
	 (create-latent-state v category :phase phase))))

;; Create a variant of latent1 in which the prior for phase and meter is
;; calculated jointly with an empirical, nonuniform, distribution of phase
(defclass latent1-empirical (latent1) ())

(defmethod get-prior-distribution (training-data categories
				   (v latent1-empirical))
  (let* ((category-counts (mapcar #'length training-data))
	 (observation-count (apply #'+ category-counts))
	 (distribution))
    (loop for category in categories
       for category-count in category-counts
       for training-set in training-data do
	 (let* ((category-rel-freq (/ category-count observation-count))
		(phases training-set) ; dummy training-sets used here consist of numbers
		(phase-counts (utils:count-frequencies phases #'<)))
	   (loop for phase-count in phase-counts do
		(let ((phase (car phase-count))
		      (count (cdr phase-count)))
		  (let ((phase-rel-freq (/ count category-count)))
		    (push (cons (create-latent-state v category
						      :phase phase)
				(* category-rel-freq phase-rel-freq))
			  distribution))))))
    distribution))

;; The latent states of the empirical version of latent1 depend on the observed
;; phases
(defmethod get-latent-states (category (v latent1-empirical))
  (loop for latent-state in (mapcar #'car (prior-distribution v))
     when (equal (get-category latent-state v) category)
       collect latent-state))

;; A latent variable whose interpretations are all part of a single category
;; (like key-signature)
(define-latent-variable latent2 () (:keysig))

(defmethod get-latent-states (category (v latent2))
  (loop for keysig in (utils:generate-integers 0 11) collect
       (create-latent-state v category :keysig keysig)))

;; A latent variable whose categories do not impose interpretations
(define-latent-variable latent3 (:style) ())

(defmethod get-event-category (event (v latent3))
  (list (md:description event)))
       
(defun plist-equal (a b &key (test #'eq))
  (flet ((keys (plist)
	   (loop for i below (length plist) if (evenp i) collect (elt plist i))))
    (and (every (lambda (key)
		  (apply test (list (getf a key) (getf b key))))
		(keys a))
	 (every (lambda (key)
		  (apply test (list (getf a key) (getf b key))))
		(keys b)))))

;;; The define-latent-variable macro should define a subclass of
;;; the latent variable clas with the provided name.
(5am:test latent-variable-definition
  (loop for class in '(latent1 latent2 latent3) do
       (5am:is (not (null (find-class class :errorp nil))))
       (let ((latent-variable (make-instance class)))
	 (5am:is (typep latent-variable 'latent-variable)))))

;; For each latent variable, a specialised method should be defined
;; which returns an alphabetically sorted list of the category attributes
;; provided to the define-latent-variable macro.
;; A method specialized on linked latent variables should return an alphabetically
;; sorted list containing the category attributes of the constituent latent variables.
(5am:test category-attributes 
  (let* ((latent1 (make-instance 'latent1))
	 (latent2 (make-instance 'latent2))
	 (latent3 (make-instance 'latent3))
	 (linked (make-instance 'linked :links (list latent1 latent2 latent3))))
    (5am:is (utils:set-equal (category-attributes latent1) '(:barlength :pulses)))
    (5am:is (utils:set-equal (category-attributes latent2) nil))
    (5am:is (utils:set-equal (category-attributes latent3) '(:style)))
    (5am:is (utils:set-equal (category-attributes linked) '(:barlength :pulses :style)))))

;; For each latent variable, a specialised method should be defined
;; which returns an alphabetically sorted list of the interpretation attributes
;; provided to the define-latent-variable macro.
;; A method specialized on linked latent variables should return an alphabetically
;; sorted list containing the interpretation attributes of the constituent latent
;; variables.
(5am:test interpretation-attributes 
  (let* ((latent1 (make-instance 'latent1))
	 (latent2 (make-instance 'latent2))
	 (latent3 (make-instance 'latent3))
	 (linked (make-instance 'linked :links (list latent1 latent2 latent3))))
    (5am:is (utils:set-equal (interpretation-attributes latent1) '(:barlength :phase)))
    (5am:is (utils:set-equal (interpretation-attributes latent2) '(:keysig)))
    (5am:is (utils:set-equal (interpretation-attributes latent3) nil))
    (5am:is (utils:set-equal (interpretation-attributes linked)
			     '(:barlength :keysig :phase)))))

;; The latent-state-attributes method should return the an alphabetically
;; sorted list of the union between the category attribute set and the
;; interpretation attribute set. 
(5am:test latent-state-attributes
  (let* ((latent1 (make-instance 'latent1))
	 (latent2 (make-instance 'latent2))
	 (latent3 (make-instance 'latent3))
	 (linked (make-instance 'linked :links (list latent1 latent2 latent3))))
    (5am:is (utils:set-equal (latent-state-attributes latent1)
			     '(:barlength :phase :pulses)))
    (5am:is (utils:set-equal (latent-state-attributes latent2)
			     '(:keysig)))
    (5am:is (utils:set-equal (latent-state-attributes latent3)
			     '(:style)))
    (5am:is (utils:set-equal (latent-state-attributes linked)
			     '(:barlength :keysig :phase :pulses :style)))))

;; Get latent variable should return an instance of the latent variable
;; with the name of the provided symbol.
;; When a list of symbols is provided, it should return a linked latent
;; variable whose links equal the result of calling get-latent-variable
;; on the symbols in the list.
(5am:test (get-latent-variable :depends-on latent-variable-definition)
  (let ((latent1 (get-latent-variable 'latent1))
	(linked (get-latent-variable '(latent1 latent2))))
    (5am:is (eq (type-of latent1) 'latent1))
    (5am:is (eq (type-of linked) 'linked))
    (5am:is (utils:set-equal (mapcar #'type-of (latent-variable-links linked))
			     '(latent1 latent2)))))

(5am:test (get-latent-variables :depends-on get-latent-variable)
  (let ((latent-variables (get-latent-variables '(latent1 latent2))))
    (5am:is (utils:set-equal (mapcar #'type-of latent-variables)
			     '(latent1 latent2)))))

(5am:test (get-latent-category :depends-on get-latent-variable)
  (let ((latent1 (get-latent-variable 'latent1))
	(latent2 (get-latent-variable 'latent2))
	(latent3 (get-latent-variable 'latent3))
	(linked (get-latent-variable '(latent1 latent2 latent3)))
	(*latent-state* '(:barlength 4 :style salsa :pulses 2 :foo 'a :bar 3)))
    (5am:is (equal (get-latent-category latent1) '(4 2)))
    (5am:is (equal (get-latent-category latent2) nil))
    (5am:is (equal (get-latent-category latent3) '(salsa)))
    (let ((attributes (category-attributes linked)))
      (5am:is (equal (get-latent-category linked)
		  (mapcar (lambda (p) (getf *latent-state* p)) attributes))))))

(5am:test (get-latent-interpretation :depends-on get-latent-variable)
  (let ((latent1 (get-latent-variable 'latent1))
	(latent2 (get-latent-variable 'latent2))
	(latent3 (get-latent-variable 'latent3))
	(linked (get-latent-variable '(latent1 latent2 latent3)))
	(*latent-state* '(:barlength 4 :pulses 2 :phase 3 :style 'jazz
			  :keysig 7 :foo 'a :bar 3)))
    (5am:is (equal (get-latent-interpretation latent1) '(4 3)))
    (5am:is (equal (get-latent-interpretation latent2) '(7)))
    (5am:is (equal (get-latent-interpretation latent3) nil))
    (let ((attributes (interpretation-attributes linked)))
      (5am:is (equal (get-latent-interpretation linked)
		     (mapcar (lambda (p) (getf *latent-state* p)) attributes))))))

(5am:test with-latent-state
  (let* ((initial-state '(:foo a :bar b))
	 (new-state '(:lorum ipsum))
	 (shadowed-state '(:foo b))
	 (*latent-state* initial-state))
    (5am:is (equal *latent-state* initial-state))
    (with-latent-state new-state
      (5am:is (plist-equal *latent-state* (append initial-state
						  new-state)))
      (with-latent-state shadowed-state
	(5am:is (plist-equal *latent-state* (append shadowed-state
						    new-state
						    '(:bar b)))))
      (5am:is (plist-equal *latent-state* (append initial-state new-state))))
    (with-latent-state (append new-state shadowed-state)
      (5am:is (plist-equal *latent-state* (append shadowed-state
						  new-state
						  '(:bar b)))))
    (5am:is (plist-equal *latent-state* initial-state))))

(5am:test (with-latent-variable-state :depends-on (and . (get-latent-variable
							  with-latent-state))))

(5am:test (with-latent-category :depends-on (and . (get-latent-category
						    with-latent-variable-state)))
  (let* ((latent1 (get-latent-variable 'latent1))
	 (latent2 (get-latent-variable 'latent2))
	 (*latent-state* '(:barlength 0 :phase 1 :add 4 :foo 'a :bar 3))
	 (latent-category-1 (create-category latent1 :barlength 3 :pulses 2))
	 (latent-category-2 (create-category latent2)))
    (5am:is (equal (get-latent-category latent1) (create-category latent1 :barlength 0
								  :phase 1)))
    (5am:is (equal (get-latent-category latent2) (create-category latent2 :add 4)))
    (5am:is (not (latent-category-set-p latent1)))
    (5am:is (not (latent-category-set-p latent2)))
    (with-latent-category (latent-category-1 latent1)
      (5am:is (equal (get-latent-category latent1) latent-category-1))
      (5am:is (latent-category-set-p latent1))
      (5am:is (not (latent-category-set-p latent2)))
      (with-latent-category (latent-category-2 latent2)
	(5am:is (latent-category-set-p latent1))
	(5am:is (latent-category-set-p latent2))
	(5am:is (equal (get-latent-category latent1) latent-category-1))
	(5am:is (equal (get-latent-category latent2) latent-category-2))))))

;;; Three different latent-variable behaviors can be achieved depending on the
;;; category and interpretation attribute lists:
;;;
;;; * A non-empty *category-attributes* and *interpretation* attributes list means
;;;   that various models are used for multiple interpretations per model
;;; * An empty *category-attributes* list indicates that the same model is used
;;;   for all interpretations.
;;; * An empty *interpretation-attributes* list indicates that different models are
;;;   (probably but not necessarily) used and no interpretation takes place
;;;
;;; Methods whose behaviour is affected by these different model types are
;;; GET-LATENT-STATES and GET-PRIOR-DISTRIBUTION and therefore those should be
;;; tested with linked variables whose links are different combinations of those
;;; types.

(5am:def-fixture event (&rest attributes)
  (let ((default-attributes '(:id identifier
			      :voice 0 :vertint12 0 :articulation nil
			      :comma nil :ornament nil :dyn nil
			      :accidental 0 :mpitch 40 :cpitch 80
			      :deltast 0 :bioi bioi :midc midc
			      :timebase 96 :onset 0 :description nil
			      :duration 1 :mode 0 :keysig 0
			      :pulses nil :barlength nil
			      :tempo nil :phrase nil)))
    (progn
      (dotimes (pair-index (/ (length attributes) 2))      
	(setf (getf default-attributes (elt attributes (* pair-index 2)))
	      (elt attributes (+ (* pair-index 2) 1))))
      (let ((event (apply #'make-instance (cons 'md:music-event attributes))))
	(&body)))))

(5am:test create-category
  (let ((latent1 (get-latent-variable 'latent1))
	(latent2 (get-latent-variable 'latent2))
    	(latent3 (get-latent-variable 'latent3))
	(linked (get-latent-variable '(latent1 latent2 latent3))))
    (5am:is (equal (create-category latent1 :barlength 4 :pulses 2)
		   '(4 2)))
    (5am:is (equal (create-category latent2)
		   nil))
    (5am:is (equal (create-category latent3 :style 'reggae)
		   '(reggae)))
    (5am:is (equal (create-category linked :pulses 2 :style 'bebop :barlength 4)
		   '(4 2 bebop)))))

(5am:test create-latent-state
  (let ((latent1 (get-latent-variable 'latent1))
	(latent2 (get-latent-variable 'latent2))
    	(latent3 (get-latent-variable 'latent3))
	(linked (get-latent-variable '(latent1 latent2 latent3))))
    (5am:is (equal (create-latent-state latent1 (create-category latent1
								 :barlength 4
								 :pulses 2) :phase 3)
		   '(4 3 2)))
    (5am:is (equal (create-latent-state latent2 (create-category latent2)
					       :keysig 2)
		   '(2)))
    (5am:is (equal (create-latent-state latent3 (create-category latent3
								 :style 'classical))
		   '(classical)))
    (5am:is (equal (create-latent-state linked (create-category linked
								:pulses 2
								:barlength 4
								:style 'metal)
					:keysig 3 :phase 5)
		   '(4 3 5 2 metal)))))

(5am:test (get-event-category :depends-on (and . (get-latent-variable
						  create-category)))
  (let ((latent1 (get-latent-variable 'latent1))
	(latent2 (get-latent-variable 'latent2))
    	(latent3 (get-latent-variable 'latent3)))
    (5am:with-fixture event (:barlength 4 :pulses 2 :keysig 5 :description 'rock)
      (5am:is (equal (get-event-category event latent1)
		     (create-category latent1 :barlength 4 :pulses 2)))
      (5am:is (equal (get-event-category event latent2)
		     (create-category latent2)))
      (5am:is (equal (get-event-category event latent3)
		     (create-category latent3 :style 'rock))))))
  
(5am:test get-latent-states
  (let ((latent1 (get-latent-variable 'latent1))
	(latent2 (get-latent-variable 'latent2))
    	(latent3 (get-latent-variable 'latent3))
	(latent1-empirical (get-latent-variable 'latent1-empirical))
	(linked-1-2 (get-latent-variable '(latent1 latent2)))
	(linked-2-3 (get-latent-variable '(latent2 latent3)))
	(linked-1-3 (get-latent-variable '(latent1 latent3)))
	(linked-1-2-3 (get-latent-variable '(latent1 latent2 latent3))))
    (5am:is (utils:set-equal (get-latent-states '(4 2) latent1)
			     (loop for i below 4 collect (list 4 i 2))
			     :test #'equal))
    (5am:is (utils:set-equal (get-latent-states nil latent2)
			     (loop for i below 12 collect (list i))
			     :test #'equal))
    (5am:is (utils:set-equal (get-latent-states '(baroque) latent3) '((baroque))
			     :test #'equal))
    (setf (prior-distribution latent1-empirical)
	  '(((3 0 1) . 0.5) ((3 1 1) . 0.5)
	    ((2 0 1) . 0.3) ((2 1 1) . 0.7)))
    (5am:is (utils:set-equal (get-latent-states '(3 1) latent1-empirical)
			     '((3 0 1) (3 1 1))
			     :test #'equal))
    (let* ((variable linked-1-2)
	   (category (create-category variable :barlength 4 :pulses 2)))
      (5am:is (utils:set-equal
	       (get-latent-states category variable)
	       (let ((latent-states))
		 (dotimes (phase 4 latent-states) 
		   (dotimes (keysig 12)
		     (push (create-latent-state variable category 
						:phase phase
						:keysig keysig)
			   latent-states))))
	       :test #'equal)))
    (let* ((variable linked-2-3)
	   (category (create-category variable :style 'latin)))
      (5am:is (utils:set-equal
	       (get-latent-states category variable)
	       (let ((latent-states))
		 (dotimes (keysig 12 latent-states)
		   (push (create-latent-state variable category
					      :keysig keysig)
			 latent-states)))
	       :test #'equal)))
    (let* ((variable linked-1-3)
	   (category (create-category variable :style 'samba :barlength 4 :pulses 2)))
      (5am:is (utils:set-equal
	       (get-latent-states category variable)
	       (let ((latent-states))
		 (dotimes (phase 4 latent-states) 
		   (push (create-latent-state variable category
				    :phase phase)
			 latent-states)))
	       :test #'equal)))
    (let* ((variable linked-1-2-3)
	   (category (create-category variable :style 'tango :barlength 4 :pulses 2)))
      (5am:is (utils:set-equal
	       (get-latent-states category variable)
	       (let ((latent-states))
		 (dotimes (phase 4 latent-states) 
		   (dotimes (keysig 12)
		     (push (create-latent-state variable category
						:phase phase
						:keysig keysig)
			   latent-states))))
	       :test #'equal)))))

(5am:test get-link-category 
  (let ((latent1 (get-latent-variable 'latent1))
	(latent2 (get-latent-variable 'latent2))
	(latent3 (get-latent-variable 'latent3))
	(linked-1-2 (get-latent-variable '(latent1 latent2)))
	(linked-2-3 (get-latent-variable '(latent2 latent3)))
	(linked-1-3 (get-latent-variable '(latent1 latent3)))
	(linked-1-2-3 (get-latent-variable '(latent1 latent2 latent3))))
    (5am:is (equal (get-link-category latent1 '(3 2) latent1) '(3 2)))
    (5am:is (equal (get-link-category linked-1-2 '(3 2) latent1) '(3 2)))
    (5am:is (equal (get-link-category linked-1-2 '(3 2) latent2) nil))
    (5am:is (equal (get-link-category linked-2-3 '(trance) latent2) nil))
    (5am:is (equal (get-link-category linked-2-3 '(trance) latent3) '(trance)))
    (5am:is (equal (get-link-category linked-1-3 '(3 2 minimal) latent1) '(3 2)))
    (5am:is (equal (get-link-category linked-1-3 '(3 2 minimal) latent3) '(minimal)))
    (5am:is (equal (get-link-category linked-1-2-3 '(3 2 drumnbass) latent1) '(3 2)))
    (5am:is (equal (get-link-category linked-1-2-3 '(3 2 drumnbass) latent2) nil))
    (5am:is (equal (get-link-category linked-1-2-3 '(3 2 drumnbass) latent3) '(drumnbass)))))
  
(5am:test get-prior-distribution
  "Test GET-PRIOR-DISTRIBUTION, the method that initializes a latent variable's
prior distribution based on a set of training items and labels."
  (let* ((latent1 (get-latent-variable 'latent1))
	 (latent3 (get-latent-variable 'latent3))
	 (latent1-empirical (get-latent-variable 'latent1-empirical))
	 (linked-1-3 (get-latent-variable '(latent1 latent3)))
;	 (linked-1-empirical-3 (get-latent-variable '(latent1-empirical latent3)))
	 ;; Each individual number in the lists below represents a training
	 ;; item (which would normally be an event sequence/composition).
	 ;; The value of the number represents the phase of the composition
	 ;; (which is used for calculating the prior of latent1-empirical).
	 (2-1-disco '(0))
	 (2-1-punk '(0 0 1))
	 (3-1-disco '(0))
	 (3-1-soul '(1 1 0 0 1))
	 ;; Every pair of lists below represents a training set.
	 ;; The var-name-categories list represents category labels;
	 ;; The var-name training list is a list containing lists of training items
	 ;; for each category in the var-name categories list.
	 (latent1-categories '((2 1) (3 1)))
	 (latent1-training (list (append 2-1-disco 2-1-punk) (append 3-1-disco 3-1-soul)))
	 (latent3-categories '((disco) (punk) (soul)))
	 (latent3-training (list (append 2-1-disco 3-1-disco) 2-1-punk 3-1-soul))
	 (linked-1-3-categories '((2 1 disco) (2 1 punk) (3 1 disco) (3 1 soul)))
	 (linked-1-3-training (list 2-1-disco 2-1-punk 3-1-disco 3-1-soul))
	 (latent1-prior (get-prior-distribution latent1-training latent1-categories latent1))
	 (latent1-latent-states
	  (apply #'append (mapcar (lambda (c) (get-latent-states c latent1))
				  latent1-categories)))
	 (counts (mapcar #'length latent1-training))
	 (normalisation (apply #'+ (mapcar (lambda (cat count)
					     (* (/ count (apply #'+ counts))
						(get-category-attribute
						 cat :barlength latent1)))
					   latent1-categories counts)))
	 (latent3-prior (get-prior-distribution latent3-training latent3-categories latent3))
	 (latent3-latent-states (apply #'append
				       (mapcar (lambda (c) (get-latent-states c latent3))
					       latent3-categories)))
	 (latent1-empirical-prior (get-prior-distribution latent1-training
							  latent1-categories latent1-empirical)))
    (initialise-prior-distribution (loop for c in linked-1-3-categories
				      for items in linked-1-3-training collect
					(cons c items)) linked-1-3)
    (setf (prior-distribution latent1-empirical) latent1-empirical-prior)
    (let ((latent1-empirical-latent-states
	   (apply #'append (mapcar (lambda (c) (get-latent-states c latent1-empirical))
				   latent1-categories)))
	  (linked-1-3-latent-states (apply #'append
				       (mapcar (lambda (c) (get-latent-states c linked-1-3))
					       (categories linked-1-3)))))
      ;; For each prior and corresponding set of latent states...
      (loop for prior in (list latent1-prior latent3-prior latent1-empirical-prior
			       (prior-distribution linked-1-3))
	 for latent-states in (list latent1-latent-states latent3-latent-states
				    latent1-empirical-latent-states
				    linked-1-3-latent-states) do
	 ;; Things shall sum to one.
	   (5am:is (equal (apply #'+ (mapcar #'cdr prior)) 1))
	 ;; The number of attributes shall equal the number of latent states.
	   (5am:is (equal (length prior) (length latent-states)))))
    ;; For the linked latent variable, the number of attributes equals the n
    ;; the product of the number of latent states of each of its links.
    (5am:is (equal (length (prior-distribution linked-1-3))
		   (* (length latent1-latent-states)
		      (length latent3-latent-states))))
    ;; Default method for prior calculation spreads out probability mass equally
    ;; over different interpretations
    ;; Below a few random attributes are checked against hand-calculated
    ;; gold standards.
    (5am:is (equal (cdr (assoc '(2 0 1) latent1-prior :test #'equal))
		   (/ (/ 4 10) normalisation)))
    (5am:is (equal (cdr (assoc '(2 1 1) latent1-prior :test #'equal))
		   (/ (/ 4 10) normalisation)))
    (5am:is (equal (cdr (assoc '(3 0 1) latent1-prior :test #'equal))
		   (/ (/ 6 10) normalisation)))
    (5am:is (equal (cdr (assoc '(3 2 1) latent1-prior :test #'equal))
		   (/ (/ 6 10) normalisation)))
    (5am:is (equal (cdr (assoc '(disco) latent3-prior :test #'equal))
		   (/ 2 10)))
    (5am:is (equal (cdr (assoc '(punk) latent3-prior :test #'equal))
		   (/ 3 10)))
    (5am:is (equal (cdr (assoc '(soul) latent3-prior :test #'equal))
		   (/ 5 10)))
    ;; The prior distribution for latent1-empirical is calculated by counting
    ;; the occurrences of specific phases.
    (5am:is (equal (cdr (assoc '(3 0 1) latent1-empirical-prior :test #'equal))
		   (/ 3 10)))
    (5am:is (equal (cdr (assoc '(3 1 1) latent1-empirical-prior :test #'equal))
		   (/ 3 10)))
    (5am:is (equal (cdr (assoc '(2 1 1) latent1-empirical-prior :test #'equal))
		   (/ 1 10)))
    ;; For linked variables, the prior probability of an attribute should match the
    ;; product of prior probabilities of the link attributes associated with the
    ;; attribute.
    (5am:is (equal (cdr (assoc '(3 0 1 disco) (prior-distribution linked-1-3) :test #'equal))
		   (* (cdr (assoc '(3 0 1) latent1-prior :test #'equal))
		      (cdr (assoc '(disco) latent3-prior :test #'equal)))))
    (5am:is (equal (cdr (assoc '(3 2 1 disco) (prior-distribution linked-1-3) :test #'equal))
		   (* (cdr (assoc '(3 0 1) latent1-prior :test #'equal))
		      (cdr (assoc '(disco) latent3-prior :test #'equal)))))
    (5am:is (equal (cdr (assoc '(2 0 1 disco) (prior-distribution linked-1-3) :test #'equal))
		   (* (cdr (assoc '(2 0 1) latent1-prior :test #'equal))
		      (cdr (assoc '(disco) latent3-prior :test #'equal)))))
    ;; These ((2 1) (soul)) and have not been observed together but since the
    ;; we have probability estimates for them independently and linked prior
    ;; distributions assume independence among their constituents there should
    ;; be a probability estimate.
    (5am:is (equal (cdr (assoc '(2 1 1 soul) (prior-distribution linked-1-3) :test #'equal))
		   (* (cdr (assoc '(2 1 1) latent1-prior :test #'equal))
		      (cdr (assoc '(soul) latent3-prior :test #'equal)))))))
    


(5am:test get-category-subsets
  (flet ((make-event (id barlength pulses)
	   (make-instance 'md:music-event
			  :barlength barlength
			  :pulses pulses
			  :id (md:make-event-id 1 2 id))))
    (let ((events-1 (mapcar #'make-event
			    '(0 1 2 3 4 5 6)
			    '(3 3 3 2 2 2 2)
			    '(3 3 1 2 2 2 2)))
	  (events-2 (mapcar #'make-event
			    '(0 1 2 3 4 5 6)
			    '(3 3 3 2 2 2 2)
			    '(3 3 3 2 2 2 2)))
	  (melodic-sequence-1 (make-instance 'md:melodic-sequence :onset 0 :duration 0
					   :timebase 0 :midc 0 :description ""
					   :id (md:make-composition-id 1 1)))
	  (melodic-sequence-2 (make-instance 'md:melodic-sequence :onset 0 :duration 0
					   :timebase 0 :midc 0 :description ""
					   :id (md:make-composition-id 1 2)))
	  (latent1 (get-latent-variable 'latent1)))
      (sequence:adjust-sequence melodic-sequence-1 (length events-1) :initial-contents events-1)
      (sequence:adjust-sequence melodic-sequence-2 (length events-2) :initial-contents events-2)
      (let* ((partitions (md::partition-music-sequences (list melodic-sequence-1 melodic-sequence-2)
							(print (category-attributes latent1))))
	     (category-subsets (get-category-subsets partitions latent1)))
	(print partitions)
	(print category-subsets)))))
