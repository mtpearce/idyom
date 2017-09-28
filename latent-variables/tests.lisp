(cl:in-package #:latent-variables)

(5am:def-suite latent-variables)
(5am:in-suite latent-variables)

;; A latent variable with multiple interpretations per category, like metre
(define-latent-variable latent1 (:barlength :pulses) (:phase :barlength))

(defmethod get-latent-states (category (v latent1))
  (let ((barlength (get-category-parameter category :barlength v)))
    (loop for phase below barlength collect
	 (create-latent-state v category :phase phase))))

;; Create a variant of latent1 that uses a custom method for calculating the
;; prior distribution
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

(defmethod get-latent-states (category (v latent1-empirical))
  (loop for latent-state in (mapcar #'car (prior-distribution v))
     when (equal (get-category latent-state v) category)
       collect latent-state))

;; A latent variable whose interpretations are all part of a single category
;; (like key-signature)
(define-latent-variable latent2 () (:keysig))

(defmethod get-latent-states (category (v latent2))
  (loop for keysig in (utils:range 12) collect
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
;; which returns an alphabetically sorted list of the category parameters
;; provided to the define-latent-variable macro.
;; A method specialized on linked latent variables should return an alphabetically
;; sorted list containing the category parameters of the constituent latent variables.
(5am:test category-parameters 
  (let* ((latent1 (make-instance 'latent1))
	 (latent2 (make-instance 'latent2))
	 (latent3 (make-instance 'latent3))
	 (linked (make-instance 'linked :links (list latent1 latent2 latent3))))
    (5am:is (utils:set-equal (category-parameters latent1) '(:barlength :pulses)))
    (5am:is (utils:set-equal (category-parameters latent2) nil))
    (5am:is (utils:set-equal (category-parameters latent3) '(:style)))
    (5am:is (utils:set-equal (category-parameters linked) '(:barlength :pulses :style)))))

;; For each latent variable, a specialised method should be defined
;; which returns an alphabetically sorted list of the interpretation parameters
;; provided to the define-latent-variable macro.
;; A method specialized on linked latent variables should return an alphabetically
;; sorted list containing the interpretation parameters of the constituent latent
;; variables.
(5am:test interpretation-parameters 
  (let* ((latent1 (make-instance 'latent1))
	 (latent2 (make-instance 'latent2))
	 (latent3 (make-instance 'latent3))
	 (linked (make-instance 'linked :links (list latent1 latent2 latent3))))
    (5am:is (utils:set-equal (interpretation-parameters latent1) '(:barlength :phase)))
    (5am:is (utils:set-equal (interpretation-parameters latent2) '(:keysig)))
    (5am:is (utils:set-equal (interpretation-parameters latent3) nil))
    (5am:is (utils:set-equal (interpretation-parameters linked)
			     '(:barlength :keysig :phase)))))

;; The latent-state-parameters method should return the an alphabetically
;; sorted list of the union between the category parameter set and the
;; interpretation parameter set. 
(5am:test latent-state-parameters
  (let* ((latent1 (make-instance 'latent1))
	 (latent2 (make-instance 'latent2))
	 (latent3 (make-instance 'latent3))
	 (linked (make-instance 'linked :links (list latent1 latent2 latent3))))
    (5am:is (utils:set-equal (latent-state-parameters latent1)
			     '(:barlength :phase :pulses)))
    (5am:is (utils:set-equal (latent-state-parameters latent2)
			     '(:keysig)))
    (5am:is (utils:set-equal (latent-state-parameters latent3)
			     '(:style)))
    (5am:is (utils:set-equal (latent-state-parameters linked)
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
    (let ((parameters (category-parameters linked)))
      (5am:is (equal (get-latent-category linked)
		  (mapcar (lambda (p) (getf *latent-state* p)) parameters))))))

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
    (let ((parameters (interpretation-parameters linked)))
      (5am:is (equal (get-latent-interpretation linked)
		     (mapcar (lambda (p) (getf *latent-state* p)) parameters))))))

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
	(5am:is (equal (print (get-latent-category latent2)) latent-category-2))))))

;;; Three different latent-variable behaviors can be achieved depending on the
;;; category and interpretation parameter lists:
;;;
;;; * A non-empty *category-parameters* and *interpretation* parameters list means
;;;   that various models are used for multiple interpretations per model
;;; * An empty *category-parameters* list indicates that the same model is used
;;;   for all interpretations.
;;; * An empty *interpretation-parameters* list indicates that different models are
;;;   (probably but not necessarily) used and no interpretation takes place
;;;
;;; Methods whose behaviour is affected by these different model types are
;;; GET-LATENT-STATES and GET-PRIOR-DISTRIBUTION and therefore those should be
;;; tested with linked variables whose links are different combinations of those
;;; types.

(5am:def-fixture event (&rest parameters)
  (let ((default-parameters '(:id identifier
			      :voice 0 :vertint12 0 :articulation nil
			      :comma nil :ornament nil :dyn nil
			      :accidental 0 :mpitch 40 :cpitch 80
			      :deltast 0 :bioi bioi :midc midc
			      :timebase 96 :onset 0 :description nil
			      :duration 1 :mode 0 :keysig 0
			      :pulses nil :barlength nil
			      :tempo nil :phrase nil)))
    (progn
      (dotimes (pair-index (/ (length parameters) 2))      
	(setf (getf default-parameters (elt parameters (* pair-index 2)))
	      (elt parameters (+ (* pair-index 2) 1))))
      (let ((event (apply #'make-instance (cons 'md:music-event parameters))))
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
	

(5am:test get-link-categories
  (let ((latent1 (get-latent-variable 'latent1))
	(latent2 (get-latent-variable 'latent2))
	(latent3 (get-latent-variable 'latent3))
	(linked-1-2 (get-latent-variable '(latent1 latent2)))
	(linked-2-3 (get-latent-variable '(latent2 latent3)))
	(linked-1-3 (get-latent-variable '(latent1 latent3)))
	(linked-1-2-3 (get-latent-variable '(latent1 latent2 latent3)))
	(categories-1) (categories-2) (categories-3) (categories-1-2)
	(categories-2-3) (categories-1-3) (categories-1-2-3))
    (dolist (barlength '(1 2 3 4))
      (dolist (pulses '(1 2))
	(push (list barlength pulses) categories-1)
	(push (car categories-1) categories-1-2)))
    (push nil categories-2)
    (dolist (style '(psychedelic calypso))
      (push (list style) categories-3)
      (push (car categories-3) categories-2-3))
    (dolist (c-1 categories-1)
      (dolist (c-3 categories-3)
	(push (append c-1 c-3) categories-1-3)
	(push (car categories-1-3) categories-1-2-3)))
    (setf (categories latent1) categories-1)
    (setf (categories linked-1-2) categories-1-2)
    (setf (categories linked-2-3) categories-2-3)
    (setf (categories linked-1-3) categories-1-3)
    (setf (categories linked-1-2-3) categories-1-2-3)
    (5am:is (utils:set-equal (get-link-categories latent1 latent1)
			     categories-1 :test #'equal))
    (5am:is (utils:set-equal (get-link-categories linked-1-2 latent1)
			     categories-1 :test #'equal))
    (5am:is (utils:set-equal (get-link-categories linked-1-2 latent2)
			     categories-2 :test #'equal))
    (5am:is (utils:set-equal (get-link-categories linked-2-3 latent2)
			     categories-2 :test #'equal))
    (5am:is (utils:set-equal (get-link-categories linked-2-3 latent3)
			     categories-3 :test #'equal))
    (5am:is (utils:set-equal (get-link-categories linked-1-3 latent1)
			     categories-1 :test #'equal))
    (5am:is (utils:set-equal (get-link-categories linked-1-3 latent3)
			     categories-3 :test #'equal))
    (5am:is (utils:set-equal (get-link-categories linked-1-2-3 latent1)
			     categories-1 :test #'equal))
    (5am:is (utils:set-equal (get-link-categories linked-1-2-3 latent2)
			     categories-2 :test #'equal))
    (5am:is (utils:set-equal (get-link-categories linked-1-2-3 latent3)
			     categories-3 :test #'equal)))))
  
(5am:test set-link-categories
  (let ((linked-1-2-3 (get-latent-variable '(latent1 latent2 latent3)))
	(categories-1) (categories-2) (categories-3) (categories-1-2-3))
    (dolist (barlength '(1 2 3 4))
      (dolist (pulses '(1 2))
	(push (list barlength pulses) categories-1)))
    (push nil categories-2)
    (dolist (style '(psychedelic calypso))
      (push (list style) categories-3))
    (dolist (c-1 categories-1)
      (dolist (c-3 categories-3)
	(push (append c-1 c-3) categories-1-2-3)))
    (setf (categories linked-1-2-3) categories-1-2-3)
    (set-link-categories linked-1-2-3)
    (multiple-value-bind (latent1 latent2 latent3)
	(apply #'values (latent-variable-links linked-1-2-3))
      (5am:is (utils:set-equal (categories latent1) categories-1 :test #'equal))
      (5am:is (utils:set-equal (categories latent2) categories-2 :test #'equal))
      (5am:is (utils:set-equal (categories latent3) categories-3 :test #'equal)))))

(5am:test get-prior-distribution
  (let* ((latent1 (get-latent-variable 'latent1))
	 (latent3 (get-latent-variable 'latent3))
	 (latent1-empirical (get-latent-variable 'latent1-empirical))
	 (linked-1-3 (get-latent-variable '(latent1 latent3)))
	 (linked-1-empirical-3 (get-latent-variable '(latent1-empirical latent3)))
	 ;; Training items (normally event sequences) are represented by numbers
	 ;; which are used as phases by latent1-empirical.
	 (2-1-disco '(0))
	 (2-1-punk '(0 0 1))
	 (3-1-disco '(0))
	 (3-1-soul '(1 1 0 0 1))
	 (latent1-categories '((2 1) (3 1)))
	 (latent1-training (list (append 2-1-disco 2-1-punk) (append 3-1-disco 3-1-soul)))
	 (latent3-categories '((disco) (punk) (soul)))
	 (latent3-training (list (append 2-1-disco 3-1-disco) 2-1-punk 3-1-soul))
	 (linked1-3-categories '((2 1 disco) (2 1 punk) (3 1 disco) (3 1 punk)))
	 (linked-1-3-training (list 2-1-disco 2-1-punk 3-1-disco 3-1-soul)))
    (flet ((test-references (var training-data categories)
	     (let* ((prior (get-prior-distribution training-data categories var))
		    (latent-states (apply #'append
					  (mapcar (lambda (c) (get-latent-states c var))
						  categories))))
	       (values prior latent-states))))
      (multiple-value-bind (prior latent-states)
	  (test-references latent1 latent1-training latent1-categories)
	(let* ((counts (mapcar #'length latent1-training))
	       (normalisation (apply #'+ (mapcar (lambda (cat count)
						   (* (/ count (apply #'+ counts))
						      (get-category-parameter
						       cat :barlength var)))
						 latent1-categories counts))))
	  ;; All priors must sum to one.
	  (5am:is (equal (apply #'+ (mapcar #'cdr prior)) 1))
	  ;; Number of parameters must equal number of latent states
	  (5am:is (equal (length prior) (length latent-states)))
	  ;; Default method for prior calculation spreads out probability mass equally
	  ;; over different interpretations
	  (5am:is (equal (cdr (assoc '(2 0 1) prior :test #'equal))
			 (/ (/ 6 11) normalisation)))
	  (5am:is (equal (cdr (assoc '(2 1 1) prior :test #'equal))
			 (/ (/ 6 11) normalisation)))
	  (5am:is (equal (cdr (assoc '(3 0 1) prior :test #'equal))
			 (/ (/ 5 11) normalisation)))
	  (5am:is (equal (cdr (assoc '(3 2 1) prior :test #'equal))
			 (/ (/ 5 11) normalisation)))))
      (multiple-value-bind (prior latent-states)
	  (test-references latent3 latent3-training latent3-categories)
	(5am:is (equal (apply #'+ (mapcar #'cdr prior)) 1))
	(5am:is (equal (length prior) (length latent-states)))
	;; Default method for prior calculation spreads out probability mass equally
	;; over different interpretations
	(5am:is (equal (cdr (assoc '(disco) prior :test #'equal))
		       (/ 2 11)))
	(5am:is (equal (cdr (assoc '(punk) prior :test #'equal))
		       (/ 3 11)))
	(5am:is (equal (cdr (assoc '(soul) prior :test #'equal))
		       (/ 5 11))))
      (multiple-value-bind (prior latent-states normalisation)
	  (test-references latent1-empirical latent1-training latent1-categories)
	(5am:is (equal (apply #'+ (mapcar #'cdr prior)) 1))
	(5am:is (equal (length prior) (length latent-states)))
	;; Default method for prior calculation spreads out probability mass equally
	;; over different interpretations
	(5am:is (equal (cdr (assoc '(2 0 1) prior :test #'equal))
		       (/ 2 11)))
	(5am:is (equal (cdr (assoc '(punk) prior :test #'equal))
		       (/ 3 11)))
	(5am:is (equal (cdr (assoc '(soul) prior :test #'equal))
		       (/ 5 11)))))))
(let* ((linked-1-3-prior (get-prior-distribution linked-1-3-training linked-1-3-categories
						     linked-1-3))
	   (linked-1-empirical-3-prior (get-prior-distribution linked-1-3-training
							       linked-1-3-categories
							       linked-1-empirical-3))
	   (latent1-latent-states 
	   (priors (list latent1-prior latent3-prior linked-1-3-prior
			 linked-1-empirical-prior))
	   (latent-variables (list latent1 latent3 linked-1-3 linked-1-empirical-3)))
      
    (dolist (prior priors)
      (5am:is (equal (apply #'+ (mapcar #'cdr prior)) 1)))
    (loop for prior in priors
       for latent-variable in latent-variables do
	 (5am:is (utils:set-equal (mapcar #'car prior) (print latent-states) :test #'equal)))
    (5am:is (set-equal (prior-distribution (first latent-variable-links latent-1-3

      ;; The probability of each interpretation (category with phase in this case) is
      ;; equal to the probability of the category divided by the normalising constant.
      (5am:is (equal (cdr (assoc '(2 0 1 disco) prior :test #'equal))
		     (/ (/ 2 10) unnormalised-sum)))
      (5am:is (equal (cdr (assoc '(3 2 1 disco) prior :test #'equal))
		     (/ (/ 1 10) unnormalised-sum)))
      ;; The pr
      (5am:is (equal (print (cdr (assoc '(2 0 1 disco) prior :test #'equal)))
		     (* (print (cdr (assoc '(2 0 1) latent1-prior :test #'equal)))
			(print (cdr (assoc '(disco) latent3-prior :test #'equal)))))))))
    (let ((prior (get-prior-distribution training-data categories linked-1-empirical-3))
	  (latent-states (apply #'append
				(mapcar (lambda (c) (get-latent-states c linked-1-3))
					categories))))
      
      (5am:is (utils:set-equal (mapcar #'car prior) latent-states :test #'equal))
      (5am:is (equal (apply #'+ (mapcar #'cdr prior)) 1))
      (5am:is (equal (cdr (assoc '(2 0 1 disco) prior :test #'equal))
		     (/ (/ 2 10) unnormalised-sum)))
      (5am:is (equal (cdr (assoc '(3 2 1 disco) prior :test #'equal))
		     (/ (/ 1 10) unnormalised-sum))))))
