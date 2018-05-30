(cl:in-package #:viewpoints)

(defun iois->durations (iois)
  (cdr iois))

(defun iois->biois (iois)
  (butlast iois))

(defun iois->onsets (iois)
  (apply #'utils:cumsum
	 (iois->biois iois)))

(defun list-pair->plist (keywords values)
  (unless (or (null keywords) (null values))
    (let ((k (car keywords))
	  (v (car values)))
      (cons k (cons v (list-pair->plist (cdr keywords) (cdr values)))))))

(defparameter *default-attributes* (list-pair->plist (get-basic-types nil)
						     (loop repeat (length (get-basic-types nil))
							collect +undefined+)))

(defun basic-attributes->music-events (dataset-index composition-index
					  &rest basic-attribute-sequences)
  "Given a PLIST, BASIC-ATTRIBUTE-SEQUENCES, generate a list of events whose basic
attributes corresponds to the basic attributes in BASIC-ATTRIBUTE-SEQUENCES."
  (let* ((basic-attributes (loop for i below (/ (length basic-attribute-sequences) 2)
			      collect (nth (* i 2) basic-attribute-sequences)))
	 (basic-attribute-sequences (loop for type in basic-attributes
				       collect (getf basic-attribute-sequences type)))
	 (event-indices (utils:generate-integers 0 (length (first basic-attribute-sequences)))))
    (apply #'mapcar (cons (lambda (event-index &rest values)
			    (let ((event (apply #'make-instance
						(list 'md:music-event
						      :id (md:make-event-id dataset-index
									    composition-index
									    event-index)))))
			      (dolist (slot md::*music-slots* event)
				(setf (slot-value event slot)
				      (or (getf (list-pair->plist basic-attributes values)
						(find-symbol (symbol-name slot) :keyword))
					  +undefined+)))))
			  (cons event-indices basic-attribute-sequences)))))

(5am:def-suite extensions :description "Unit tests for functions in extensions.lisp.")
(5am:in-suite extensions)

(5am:def-fixture mock-rhythm-dataset (onset-sequences &optional (dataset-index 0))
  (let* ((event-lists (mapcar (lambda (onsets)
				(basic-attributes->music-events
				 dataset-index 0 :onset onsets
				 :bioi (cons (first onsets)
					     (mapcar (lambda (a b) (- a b))
						     (subseq onsets 1) onsets))))
			      onset-sequences))
	 (music-sequences (mapcar (lambda (event-list composition-index)
				    (make-instance 'md:music-sequence
						   :id (md:make-composition-id dataset-index
									       composition-index)
						   :%data event-list))
				  event-lists
				  (generate-integers 0 (1- (length onset-sequences)))))
	 (dataset (make-instance 'md:music-dataset :id (md:make-dataset-id dataset-index)
				       :%data music-sequences)))
    (&body)))

(5am:test onset-alphabet
  (let* ((rhythms '((0 1) (1 3))))
    (5am:with-fixture mock-rhythm-dataset (rhythms)
      (initialise-basic-viewpoints (coerce dataset 'list))
      (let ((onset (get-viewpoint 'onset))
	    (bioi (get-viewpoint 'bioi)))
	(5am:is (equal (viewpoint-alphabet onset) '(0 1 3)))
	(5am:is (equal (viewpoint-alphabet bioi) '(0 1 2)))
	(5am:is (equal (onset-alphabet (subseq (first event-lists) 0 1)) '(0)))
	(5am:is (equal (onset-alphabet (subseq (second event-lists) 0 1)) '(1)))
	(5am:is (equal (onset-alphabet (first event-lists)) '(1 2)))
	(5am:is (equal (onset-alphabet (second event-lists))'(2 3)))))))
