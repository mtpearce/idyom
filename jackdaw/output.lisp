(cl:in-package #:generative-models)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Writers are designed to write output in csv format to a
;; given destination.
;;
;; Assumptions are that the program to which they are assigned
;; processes a sequence of event-sequences item by item, that the program
;; calls NEXT-EVENT after each event, and NEXT-SEQUENCE after each
;; sequence, and WRITE-EVENT once after processing each event.
;;
;; The writer keeps track of a set of fields that is written to
;; DESTINATION upon calling WRITE-EVENT for the first time
;; (when SEQUENCE-INDEX and EVENT-INDEX equal 0).
;;
;; A WRITER instance can for example be used to accumulate information
;; during event processing.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass writer ()
  ((sequence-index :accessor sequence-index :initform 0)
   (event-index :accessor event-index :initform 0)
   (fields :accessor fields :initform nil)
   (destination :initarg :destination :reader destination)))

(defclass marginal-writer (writer)
  ((marginal :accessor marginal :initform nil)
   (branch-ids :accessor branch-ids)
   (sequence-uid :accessor sequence-uid)
   (graph :initarg :graph :reader graph :type gm::feature-graph)
   (features :initarg :features :accessor writer-features :initform nil)))

(defclass full-writer (marginal-writer)
  ((plausible-table :accessor plausible-table :initform nil)))

(defclass plausible-writer (full-writer) ()
  (:documentation "Write only plausible parameters"))

(defclass annotations-writer (full-writer) ()
  (:documentation "Like full-writer, but include a column indicating
whether the parameter is correct."))

(defclass sequence-index-writer (writer) ())

(defmethod initialize-instance :after ((writer marginal-writer) &key)
  (setf (fields writer) 
	(append (writer-features writer) (list 'probability))))

(defmethod initialize-instance :after ((writer full-writer) &key)
  (setf (writer-features writer)
	(f:identifiers (features (graph writer))))
  (setf (fields writer)
	(append (writer-features writer) '(probability plausible))))

(defmethod initialize-instance :after ((writer annotations-writer) &key)
  (setf (writer-features writer)
	(f:identifiers (features (graph writer))))
  (setf (fields writer)
	(append (writer-features writer) '(probability plausible correct))))

(defmethod initialize-instance :after ((writer sequence-index-writer) &key)
  (setf (fields writer) '(sequence-index)))

(defmethod set-uid ((writer writer) uid)
  (warn "~a writer does not support setting UID." (type-of writer)))

(defmethod set-uid ((writer marginal-writer) uid)
  (setf (sequence-uid writer) uid))
  
(defmethod next-sequence ((writer writer))
  (incf (sequence-index writer))
  (setf (event-index writer) 0))

(defmethod next-sequence :before ((writer sequence-index-writer))
  (format (destination writer) "~A~%" (sequence-index writer)))

(defmethod next-event :before ((writer sequence-index-writer))
  (format (destination writer) "~A " (event-index writer)))

;;(defmethod next-sequence :before ((writer full-writer))
;;  (format t "~A~%" (sequence-index writer)))

(defmethod next-event ((writer writer))
  (incf (event-index writer)))

(defmethod next-event ((writer marginal-writer))
  (setf (marginal writer) nil)
  (call-next-method))

(defmethod next-event ((writer full-writer))
  (setf (plausible-table writer) nil)
  (call-next-method))

;;(defmethod next-event :before ((writer full-writer))
;;  (format t "~A~%" (event-index writer)))

(defmethod field-formatter ((writer writer) field value)
  (format nil "~A" value))

(defmethod field-formatter ((writer marginal-writer) field value)
  (if (eq field 'probability)
      (format nil "~F" value)
      (call-next-method)))

(defmethod field-formatter ((writer full-writer) field value)
  (if (member field '(plausible correct))
      (format nil "~A" (if value 1 0))
      (call-next-method)))

(defmethod write-header ((writer writer))
  (format (destination writer) "sequence,event,~{~A~^,~}~%"
	  (mapcar #'string-downcase (mapcar #'symbol-name (fields writer)))))

(defmethod write-header ((writer marginal-writer))
  (format (destination writer) "sequence-uid,event,~{~A~^,~}~%"
	  (mapcar #'string-downcase (mapcar #'symbol-name (fields writer)))))

(defmethod write-event :before ((writer writer))
  (when (and (eq (sequence-index writer) 0)
	     (eq (event-index writer) 0))
    (write-header writer)))

(defmethod write-event ((writer marginal-writer))
  (dolist (param (marginals:parameters (marginal writer)))
    (write-row writer (row-values writer param))))

(defmethod write-event ((writer plausible-writer))
  (dolist (param (marginals:parameters (marginal writer)))
    (when (gethash param (plausible-table writer))
      (write-row writer (row-values writer param)))))

(defmethod write-event ((writer sequence-index-writer)))

(defmethod write-row ((writer writer) values)
  (format (destination writer) "~A,~A,~{~A~^,~}~%"
	  (sequence-index writer)
	  (event-index writer)
	  (mapcar (lambda (f v) (field-formatter writer f v))
		  (fields writer) values)))

(defmethod write-row ((writer writer) values)
  (format (destination writer) "~A,~A,~{~A~^,~}~%"
	  (if (slot-boundp writer 'sequence-uid)
	      (sequence-uid writer)
	      (progn (warn "Sequence UID not set. Falling back to sequence-index.")
		     (sequence-index writer)))
	  (event-index writer)
	  (mapcar (lambda (f v) (field-formatter writer f v))
		  (fields writer) values)))

(defmethod set-writer-horizontal-state ((writer writer) horizontal-state))

(defmethod set-writer-horizontal-state ((writer marginal-writer) horizontal-state)
  (let ((branch-ids (car horizontal-state))
	(modeled (f:identifiers (gm::modeled (graph writer)))))
    (setf (branch-ids writer)
	  (loop for f in (writer-features writer) if (member f modeled) collect
	       (gethash f branch-ids)))))

(defmethod add-possible-state ((writer writer) elements event plausible? probability))

(defmethod add-possible-state :after ((writer full-writer) elements event plausible? probability)
  (when (null (plausible-table writer))
    (setf (plausible-table writer) (make-hash-table :test #'equal)))
  (let ((key (marginals:marginal-parameter
	      elements (writer-features writer) (list (branch-ids writer)))))
    (setf (gethash key (plausible-table writer)) plausible?)))

(defmethod add-possible-state :after ((writer annotations-writer)
			       elements event plausible? probability)
  (when (null (plausible-table writer))
    (setf (plausible-table writer) (make-hash-table :test #'equal)))
  (let ((correct? (correct? (graph writer) elements event))
	(key (marginals:marginal-parameter
	      elements (writer-features writer) (list (branch-ids writer)))))
    (setf (gethash key (plausible-table writer))
	  (list plausible? correct?))))

(defmethod add-possible-state ((writer marginal-writer) elements event plausible?
			       probability)
  (when (null (marginal writer))
    (setf (marginal writer) (marginals:make)))
  (marginals:update (marginal writer) elements (writer-features writer) probability
			 (list (branch-ids writer))))

(defmethod row-values ((writer marginal-writer) param)
  (append (cdr param) (list (marginals:probability (marginal writer) param))))

(defmethod row-values ((writer full-writer) param)
  (let ((plausible? (gethash param (plausible-table writer))))
    (append (cdr param)
	    (list (marginals:probability (marginal writer) param) plausible?))))

(defmethod row-values ((writer annotations-writer) param)
  (let* ((pl-c (gethash param (plausible-table writer)))
	 (plausible? (car pl-c))
	 (correct? (cadr pl-c)))
    (append (cdr param)
	    (list (marginals:probability (marginal writer) param)
		  plausible? correct?))))
