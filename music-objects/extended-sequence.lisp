;;;; ======================================================================
;;;; File:       extended-sequence.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2014-09-07 12:11:04 marcusp>
;;;; Time-stamp: <2014-09-07 12:26:07 marcusp>
;;;; ======================================================================

(cl:in-package #:music-data)

;; Defines a set of methods for a sequence one of whose slots is the
;; real sequence (in this case a list). This allows the allocation of
;; other slots for state information.
;;
;; Requires a lisp with extensible lists (Rhodes, User-extensible
;; Sequences, 2006/7??). Currently, that means SBCL >1.0 only.
;;
;; The code here draws heavily on Christophe's examples

(defclass list-slot-sequence (sequence standard-object)
  ((%data :accessor %list-slot-sequence-data
	  :initarg :%data
	  :initform nil)))

(defmethod sequence:length ((o list-slot-sequence))
  (length (%list-slot-sequence-data o)))

(defmethod sequence:elt ((o list-slot-sequence) index)
  (elt (%list-slot-sequence-data o) index))

(defmethod (setf sequence:elt) (new-value (o list-slot-sequence) index)
  (setf (elt (%list-slot-sequence-data o) index) new-value))

(defmethod sequence:make-sequence-like ((o list-slot-sequence) length
					&key (initial-element nil iep)
					(initial-contents nil icp))
  (let ((result (make-instance (class-of o))))
    (cond 
      ((and iep icp)
       (error "Supplied both ~S and ~S to ~S" :initial-element :initial-contents 'make-sequence-like))
      (icp
       (unless (= (length initial-contents) length)
	 (error "Length mismatch in ~S" 'make-sequence-like))
       (setf (%list-slot-sequence-data result) (coerce initial-contents 'list))
       result)
      (t
       (dotimes (i length result)
	 (push initial-element (%list-slot-sequence-data result)))))))

(defmethod sequence:adjust-sequence ((o list-slot-sequence) length
				     &key initial-element
				     (initial-contents nil icp))
  (cond
    ((= length 0)
     (setf (%list-slot-sequence-data o) nil))
    (icp
     (setf (%list-slot-sequence-data o)
	   (sequence:adjust-sequence (%list-slot-sequence-data o)
				     length
				     :initial-contents initial-contents)))
    (t (setf (%list-slot-sequence-data o)
	     (sequence:adjust-sequence (%list-slot-sequence-data o)
				       length
				       :initial-element initial-element))))
  o)

(defmethod sequence:make-simple-sequence-iterator 
    ((o list-slot-sequence) &rest args &key from-end start end)
  (declare (ignore from-end start end))
  (apply #'sequence:make-simple-sequence-iterator
	 (%list-slot-sequence-data o) args))
(defmethod sequence:iterator-step ((o list-slot-sequence) iterator from-end)
  (sequence:iterator-step (%list-slot-sequence-data o) iterator from-end))
(defmethod sequence:iterator-endp ((o list-slot-sequence) iterator limit from-end)
  (sequence:iterator-endp (%list-slot-sequence-data o) iterator limit from-end))
(defmethod sequence:iterator-element ((o list-slot-sequence) iterator)
  (sequence:iterator-element (%list-slot-sequence-data o) iterator))
(defmethod (setf sequence:iterator-element) (new-value (o list-slot-sequence) iterator)
  (setf (sequence:iterator-element (%list-slot-sequence-data o) iterator) new-value))
(defmethod sequence:iterator-index ((o list-slot-sequence) iterator)
  (sequence:iterator-index (%list-slot-sequence-data o) iterator))
(defmethod sequence:iterator-copy ((o list-slot-sequence) iterator)
  (sequence:iterator-copy (%list-slot-sequence-data o) iterator))
