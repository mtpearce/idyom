(cl:in-package #:promises)

(defclass promise ()
  ((identifier :initarg :id :accessor get-identifier :type identifier) ; necessary to look up associated cached objects
  (retrieval-function :initarg :f :accessor retrieval-function :type function)
  (result :accessor result))
  (:documentation "The promise object stores a function whose result is not immediately required.
Implements lazy function evaluation."))


(defgeneric retrieve (p))

(defmethod retrieve ((p promise))
  "Retrieve the result of the function and store it in the result field.
If the result was already retrieved, return it and don't call the function
again"
  (if (slot-boundp p 'result) 
      (result p) ; If the result has already been retrieved, return the result
      (let ((result (funcall (retrieval-function p)))) ; Call the function and store the result
	(setf (result p) result)
	result)))

(defmethod retrieve ((p t))
  "When retrieving anything that's not a promise, just return it."
  p)


(defmacro make-promise (&key function id)
  "Macro for creating a promise object from a function by wrapping a lambda around it."
  `(make-instance 'promise :f (lambda nil ,function) :id ,id))
