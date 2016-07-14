(cl:in-package #:viewpoints)

;;; Viewpoint Definitions

(defmacro define-viewpoint ((name superclass typeset) 
                            ((events class) element)
                            &key function function*)
  (let ((f* function*))
    `(progn
       (defclass ,name (,superclass)
	 ((alphabet :allocation :class
		    :initform ,(when (eql superclass 'test) ''(0 1)))
	  (typeset :initform ',typeset :allocation :class)))
       (defgeneric ,name (,events)) ; Generic function with same name as viewpoint class that takes events
       (defmethod ,name ((,events ,class)) ; Implementation that takes events of provided class
	 (declare (ignorable events))
	 (let ((events (coerce ,events 'list)))
	   ,function))
       (defmethod ,name ((,events list))
	 (declare (ignorable events))
	 ,function)
       ,(when f*
	      `(defgeneric ,(intern (concatenate 'string (symbol-name name) "*")) ; Create a symbol with the name of the viewpoint and a *?
		   (,element ,events))
	      `(defmethod ,(intern (concatenate 'string (symbol-name name) "*"))
		   (,element ,events)
		 (declare (ignorable events element))
		 ,f*)))))

(defmacro define-metrical-viewpoint ((name superclass typeset)
				     ((events class) 
				      (interpretation interpretation-class) 
				      element)
				     &key function function*)
  (let ((f* function*))
    `(progn
       (defclass ,name (,superclass)
	 ((alphabet :allocation :class
		    :initform ,(when (eql superclass 'test) ''(0 1)))
	  (typeset :initform ',typeset :allocation :class)))
       (defgeneric ,name (,events ,interpretation)) ; Generic function with same name as viewpoint class that takes events
       (defmethod ,name ((,events ,class) (,interpretation ,interpretation-class)) ; Implementation that takes events of provided class
	 (declare (ignorable events))
	 (let ((event (last-element ,events)))
	       (let ((events (coerce ,events 'list)))
		 ,function)))
       (defmethod ,name ((,events list) (,interpretation ,interpretation-class))
	 (declare (ignorable events))
	 (let ((event (last-element events)))
	       ,function))
       ,(when f*
	   `(defgeneric ,(intern (concatenate 'string (symbol-name name) "*")) ; Create a symbol with the name of the viewpoint and a *?
		(,element ,events ,interpretation))
	   `(defmethod ,(intern (concatenate 'string (symbol-name name) "*"))
	       (,element ,events  (,interpretation ,interpretation-class))
	     (declare (ignorable events element))
	     ,f*)))))


(defmacro define-basic-viewpoint (name ((events class)) function)
  `(progn 
     (register-basic-type ',name '(elt ,events 0))
     (define-viewpoint (,name basic (,name))
         ((,events ,class) element)
       :function ,function
       :function* (list element))))


