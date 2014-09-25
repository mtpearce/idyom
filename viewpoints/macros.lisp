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
      (defgeneric ,name (,events))
      (defmethod ,name ((,events ,class))
        (declare (ignorable events))
        (let ((events (coerce ,events 'list)))
          ,function))
      (defmethod ,name ((,events list))
        (declare (ignorable events))
        ,function)
      ,(when f*
             `(defgeneric ,name (,element ,events))
             `(defmethod ,(intern (concatenate 'string (symbol-name name) "*"))
                  (,element ,events)
                (declare (ignorable events element))
                ,f*)))))

(defmacro define-basic-viewpoint (name ((events class)) function)
  `(progn 
     (register-basic-type ',name)
     (define-viewpoint (,name basic (,name))
         ((,events ,class) element)
       :function ,function
       :function* (list element))))


