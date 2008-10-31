(cl:in-package #:viewpoints)

;;; Viewpoint Definitions

(defmacro define-viewpoint ((name superclass typeset) 
                            (events element)
                            &key function function*)
  (let ((f* function*))
    `(progn 
      (defclass ,name (,superclass)
        ((alphabet :allocation :class 
                   :initform ,(when (eql superclass 'test) ''(0 1)))
         (typeset :initform ',typeset :allocation :class)))
      (defun ,name (,events)
        (declare (ignorable events))
        ,function)
      ,(when f*
             `(defun ,(intern (concatenate 'string (symbol-name name) "*"))
               (,element ,events)
               (declare (ignorable events element))
               ,f*)))))

(defmacro define-basic-viewpoint (name (events) function)
  `(progn 
     (register-basic-type ',name)
     (define-viewpoint (,name basic (,name))
         (,events element) 
       :function ,function
       :function* (list element))))


