(cl:in-package #:viewpoints)

;;; Viewpoint Definitions

(defmacro define-viewpoint ((name superclass typeset) 
                            ((events class) element)
                            &key function function* continuous)
  "Defines a viewpoint.
<continuous> should be a Boolean, identifying whether the viewpoint
is a continuous viewpoint or not."
  (let ((f* function*)
	(superclasses (if continuous (list superclass 'continuous)
			  (list superclass))))
    `(progn 
       (defclass ,name (,@superclasses)
	 ((alphabet :allocation :class 
		    :initform ,(when (eql superclass 'test) ''(0 1)))
	  (typeset :initform ',typeset :allocation :class)))
       (defgeneric ,name (,events))
       (defmethod ,name ((,events ,class))
	 (let ((events (coerce ,events 'list)))
	   (,name events)))
       (defmethod ,name ((,events list))
	 (declare (ignorable ,events))
	 ,(if continuous
	      `(let ((continuous-output
		      (utils:round-to-nearest-decimal-place ,function 10)))
		 (if *discretise-viewpoints*
		     (let ((quantiles (gethash (symbol-name ',name)
					       *viewpoint-quantiles*)))
		       (if quantiles
			   (if (undefined-p continuous-output)
			       continuous-output
			       (utils:assign-to-quantile continuous-output
							 quantiles))
			   (error "Quantiles needed to discretise viewpoints.")))
		     continuous-output))
	      `,function))
       ,(when f*
	  (let ((fname `,(intern (concatenate 'string (symbol-name name) "*"))))
	    `(progn
	       (defgeneric ,fname (,element ,events))
	       (defmethod ,fname (,element ,events)
		 (declare (ignorable events element))
		 ,f*)))))))

(defmacro define-basic-viewpoint (name ((events class)) function)
  `(progn 
     (register-basic-type ',name '(elt ,events 0))
     (define-viewpoint (,name basic (,name))
         ((,events ,class) element)
       :function ,function
       :function* (list element))))

(defmacro define-threaded-viewpoint (name base-viewpoint test-viewpoint class)
  (let* ((base-viewpoint `(get-viewpoint ',base-viewpoint))
         (test-viewpoint `(get-viewpoint ',test-viewpoint))
         (typeset `(append (viewpoint-typeset ,base-viewpoint) (viewpoint-typeset ,test-viewpoint))))
    `(define-viewpoint (,name threaded ,(eval typeset))
         ((events ,class) element)
       :function (let ((e (last-element events)))
                   (if (null e)
                       +undefined+
                       (let ((f (viewpoint-element ,test-viewpoint events)))
                         (if (zerop f)
                             +undefined+
                             (viewpoint-element ,base-viewpoint (filter ,test-viewpoint events))))))
       :function* (let ((base-function (inverse-viewpoint-function ,base-viewpoint)))
                    (when base-function 
                      (let ((e (append (strip-until-true ,test-viewpoint (butlast events))
                                       (last events))))
                        (funcall base-function element e)))))))  

