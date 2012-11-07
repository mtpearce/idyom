(cl:in-package #:viewpoints)

(defconstant +undefined+ '@ "The undefined symbol.")

(defvar *basic-types* nil)

(defun register-basic-type (type) 
  (pushnew (intern (symbol-name type) :keyword) *basic-types*))

(defun undefined-p (&rest viewpoint-elements)
  "Returns true if any of the supplied viewpoint elements are eql to
the +undefined+ symbol if they are atoms or contain the +undefined+
symbol if they are lists else nil."
  (flet ((undefined-ps (viewpoint-element)
           (if (atom viewpoint-element)
               (eql viewpoint-element +undefined+)
               (find-if #'(lambda (symbol) (eql symbol +undefined+))
                        viewpoint-element))))
    (find-if #'undefined-ps viewpoint-elements)))

(defun get-viewpoints (attributes)
  "Returns a list of viewpoint objects corresponding to <attributes>."
  (mapcar #'get-viewpoint attributes))

(defun get-viewpoint (attribute)
  "Returns a viewpoint object for <attribute>."
  (flet ((merge-typesets (links)
           (remove-duplicates 
            (reduce #'append links :key #'viewpoint-typeset))))
    (if (atom attribute)
        (make-instance 
         (find-symbol (symbol-name attribute) (find-package :viewpoints)))
        (let* ((links (mapcar #'get-viewpoint (flatten attribute)))
               (typeset (merge-typesets links)))
          (make-instance 'linked :links links :typeset typeset)))))

(defun attribute-equal (a1 a2) 
  (cond ((and (symbolp a1) (symbolp a2))
         (string= (symbol-name a1) (symbol-name a2)))
        ((and (consp a1) (consp a2))
         (every #'(lambda (x y) (string= (symbol-name x) (symbol-name y)))
                a1 a2))
        (t nil)))


; List defined viewpoints

(defun all-viewpoints () 
  (append (all-basic-viewpoints) (all-derived-viewpoints)))
	  
(defun all-basic-viewpoints ()
  (closer-mop:class-direct-subclasses (find-class 'viewpoints::basic)))

(defun all-derived-viewpoints ()
  (closer-mop:class-direct-subclasses (find-class 'viewpoints::derived)))

