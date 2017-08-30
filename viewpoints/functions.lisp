(cl:in-package #:viewpoints)

(defconstant +undefined+ '@ "The undefined symbol.")

(defvar *basic-types* nil) ;;(make-hash-table))

(defun register-basic-type (type event) 
  (declare (ignore event))
  (pushnew (intern (symbol-name type) :keyword) *basic-types*))
  ;; (let ((types (gethash (type-of event) *basic-types*))
  ;;       (symbol (intern (symbol-name type) :keyword)))
  ;;   (unless (member symbol types)
  ;;     (setf (gethash  (type-of event) *basic-types*)
  ;;           (cons symbol types)))))

(defun get-basic-types (event)
  "Return the basic types matching the event-type."
  (declare (ignore event))
  *basic-types*)
  ;;(gethash (type-of event) *basic-types*))

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

(defun get-viewpoint-instance (attribute)
  (let* ((symbol (find-symbol (symbol-name attribute) (find-package :viewpoints))))
    (make-instance symbol)))

(defun abstract? (viewpoint)
  (typep viewpoint 'abstract))

(defun get-viewpoint (attribute)
  "Returns a viewpoint object for <attribute>."
  (flet ((merge-typesets (links)
           (remove-duplicates 
            (reduce #'append links :key #'viewpoint-typeset))))
    (if (atom attribute)
	(let ((viewpoint (get-viewpoint-instance attribute)))
	  (when (abstract? viewpoint)
	    (let* ((latent-variable-attribute (latent-variable-attribute viewpoint))
		   (latent-variable
		    (lv:get-latent-variable latent-variable-attribute)))
	      (setf (latent-variable viewpoint) latent-variable)))
	  viewpoint)
        (let* ((links (mapcar #'get-viewpoint-instance (flatten attribute)))
	       (typeset (merge-typesets links))
	       (links (stable-sort links #'(lambda (x y)
					     (string< (viewpoint-name x)
						      (viewpoint-name y)))))
	       (lv-attribute (mapcar (lambda (vp)
				       (when (abstract? vp)
					 (list (latent-variable-attribute vp))))
				     links))
	       (lv-attribute (stable-sort (remove-duplicates
					   (apply #'append
						  lv-attribute))
					  (lambda (x y) (string< (symbol-name x)
								 (symbol-name y))))))
	  (if (null lv-attribute)
	      (make-instance 'linked :links links :typeset typeset)
	      (make-instance 'abstract-linked :links links :typeset typeset
			     :latent-variable (lv:get-latent-variable lv-attribute)))))))
					       

(defun attribute-equal (a1 a2) 
  (cond ((and (symbolp a1) (symbolp a2))
         (string= (symbol-name a1) (symbol-name a2)))
        ((and (consp a1) (consp a2))
         (every #'(lambda (x y) (string= (symbol-name x) (symbol-name y)))
                a1 a2))
        (t nil)))


;;; List defined viewpoints

(defun list-direct-subclasses (class) 
  (mapcar #'class-name (closer-mop:class-direct-subclasses (find-class class))))

(defun list-basic ()
  "List of all known basic viewpoints"
  (list-direct-subclasses 'viewpoints::basic))

(defun list-derived ()
  "List of all known derived viewpoints"
  (list-direct-subclasses 'viewpoints::derived))

(defun list-threaded ()
  "List of all known threaded viewpoints"
  (list-direct-subclasses 'viewpoints::threaded))

(defun list-test ()
  "List of all known test viewpoints"
  (list-direct-subclasses 'viewpoints::test))

(defun list-viewpoints ()
  "List of all known viewpoints"
  (append (list-basic)
	  (list-derived)
	  (list-threaded)
	  (list-test)))
	  
(defun viewpoint-symbol (vp)
  (car (multiple-value-list (find-symbol (symbol-name vp)
					 (find-package 'viewpoints)))))

(defun predictors (vps)
  "List of known viewpoints that are defined in terms of at least one of the
given viewpoints, and hence may be useful for predicting them. Note
the original viewpoints are included."
  (let ((vp-names (mapcar #'viewpoint-symbol vps))
	(desc nil))
    (dolist (vp (list-viewpoints) desc)
      (let* ((ts (viewpoint-typeset (make-instance vp))))
	(if (not (null (intersection ts vp-names)))
	    (push vp desc))))))

(defun predictable ()
  "List of known viewpoints which can be predicted."
  (let ((vps nil))
    (dolist (vp (list-basic) vps)
      (if (not (null (predictors (list vp))))
	  (push vp vps)))))



