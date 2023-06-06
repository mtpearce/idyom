(cl:in-package #:viewpoints)

;; NB: An attribute is a symbol representing a primitive viewpoint,
;; whereas a viewpoint is the viewpoint object itself (see
;; classes.lisp).

(defconstant +undefined+ '@ "The undefined symbol.")

(defvar *basic-attributes* (make-hash-table) "A record of the basic attributes for each kind of music object (e.g., events, slices).")

(defun register-basic-attribute (attribute event)
  "Register an attribute (symbol) for a particular kind of music event
from music-objects (e.g., md:music-event or md:music-slice)."
  (let ((attributes (gethash (type-of event) *basic-attributes*))
        (symbol (intern (symbol-name attribute) :keyword)))
    (unless (member symbol attributes)
      (setf (gethash  (type-of event) *basic-attributes*)
            (cons symbol attributes)))))

(defun get-basic-attributes (event)
  "Returns the registered basic attributes corresponding to the music object <event>."
  (gethash (type-of event) *basic-attributes*))

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

(defun get-viewpoints (attributes &key (sort t))
  "Returns a list of viewpoint objects corresponding to <attributes>."
  (mapcar #'(lambda (a) (get-viewpoint a :sort sort)) attributes))

(defun get-viewpoint (attribute &key (sort t))
  "Returns a viewpoint object for <attribute>."
  (flet ((merge-typesets (links)
           (remove-duplicates 
            (reduce #'append links :key #'viewpoint-typeset))))
    (if (atom attribute)
        (make-instance 
         (find-symbol (symbol-name attribute) (find-package :viewpoints)))
        (let* ((links (mapcar #'get-viewpoint (flatten attribute)))
               (typeset (merge-typesets links))
               (links (if sort (stable-sort links #'(lambda (x y) (string< (viewpoint-name x) (viewpoint-name y)))) links)))
          (make-instance 'linked :links links :typeset typeset)))))

(defun attribute-equal (a1 a2)
  "Equality predicate for attributes."
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
  (list-direct-subclasses 'basic))

(defun list-derived ()
  "List of all known derived viewpoints"
  (list-direct-subclasses 'derived))

(defun list-threaded ()
  "List of all known threaded viewpoints"
  (list-direct-subclasses 'threaded))

(defun list-test ()
  "List of all known test viewpoints"
  (list-direct-subclasses 'test))

(defun list-viewpoints ()
  "List of all known viewpoints"
  (append (list-basic)
	  (list-derived)
	  (list-threaded)
	  (list-test)))
	  
(defun viewpoint-symbol (vp)
  "Returns a symbol corresponding to the supplied viewpoint <vp>."
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



