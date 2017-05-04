(cl:in-package #:viewpoints)

;;; Classes

(defclass viewpoint ()
  ((alphabet :accessor %viewpoint-alphabet :initarg :alphabet
             :initform '() :type list)
   (typeset :reader %viewpoint-typeset :initarg :typeset
            :initform '() :type list))
  (:documentation "A generic template for a viewpoint that holds a
list of symbols <alphabet> defining the domain of viewpoint elements
and a list of symbols <typeset> defining the basic viewpoints from
which a given viewpoint is derived. The viewpoint class is not
intended to be directly instantiated."))

(defclass basic (viewpoint)
  ()
  (:documentation "A basic viewpoint models an attribute which is part
of the basic event structure."))

(defclass derived (viewpoint)
  ()
  (:documentation "A derived viewpoint represents an attribute which
can be derived from and, therefore, make predictions about a basic
attribute."))

(defclass test (derived)
  () 
  (:documentation "A test viewpoint represents an attribute which may
assume binary values and which is used to identify the points in a
sequence where threaded viewpoints are defined."))

(defclass linked (viewpoint)
  ((links :accessor %viewpoint-links :initarg :links
          :initform '() :type list))
  (:documentation "A linked viewpoint represents an attribute composed
of more than one basic or derived attributes whose elements are
elements of the Cartesian product of the alphabets of the constituent
viewpoints."))

(defclass threaded (derived)
  ()
  (:documentation "A threaded viewpoint represents an attribute whose
values are only defined at designated points in a sequence."))

(defclass continuous (viewpoint)
  ()
  (:documentation "A continuous viewpoint is one that represents
a continuous-valued attribute."))
