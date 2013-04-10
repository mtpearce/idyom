(cl:in-package #:viewpoints) 

(defgeneric viewpoint-sequences (viewpoint sequences)
  (:documentation "Returns a list of viewpoint sequences."))

(defgeneric viewpoint-sequence (viewpoint sequence)
  (:documentation "Returns a list of viewpoint elements associated
with each suffix of SEQUENCE."))

(defgeneric viewpoint-element (viewpoint sequence)
  (:documentation "Returns the viewpoint element associated with SEQUENCE 
and VIEWPOINT."))

(defgeneric viewpoint-alphabet (viewpoint))
(defgeneric viewpoint-typeset(viewpoint))
(defgeneric viewpoint-links (viewpoint)
  (:documentation "Returns the constituent viewpoints of a given
viewpoint. Except in the case of linked viewpoints, this should simply
returns the supplied object."))

(defgeneric basic-p (viewpoint))
(defgeneric derived-p (viewpoint)) 
(defgeneric test-p (viewpoint))
(defgeneric linked-p (viewpoint))
(defgeneric threaded-p (viewpoint))
(defgeneric viewpoint-name (viewpoint))
(defgeneric viewpoint-type (viewpoint))
(defgeneric inverse-viewpoint-function (viewpoint))
(defgeneric inverse-viewpoint-function-defined-p (viewpoint))
(defgeneric viewpoint-equal (viewpoint1 viewpoint2))
(defgeneric in-typeset-p (basic-viewpoint viewpoint))
(defgeneric viewpoint-element-equal (basic viewpoint element1 element2))
(defgeneric (setf viewpoint-alphabet) (alphabet viewpoint))
(defgeneric basic-sequence (viewpoint viewpoint element-list event-list))
(defgeneric basic-element (viewpoint viewpoint element event-list))

