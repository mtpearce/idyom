(cl:defpackage #:viewpoints 
  (:use #:common-lisp)
  (:export "+UNDEFINED+" "UNDEFINED-P" "GET-VIEWPOINT" "GET-VIEWPOINTS" 
           "VIEWPOINT-ELEMENT" "VIEWPOINT-SEQUENCE" "VIEWPOINT-SEQUENCES"
           "VIEWPOINT-ALPHABET" "VIEWPOINT-TYPESET" "VIEWPOINT-LINKS")
  (:documentation "Multiple viewpoint framework for music representation."))
