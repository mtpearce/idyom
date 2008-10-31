(cl:defpackage #:viewpoints 
  (:use #:common-lisp)
  (:export #:+undefined+ #:undefined-p #:get-viewpoint #:get-viewpoints  
           #:viewpoint-element #:viewpoint-sequence  #:viewpoint-sequences 
           #:viewpoint-alphabet #:viewpoint-typeset  #:viewpoint-links 
           #:set-alphabet-from-context #:viewpoint-name #:viewpoint-type
           #:inverse-viewpoint-function-defined-p #:basic-element
           #:attribute-equal #:viewpoint #:viewpoint-equal #:basic-p 
           #:in-typeset-p #:alphabet->events #:viewpoint-element-equal)
  (:documentation "Multiple viewpoint framework for music representation."))
