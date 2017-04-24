(cl:defpackage #:viewpoints 
  (:use #:common-lisp #:utils)
  (:export #:+undefined+ #:undefined-p #:get-basic-types #:register-basic-type
           #:initialise-basic-viewpoints
           #:get-viewpoint #:get-viewpoints  
           #:viewpoint-element #:viewpoint-sequence  #:viewpoint-sequences 
           #:viewpoint-alphabet #:viewpoint-typeset  #:viewpoint-links 
           #:set-alphabet-from-context #:set-alphabet-from-dataset
           #:alphabet->events #:get-basic-viewpoints
           #:set-onset-alphabet
           #:viewpoint-name #:viewpoint-type
           #:inverse-viewpoint-function-defined-p #:basic-element
	   #:list-viewpoints #:list-basic #:list-derived
	   #:list-threaded #:list-test #:predictors #:predictable
           #:attribute-equal #:viewpoint #:viewpoint-equal #:basic-p 
           #:in-typeset-p #:alphabet->events #:viewpoint-element-equal
	   #:composition-viewpoint #:dataset-viewpoint
	   #:harm-seq)
  (:documentation "Multiple viewpoint framework for music representation."))
