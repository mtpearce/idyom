(cl:defpackage #:latent-variables
  (:use #:common-lisp)
  (:nicknames lv)
  (:export #:latent-variable #:with-latent-variable-state
	   #:with-latent-category #:with-latent-interpretation
	   #:get-latent-variable-state #:latent-variable-attribute
	   #:get-latent-state-value #:get-latent-state-parameter
	   #:latent-variable-name #:abstract? #:get-latent-variable
	   #:get-latent-variables #:get-latent-category #:get-latent-interpretation
	   #:get-interpretation #:get-category #:get-event-category
	   #:get-link-category #:get-link-categories #:get-latent-states
	   #:interpretation-parameters #:set-link-categories
	   #:latent-category-set-p #:latent-interpretation-set-p
	   #:categories #:prior-distribution
	   #:initialise-prior-distribution)
  (:documentation ""))
