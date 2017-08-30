(cl:defpackage #:latent-variables
  (:use #:common-lisp)
  (:nicknames lv)
  (:export #:latent-variable #:*latent-state* #:with-latent-state #:get-latent-variable-state
	   #:abstract? #:get-latent-variable #:get-latent-category
	   #:get-latent-interpretation #:get-interpretation
	   #:get-category #:get-event-category #:get-latent-states
	   #:categories #:latent-states #:prior-distribution
	   #:initialise-prior-distribution)
  (:documentation ""))
