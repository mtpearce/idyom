(cl:in-package #:latent-variables)

(defclass latent-variable ()
  ((categories :initarg categories
	       :accessor categories)
   (prior-distribution :accessor prior-distribution
		       :initarg :prior-distribution))
  (:documentation "A latent variable represents a set of event attributes that 
can be inferred from the event sequences with abstract viewpoints."))

(defclass linked (latent-variable)
  ((links :accessor latent-variable-links
	  :initarg :links
	  :type list))
  (:documentation "A linked latent variable represents a set of latent variables 
over which a joint distribution can be inferred from event sequences."))
