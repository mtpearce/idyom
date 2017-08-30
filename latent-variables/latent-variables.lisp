(cl:in-package #:latent-variables)

(define-latent-variable metre latent-variable (:barlength :phase) (:barlength :pulses))

(define-latent-variable key latent-variable () (:keysig))

(define-latent-variable style (:style) ())
