(cl:in-package #:latent-variables)

(define-latent-variable metre (:barlength :pulses) (:barlength :phase))

; Extension of metre with different method of prior calculation
(defclass metre-phase (metre) ())

(define-latent-variable key () (:keysig))

(define-latent-variable style (:style) (:style))
