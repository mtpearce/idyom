(cl:in-package #:latent-variables)

(5am:def-suite latent-variables)
(5am:in-suite latent-variables)

(defmacro with-latent-variables (&body body)
  `(progn
     (define-latent-variable foo (:a :b) (:b :c))
     (defmethod get-latent-states (category (v foo))
       '(p q r))
     (define-latent-variable bar () (:d))
     (defmethod get-latent-states (category (v bar))
       '(x y z))
     (let ((foo (make-instance 'foo))
	   (bar (make-instance 'bar)))
       ,@body)))

(5am:test get-latent-states
  (with-latent-variables
    (5am:is (equal (get-latent-states nil foo) '(p q r)))
    (5am:is (equal (get-latent-states nil bar) '(x y z)))))

(5am:test get-latent-states-metre
  (let ((m (make-instance 'metre))
	(k (make-instance 'key)))
    (5am:is (equal (get-latent-states '(4 2) m) '((4 2 0)))
    (5am:is (equal (get-latent-states nil k) '(x y z))))))

