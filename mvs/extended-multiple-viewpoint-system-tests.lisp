(cl:in-package #:mvs)

(5am:def-suite extended-multiple-viewpoint-system)
(5am:in-suite extended-multiple-viewpoint-system)

;;; GENERATIVE MULTIPLE VIEWPOINT SYSTEMS

(5am:test get-short-term-models
  (let ((viewpoints (viewpoints:get-viewpoints '(metpos)))
	(latent-variables (lv:get-latent-variables '(metre))))
    (loop for viewpoint in viewpoints for latent-variable in latent-variables do
	 (setf (lv:categories latent-variable) '((4 2) (3 2)))
	 (setf (viewpoints:latent-variable viewpoint) latent-variable))
    (let ((stms (get-short-term-models viewpoints)))
      (5am:is (eq (array-dimension stms 0) (length viewpoints)))
      (5am:is (eq (type-of (cdr (assoc '(4 2) (aref stms 0) :test #'equal))) 'ppm-star:ppm))
      (5am:is (eq (type-of (cdr (assoc '(3 2) (aref stms 0) :test #'equal))) 'ppm-star:ppm)))))


