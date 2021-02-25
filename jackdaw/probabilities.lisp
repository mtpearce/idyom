(cl:in-package #:probabilities)

(defparameter *log-space* t)

(defmacro defop (name params default log-space)
  `(defun ,name (,@params)
     (if *log-space* ,log-space ,default)))

(defmacro generalise (name binary-op base log-base)
  `(defun ,name (&rest probabilities)
     (reduce (lambda (a b) (funcall ,binary-op a b))
	     (cons (if *log-space* ,log-base ,base) probabilities))))
  
(defop add-2 (a b)
  (+ a b)
  (multiple-value-bind (a b)
      (if (>= b a) (values a b) (values b a))
    (+ b (log (+ (exp (- a b)) 1)))))

(defun add (p &rest more)
  (if (null more) p
      (reduce (lambda (a b) (add-2 a b)) (cons p more))))

(defop mul-2 (a b)
  (* a b)
  (+ a b))

(generalise mul #'mul-2 1 0)

(defop div (a b)
  (/ a b)
  (- a b))

(defop in (a)
  a (log a))

(defop out (a)
  a (exp a))

(defop logarithm (a)
  (log a) a)

(defop entropy (probabilities)
  (- (apply #'+ (mapcar (lambda (p) (* p (log p))) probabilities)))
  (- (apply #'+ (mapcar (lambda (lp) (* (exp lp) lp)) probabilities))))
