(cl:in-package #:probabilities)

(defparameter *log-space* t)

(defmacro defop (name params default log-space)
  `(defun ,name (,@params)
     (if *log-space* ,log-space ,default)))

(defmacro generalise (name binary-op base)
  `(defun ,name (&rest probabilities)
     (reduce (lambda (a b) (funcall ,binary-op a b)) (cons ,base probabilities))))
  
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

(generalise mul #'mul-2 0)

(defop div (a b)
  (/ a b)
  (- a b))

(defop in (a)
  a (log a))

(defop out (a)
  a (exp a))
       
