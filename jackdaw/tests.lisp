
(let ((d (make-instance 'tactus-interval :arguments '(:^t)
			:t0 '(.1 .2 .3 .23 .13 .03 .006 .002 .001
			      .0006 .0002 .0001 .00005 .00005)))
      (arguments (progn (let ((ht (make-hash-table))) (setf (gethash :^t ht) 10) ht)))
      (states '(9 10 11 12 13 14 15 16 17 18 19 20 21 22)))
  (maphash (lambda (k v) (print (cons k v))) (probabilities d arguments states)))


(let ((d (make-instance 'tactus-interval :arguments '(:^t)
			:t0 '(.1 .2 .3 .23 .13 .03 .006 .002 .001
			      .0006 .0002 .0001 .00005 .00005)))
      (arguments (progn (let ((ht (make-hash-table))) (setf (gethash :^t ht) ) ht)))
      (states '(9 10 11 12 13 14 15 16 17 18 19 20 21 22)))
  (maphash (lambda (k v) (print (cons k v))) (probabilities d arguments states)))

(let ((ut (make-instance 'bernouilli :symbols '(a b) :p 0.8)))
  (maphash (lambda (k v) (print (cons k v))) (probabilities ut nil '(a b))))

(let ((d (make-instance 'beat-deviation :arguments '(:t) :p '(0.4 0.2 0.1) :phase 2 :subdivision 3))
      (arguments (progn (let ((ht (make-hash-table))) (setf (gethash :t ht) 10) ht)))
      (states '(*)))
  (maphash (lambda (k v) (print (cons k v))) (probabilities d arguments states)))

(let ((d (make-instance 'beat-deviation :arguments '(:t) :p '(0.4 0.2 0.1) :phase 2 :subdivision 3))
      (arguments (progn (let ((ht (make-hash-table))) (setf (gethash :t ht) 10) ht)))
      (states '(5 6 7 8)))
  (maphash (lambda (k v) (print (cons k v))) (probabilities d arguments states)))

(cl:in-package #:generative-models)

(5am:def-suite generative-models)
(5am:in-suite generative-models)

(5am:test feature-function
  "FEATURE-FUNCTION returns ..."
 " (f:featurelet ((a (normal () () '(a b)
			    '
		 (b (recursive () () (list previous-b)
			       () () '(a b))))))))"
 )
		    

(5am:test possible-states
"  (f:featurelet ((a (normal () () '(a b)))
		 (b (normal () (a) (list (list 0 a) (list 1 a)))))
    (let ((graph (gm:make-feature-graph a b)))
      (gm::next graph)
      (gm::set-horizontal-state graph nil)
      (let* ((states (gm::possible-states graph nil))
	     (probabilities (mapcar #'cadr states))
	     (elements (mapcar (lambda (s) (mapcar (lambda (f) (gethash f s)) '(a b)))
			       states)))
	(5am:is (every (lambda (p) (eq p (probabilities:in (/ 1 4)))) probabilities))))))"
)

(cl:in-package #:models)

(5am:def-suite models)
(5am:in-suite models)

(5am:test zeroth-order-once
  (let ((model (make-zeroth-order-once)))
    ;; Root location evaluates to T
    (5am:is (root-location model))
    ;; Next location from root evaluates to NIL
    ;; Observe A at root
    (let ((location (next-location model 'a t :construct? t)))
      (5am:is (null location)))
    ;; Observe B at non-root
    (next-location model 'b nil :construct? t)
    ;; Non-root observation does not affect probabilities
    (5am:is (eq (probability model nil 'a) (probs:in 1)))
    ;; Observe B at root twice
    (next-location model 'b t :construct? t)
    (next-location model 'b t :construct? t)
    ;; Distribution at root-location is empirical
    (5am:is (null (set-difference (distribution model t '(a b))
				  (list (list 'a (probs:in (/ 1 3)))
					(list 'b (probs:in (/ 2 3))))
				  :test #'equal)))
    ;; Observation at root without CONSTRUCT? should be ignored
    (next-location model 'b t)
    ;; Distribution at root is empirical    
    (5am:is (null (set-difference (distribution model t '(a b))
				  (list (list 'a (probs:in (/ 1 3)))
					(list 'b (probs:in (/ 2 3))))
				  :test #'equal)))
    (5am:is (null (set-difference (distribution model nil '(a b))
				  (uniform-distribution '(a b))
				  :test #'equal)))))


(5am:test metre-and-phase-prior
  (let ((model (make-phase-metre-prior :metre-function #'cadr
				       :phases-function #'identity)))
    (dolist (s '((0 3) (1 3) (0 2) (0 2) (1 2)))
      (next-location model s t :construct? t))
    ;; Probability of metre 3 is 2 / 12
    (5am:is (eq (probability model t '(0 3))
		(probs:in (/ 2 12))))
    ;; and independent of phase
    (5am:is (eq (probability model t '(2 3))
		(probs:in (/ 2 12))))
    ;; Probability of metre 2 is 3 / 12
    (5am:is (eq (probability model t '(0 2))
		(probs:in (/ 3 12))))))
    
;; Test phase-model
(5am:test phase-distribution
  (let* ((probs:*log-space* t) (distribution (mapcar #'probs:in '(.3 .7)))
	 (period (length distribution))
	 (common-period 6)
	 (n (- common-period period))
	 (entropy (probs:entropy distribution))
	 (z (print (models::correction-factor entropy n period)))
	 (new (models::phase-distribution distribution n period z)))
    (print (mapcar #'probs:out (mapcar #'cadr new)))
    (print (probs:out (apply #'probs:add (mapcar #'cadr new)))))"

;; Test n-gram
;;(let* ((model (make-instance 'models::n-gram :n 2))
;;		(l (models::root-location model)))
;;	   (print l)
;;	   (dolist (s '(a b a c a d a b r a))
;;	     (setf l (models::next-location model s l :construct? t))
;;	     (print l))
;;	   (models::distribution model '(b) '(a r)))
	     
	   


(cl:in-package #:features)

;; Test argument and model-accessors
"(f:featurelet e 
    ((a (normal () () '(a)))
     (b (normal () () '(b)))
     (c (normal () (a b) (list (cons a b)))))
  (f:set-model c 'test :model-feature-args (list b) :model-args '(:a 1))
  (format t "~a~%" (funcall (f:model-arguments-accessor c) '(1 2) nil))
  (let* ((ma (f:model-accessor c))
	 (m (funcall ma '(1 2) nil)))
    (format t "a: ~a, b: ~a~%" (a m) (b m))))"
	   
