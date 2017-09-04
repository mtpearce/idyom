(cl:in-package #:utils)

(5am:def-suite utils)
(5am:in-suite utils)

(5am:test set-equal
  (5am:is (not (set-equal nil nil)))
  (5am:is (set-equal '(a) '(a)))
  (5am:is (set-equal '(a b) '(b a)))
  (5am:is (not (set-equal '(a c) '(b a))))
  (5am:is (not (set-equal '() '(b a))))
  (5am:is (not (set-equal '(a b) '(b a c))))
  (5am:is (set-equal (set-equal '(c a b) '(b c a))
		     '(a b c))))

