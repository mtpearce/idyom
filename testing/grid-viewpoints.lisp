(cl:in-package #:idyom-tests)

(5am:def-suite viewpoints)
(5am:in-suite viewpoints)

(defmacro with-shc-viewpoint-elements (viewpoint &rest body)
  `(let* ((composition (create-composition '(4 2 2 4 8 4 8) '(0 0 0 0 0 0 0) :timebase 16))
	  (grid-sequence (md::composition->grid composition :resolution 8))
	  (elements (viewpoints:viewpoint-sequence
		     (viewpoints:get-viewpoint ,viewpoint)
		     grid-sequence)))
     ,@body))

(5am:test (pos :depends-on composition->grid)
  (with-shc-viewpoint-elements 'pos
    (5am:is (equal elements '(0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)))))

(5am:test (is-onset :depends-on composition->grid)
  (with-shc-viewpoint-elements 'is-onset
  (5am:is (equal sequence '(t nil t t t nil t nil nil nil t nil t nil nil nil)))))

(5am:test metrical-position :depends-on (and 'is-onset 'pos 'time-signature->metrical-interpretation)
  (let* ((composition (create-composition '(4 2 2 4 8 4 8) '(0 0 0 0 0 0 0) :timebase 16))
	 (grid (md::composition->grid composition :resolution 8))
	 (viewpoint (viewpoints:get-viewpoint 'metrical-position))
	 (metre (md:time-signature->metrical-interpretation 4 4 :phase 4 :timebase 16))
	 (sequence (viewpoints:viewpoint-sequence viewpoint grid :interpretation metre)))
    (5am:is (equal sequence '(12 14 0 2 4 6 8 10 12 14 0 2 4 6 8 10)))))
