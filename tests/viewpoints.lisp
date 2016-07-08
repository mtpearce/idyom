(cl:in-package #:tests)

(defmacro with-shc-grid ((&optional (resolution 8))  &rest body)
  `(let* ((composition (create-composition '(4 2 2 4 8 4 8) '(0 0 0 0 0 0 0) :timebase 16))
	  (grid (md::composition->grid composition :resolution ,resolution)))
     ,@body))

(defmacro with-shc-viewpoint-elements ((viewpoint &optional (resolution 8)) &rest body)
  `(with-shc-grid (,resolution)
      (let ((elements (viewpoints:viewpoint-sequence
		       (viewpoints:get-viewpoint ,viewpoint)
			grid)))
	,@body)))

(defmacro with-shc-metrical-viewpoint-elements ((viewpoint metre &optional (resolution 8)) &rest body)
  `(with-shc-grid (,resolution)
      (let ((elements (viewpoints:viewpoint-sequence
		       (viewpoints:get-viewpoint ,viewpoint)
			grid :interpretation ,metre)))
	,@body)))

(def-suite viewpoints)
(in-suite viewpoints)

(test (pos :depends-on composition->grid)
  (with-shc-viewpoint-elements ('pos)
    (is (equal elements '(0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30))))
  (with-shc-viewpoint-elements ('pos 16)
    (is (equal elements '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
			  19 20 21 22 23 24 25 26 27 28 29 30 31)))))

(test (is-onset :depends-on composition->grid)
  (with-shc-viewpoint-elements ('is-onset)
    (is (equal elements '(t nil t t t nil t nil nil nil t nil t nil nil nil)))))

(test (metrical-position
  :depends-on (and . (pos is-onset time-signature->metrical-interpretation)))
  (let ((metre (md:time-signature->metrical-interpretation 4 4 :phase 4 :timebase 16)))
    (with-shc-metrical-viewpoint-elements
	('metrical-position metre)
      (is (equal elements '(12 14 0 2 4 6 8 10 12 14 0 2 4 6 8 10))))))

(test (metrical-accent
  :depends-on (and . (pos is-onset time-signature->metrical-interpretation)))
  (let ((metre (md:time-signature->metrical-interpretation 4 4 :phase 4 :timebase 16)))
    (with-shc-metrical-viewpoint-elements
	('metrical-accent metre)
      (is (equal elements '(4 3 5 3 4 3 5 3 4 3 5 3 4 3 5 3))))))
