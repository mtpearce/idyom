(cl:in-package #:tests)

(def-suite music-objects)
(in-suite music-objects)

(test make-metrical-interpretation
  "Make metrical interpretation takes any object that has time
signature properties and converts it into a metrical interpretation
by adding a phase and timebase (timebase really necessary?)"
  (let ((ts (make-instance 'md::time-signature
			   :barlength 96
			   :pulses 4)))
    (let ((interpretation (md:make-metrical-interpretation ts)))
      (is (eql (md:barlength interpretation) 96))
      (is (eql (md:pulses interpretation) 4))
      (is (eql (md:interpretation-phase interpretation) 0)))))

(test time-signature->metrical-interpretation
  "time-signature->metrical-interpretation takes the two numerals of a music-
notation time-signature and converts it into a metrical interpretation object.
The naming of this function is potentially confusing because music-objects also has
an object named time-signature, whereas time signature here refers to the two numbers."
  (let ((interpretation (md:time-signature->metrical-interpretation 4 4
								    :phase 4 :timebase 16)))
      (is (eql (md:barlength interpretation) 16))
      (is (eql (md:pulses interpretation) 4))
      (is (eql (md:interpretation-phase interpretation) 4))))

(test (create-interpretations :depends-on time-signature->metrical-interpretation)
  "create-interpretations takes any time-signature object and 
generates a list of possible metrical interpretations by iterating over all
possible phases that fit in the specified resolution."
  (let ((interpretation (md:time-signature->metrical-interpretation 4 4 :timebase 16)))
    (let ((interpretations (md:create-interpretations interpretation 4)))
      (is (eql (length interpretations) 4))
      (is (eql (md:interpretation-phase (first interpretations)) 0))
      (is (eql (md:interpretation-phase (second interpretations)) 4))
      (is (eql (md:interpretation-phase (third interpretations)) 8))
      (is (eql (md:interpretation-phase (fourth interpretations)) 12)))))

(test metre-string->metrical-interpretation
  (let* ((str "(72 3 96 24)")
	 (interpretation (md:metre-string->metrical-interpretation str)))
    (is (eql (md:barlength interpretation) 72))
    (is (eql (md:pulses interpretation) 3))
    (is (eql (md:interpretation-phase interpretation) 24))
    (is (eql (md:timebase interpretation) 96))))
    
(test category-string->metrical-interpretation 
  (let* ((str "(72 3 96)")
	 (interpretation (md:category-string->metrical-interpretation str)))
    (is (eql (md:barlength interpretation) 72))
    (is (eql (md:pulses interpretation) 3))
    (is (eql (md:timebase interpretation) 96))
    (is (eql (md:interpretation-phase interpretation) 0))))

(test (category-string :depends-on category-string->metrical-interpretation)
  (let* ((str "(72 3 96)")
	 (category (md:category-string->metrical-interpretation str))
	 (category-string (md:category-string category
					      (md:timebase category))))
    (is (string-equal str category-string))))

(test (metre-string :depends-on metre-string->metrical-interpretation)
  (let* ((str "(72 3 96 24)")
	 (interpretation (md:metre-string->metrical-interpretation str))
	 (metre-string (md:metre-string interpretation)))
    (is (string-equal str metre-string))))

;;;;;;;;;;;; GRID SEQUENCES ;;;;;;;;;;;;;;;;

(test composition->grid
  (let ((composition (create-composition '(4 2 2 4 8 4 8) '(0 0 0 0 0 0 0) :timebase 16))
	(pos-sequence (loop for pos below 16 collecting (* 2 pos)))
	(is-onset-sequence '(t nil t t t nil t nil nil nil t nil t nil nil nil)))
    (let ((grid-sequence (md::composition->grid composition :resolution 8)))
      (is (typep grid-sequence 'md:grid-sequence))
      (is (equal (mapcar #'md:is-onset (coerce grid-sequence 'list))
		     is-onset-sequence))
      (is (equal (mapcar #'md:pos (coerce grid-sequence 'list))
		     pos-sequence)))
    (let ((grid-sequence (md::composition->grid composition :resolution 16)))
      (is (equal (mapcar #'md:is-onset (coerce grid-sequence 'list))
		     (apply #'append (mapcar #'(lambda (is-onset) (list is-onset nil))
					     is-onset-sequence))))
      (is (equal (mapcar #'md:pos (coerce grid-sequence 'list))
		     (apply #'append (mapcar #'(lambda (pos) (list pos (1+ pos)))
					     pos-sequence)))))))

