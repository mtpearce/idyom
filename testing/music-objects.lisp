(cl:in-package #:idyom-tests)

(5am:def-suite music-objects)
(5am:in-suite music-objects)

(5am:test make-metrical-interpretation
  "Make metrical interpretation takes any object that has time
signature properties and converts it into a metrical interpretation
by adding a phase and timebase (timebase really necessary?)"
  (let ((ts (make-instance 'md::time-signature
			   :barlength 96
			   :pulses 4)))
    (let ((interpretation (md:make-metrical-interpretation ts)))
      (5am:is (eql (md:barlength interpretation) 96))
      (5am:is (eql (md:pulses interpretation) 4))
      (5am:is (eql (md:interpretation-phase interpretation) 0)))))

(5am:test time-signature->metrical-interpretation
  "time-signature->metrical-interpretation takes the two numerals of a music-
notation time-signature and converts it into a metrical interpretation object.
The naming of this function is potentially confusing because music-objects also has
an object named time-signature, whereas time signature here refers to the two numbers."
  (let ((interpretation (md:time-signature->metrical-interpretation 4 4
								    :phase 4 :timebase 16)))
      (5am:is (eql (md:barlength interpretation) 16))
      (5am:is (eql (md:pulses interpretation) 4))
      (5am:is (eql (md:interpretation-phase interpretation) 4))))

(5am:test (create-interpretations :depends-on time-signature->metrical-interpretation)
  "create-interpretations takes any time-signature object and 
generates a list of possible metrical interpretations by iterating over all
possible phases that fit in the specified resolution."
  (let ((interpretation (md:time-signature->metrical-interpretation 4 4 :timebase 16)))
    (let ((interpretations (md:create-interpretations interpretation 4)))
      (5am:is (eql (length interpretations) 4))
      (5am:is (eql (md:interpretation-phase (first interpretations)) 0))
      (5am:is (eql (md:interpretation-phase (second interpretations)) 4))
      (5am:is (eql (md:interpretation-phase (third interpretations)) 8))
      (5am:is (eql (md:interpretation-phase (fourth interpretations)) 12)))))

(5am:test metre-string->metrical-interpretation
  (let* ((str "(96 4 24 96)")
	 (interpretation (md:metre-string->metrical-interpretation str)))
    (5am:is (eql (md:barlength interpretation) 96))
    (5am:is (eql (md:pulses interpretation) 4))
    (5am:is (eql (md:interpretation-phase interpretation) 24))
    (5am:is (eql (md:timebase interpretation) 96))))
    
(5am:test category-string->metrical-interpretation 
  (let* ((str "(96 4)")
	 (interpretation (md:category-string->metrical-interpretation str)))
    (5am:is (eql (md:barlength interpretation) 96))
    (5am:is (eql (md:pulses interpretation) 4))
    (5am:is (eql (md:interpretation-phase interpretation) 0))))

(5am:test (category-string :depends-on category-string->metrical-interpretation)
  (let* ((str "(96 4)")
	 (interpretation (md:metre-string->metrical-interpretation str))
	 (category-string (md:category-string interpretation)))
    (5am:is (string-equal str category-string))))

(5am:test (metre-string :depends-on metre-string->metrical-interpretation)
  (let* ((str "(96 4 24 96)")
	 (interpretation (md:metre-string->metrical-interpretation str))
	 (metre-string (md:metre-string interpretation)))
    (5am:is (string-equal str metre-string))))

;;;;;;;;;;;; GRID SEQUENCES ;;;;;;;;;;;;;;;;

(5am:test composition->grid
  (let ((composition (create-composition '(4 2 2 4 8 4 8) '(0 0 0 0 0 0 0) :timebase 16))
	(pos-sequence (loop for pos below 16 collecting (* 2 pos)))
	(is-onset-sequence '(t nil t t t nil t nil nil nil t nil t nil nil nil)))
    (let ((grid-sequence (md::composition->grid composition :resolution 8)))
      (5am:is (typep grid-sequence 'md:grid-sequence))
      (5am:is (equal (mapcar #'md:is-onset (coerce grid-sequence 'list))
		     is-onset-sequence))
      (5am:is (equal (mapcar #'md:pos (coerce grid-sequence 'list))
		     pos-sequence)))
    (let ((grid-sequence (md::composition->grid composition :resolution 16)))
      (5am:is (equal (mapcar #'md:is-onset (coerce grid-sequence 'list))
		     (apply #'append (mapcar #'(lambda (is-onset) (list is-onset nil))
					     is-onset-sequence))))
      (5am:is (equal (mapcar #'md:pos (coerce grid-sequence 'list))
		     (apply #'append (mapcar #'(lambda (pos) (list pos (1+ pos)))
					     pos-sequence)))))))

