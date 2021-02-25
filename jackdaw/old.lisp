;; Testing group states

(let* ((l1 (list (make-array '(1 3) :initial-contents '((0 a 1.0))) nil))
		(l2 (list (make-array '(1 3) :initial-contents '((3 b 1.0))) nil))
		(l3 (list (make-array '(2 3) :initial-contents '((1 c 0.5) (2 d 0.5)))
			  (list l1 l2))))
  (gm:group-state-vector l3 :element (make-array '(4))))


(let* ((dataset (md:get-music-objects '(2) nil))
		(cpitch (viewpoints:get-viewpoint 'cpitch))
		(alphabet (print (viewpoints:set-alphabet-from-dataset cpitch dataset)))
		(df (f:phi 'df '(f) ()
			   (lambda (f) 
			     (lambda () 
			       (loop for next in alphabet collect
				    (- next f))))))
		(f (f:phi 'f '(f) '(df)
			  (lambda (f)
			    (lambda (df)
			      (list (+ f df))))
			  :initialisation (lambda () alphabet)
			  :observation #'md:chromatic-pitch))
		(model (gm:make f df)))
	   (gm:set-ppm-features model 'df)
	   (gm:set-observables model 'f)
	   (gm:initialise-ppm-models model)
	   (gm:model-dataset model dataset :construct? t)
	   (setf (f:obs-f f) #'identity)
	   (gm:model-sequence model '(50 51 52 53 54)))

;; Toy example simple one-feature model

(let* ((dataset '((a b r a c a d a b r a)))
		(alphabet (remove-duplicates (apply #'append dataset)))
		(f (f:phi 'f nil nil
			  (lambda ()
			    (lambda ()
			      alphabet))
			  :observation #'identity))
		(model (gm:make f))
		(ppm (ppm:build-model dataset alphabet)))
	   (gm:set-ppm-features model 'f)
	   (gm:set-observables model 'f)
	   (gm:initialise-ppm-models model)
	   (gm:model-dataset model dataset :construct? t)
	   (let ((predictions (ppm:model-sequence ppm '(a b a) :predict? t)))
	     (print (apply #'* (mapcar (lambda (p) 
					 (cadr (assoc (car p) (cadr p)))) predictions))))
	   (multiple-value-bind (states evidence)
	       (gm:model-sequence model '(a b a))
	     (print evidence)))

;;

;; (loop for delta in delta-alphabet if
;;	      (member (+ f delta) f-alphabet) 
;;	    collect delta)))))

;; Toy example delta feature

(viewpoints::define-viewpoint (viewpoints:df viewpoints:derived (viewpoints:cpitch))
    ((viewpoints:events md:music-sequence) element)
  :function (multiple-value-bind (e1 e2)
		(values-list (last viewpoints:events 2))
	      (if (null e2) viewpoints:+undefined+
		  (- (md:chromatic-pitch e2) (md:chromatic-pitch e1)))))

(let* ((dataset '((0 0 2 4)
		  (0 2 4 5)))
       (alphabet '(0 2 4 5))
       (df (f:phi 'df '(f) ()
		  (lambda (f) 
		    (lambda () 
		      (loop for next in alphabet collect
			   (- next f))))))
       (f (f:phi 'f '(f) '(df)
		 (lambda (f)
		   (lambda (df)
		     (list (+ f df))))
		 :initialisation (lambda () alphabet)
		 :observation #'identity))
       (gm (gm:make f df))
       (delta (viewpoints:get-viewpoint 'delta-testing))
       (training-data (viewpoints:viewpoint-sequences delta dataset))
       (ppm (ppm:build-model training-data nil)))
  (viewpoints:set-alphabet-from-dataset delta dataset)
  (ppm:set-alphabet ppm (viewpoints:viewpoint-alphabet delta))
  (let ((predictions (ppm:model-sequence ppm '(0 0 0 0) :predict? t)))
    (format t "PPM prob: ~A~%" 
	    (apply #'* (mapcar (lambda (p) (cadr (assoc (car p) (cadr p)))) 
			       predictions))))
  (gm:set-ppm-features gm 'df)
  (gm:set-observables gm 'f)
  (gm:initialise-ppm-models gm)
  (gm:model-dataset gm dataset :construct? t)
  (format t "GM prob: ~A~%" (gm:model-sequence gm '(0 0 0 0 0))))

;; Toy example inference

(let* ((dataset (mapcar (lambda (s l)
			  (mapcar (lambda (c)
				    (list l c))
				  (coerce s 'list)))
			(list "this is an english sentence featuring a bunch of examples of english spelling without using punctuation or capitalisation."
			      "dit is een nederlandse zin met een aantal woorden die moeten illustreren hoe nederlands typisch wordt gespeld.")
			(list 'en 'nl))))
       ;; TODO
       ))

