;; Attempting to recreate problem with saving and loading models

;; This does NOT work
;; But when construct? is NIL in the second model-dataset, it does
;; Also, when the same model is used (rather than saving and re-loading it)
;; it DOES work.
(with-open-file (s "/home/bastiaan/projects/idyom/jackdaw/commands/test-dataset"
			    :direction :input)
	   (let* ((model (make-instance 'generative-models::phase-model))
		  (attributes (list "meter" "pickup" "ioi" "period"))
		  (dataset (list (read s)))
		  (writer (make-instance 'gm::full-writer :graph model :destination t)))
	     (flet ((set-attribs (model)
		      (dolist (name attributes)
			(let* ((kw (intern (string-upcase name) :keyword))
			       (obs-slot
				(find-symbol (format nil "OBSERVE-~a"
						     (string-upcase name))
					     :generative-models)))
			  (setf (slot-value model obs-slot)
				(lambda (e) (getf e kw)))))))
	       (set-attribs model)
	       (gm::flush-cache model)
	       (gm::set-domains model dataset)
	       (gm::model-dataset model dataset :writers (list writer)
				  :predict? t :construct? t)
	       (with-open-file (s "temptemp.lisp" :direction :output
				  :if-exists :supersede
				  :if-does-not-exist :create)
		 (print (print (gm::printable model)) s))
	       (let ((model (make-instance 'gm::phase-model))
		     (printable))
		 (with-open-file (s "temptemp.lisp" :direction :input)
		   (setf printable (read s))
		   (gm::from-printable printable model))
		 (print (gm::printable model)) (set-attribs model)
		 (gm::flush-cache model)
		 (gm::model-dataset model dataset :writers (list writer)
				    :predict? t :construct? t)))))


