(cl:in-package #:latex)

(defgeneric latex-solution-array (events viewpoint-attributes texture &key enumerate interpretations))

(defmethod latex-solution-array ((events md:music-sequence) viewpoint-attributes texture
				 &key (interpretations nil) (enumerate nil))
  (latex-solution-array (coerce 'list events) viewpoint-attributes texture
			:interpretations interpretations
			:enumerate enumerate))

(defmethod latex-solution-array ((events list) viewpoint-attributes texture
				 &key enumerate interpretations)
  (let* ((viewpoints (viewpoints:get-viewpoints viewpoint-attributes)))
    (format nil "\\begin{tabular}{~{~D~}}~%~A~A~A\\end{tabular}" 
	    (loop for x to (length events) collect "l")
	    (if enumerate (enumeration-row (length events)) "")
	    (if (eql texture :grid) (grid-row events) "")
	    (viewpoint-rows events viewpoints 
			    :interpretations interpretations))))

(defun enumeration-row (n)
  (format nil "event index & ~{~A ~^& ~}\\\\~%" (utils:generate-integers 0 (1- n))))

(defun grid-row (grid-events)
  (let ((is-onsets 
	 (viewpoints:viewpoint-sequence (viewpoints:get-viewpoint 'is-onset) grid-events)))
    (flet ((draw-grid-point (is-onset)
	     (if is-onset "$\\bullet$" "$\\circ$")))
      (format nil "\\texttt{is-onset} & ~{~A ~^& ~}\\\\~%" (mapcar #'draw-grid-point is-onsets)))))

(defun viewpoint-elements (viewpoint event-sequence &key interpretation)
  (flet ((viewpoint-element (index)
	   (let ((events (subseq event-sequence 0 (1+ index))))
	     (viewpoints:viewpoint-element viewpoint events :interpretation interpretation))))
    (mapcar #'viewpoint-element (utils:generate-integers 0 (1- (length event-sequence))))))

(defun viewpoint-element (element)
  (format nil "~D"
	  (if (eql element viewpoints:+undefined+)
	      "$\\perp$"
	      element)))

(defgeneric viewpoint-row (events viewpoint &key &allow-other-keys))
(defmethod viewpoint-row (events (viewpoint viewpoints::metrical) &key interpretations)
  (format nil "~{~A~}"
	  (loop for interpretation in interpretations collecting
	       (format nil "\\texttt{~A ~D ~D ~D} & ~{~A ~^& ~}\\\\~%"
		       (viewpoints:viewpoint-name viewpoint)
		       (md:barlength interpretation)
		       (md:pulses interpretation)
		       (md:interpretation-phase interpretation)
		       (mapcar #'viewpoint-element
			       (viewpoint-elements viewpoint events
						   :interpretation interpretation))))))

(defmethod viewpoint-row (events (viewpoint viewpoints:viewpoint) &key &allow-other-keys)
    (format nil "\\texttt{~A} & ~{~A ~^& ~}\\\\~%"
	    (viewpoints:viewpoint-name viewpoint)
	    (mapcar #'viewpoint-element
		    (viewpoint-elements viewpoint events))))

(defun viewpoint-rows (events viewpoints 
			    &key interpretations)
  (format nil "~{~A~}" (mapcar #'(lambda (vp)
				   (viewpoint-row events vp :interpretations interpretations))
			       viewpoints)))

(defun predictions-row (events viewpoint &key interpretation)
    (let* ((sources (viewpoints:get-viewpoints sources))
	 (targets (viewpoints:get-basic-viewpoints targets training-set texture))
	 ;; Obtain event counts per category
	 (category-counts (inference::count-categories training-set texture resolution :use-cache? nil))
	 ;; Extract a list of metrical interpretations
	 (categories (mapcar #'(lambda (category-count) 
				 (md:category-string->metrical-interpretation 
				  (car category-count)))
			     category-counts))
	 (models (inference::make-category-models training-set dataset-id
						  categories targets sources
						  :use-cache? t
						  :texture texture
						  :resolution resolution)))
    (inference::generate-category-predictions categories models event-sequence
					      :resolution resolution :texture texture
					      :keep-prediction-sets t)))

  
			


