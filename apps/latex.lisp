(cl:in-package #:latex)

(defgeneric latex-solution-array (events viewpoint-attributes texture &key interpretation highlight-test))

(defmethod latex-solution-array ((events md:event-sequence) viewpoint-attributes texture
				 &key (interpretation nil) (highlight-test nil))
  (latex-solution-array (coerce 'list events)))

(defmethod latex-solution-array ((events list) viewpoint-attributes texture
				 &key (interpretation nil) (highlight-test nil))
  (let* ((viewpoints (viewpoints:get-viewpoints viewpoint-attributes)))
    (format nil "\\begin{tabular}{~{~D~}}~%~A~A\\end{tabular}" 
	    (loop for x to (length events) collect "l")
	    (if (eql texture :grid) (generate-grid-row events) "")
	    (generate-table-rows events viewpoints 
				 :interpretation interpretation
				 :highlight-test highlight-test))))

(defun generate-grid-row (grid-events)
  (let ((is-onsets 
	 (viewpoints:viewpoint-sequence (viewpoints:get-viewpoint 'is-onset) grid-events)))
    (flet ((draw-grid-point (is-onset)
	     (if is-onset "$\\bullet$" "$\\circ$")))
      (format nil "\\texttt{is-onset} & ~{~A ~^& ~}\\\\~%" (mapcar #'draw-grid-point is-onsets)))))

(defun generate-table-rows (events viewpoints 
			    &key (interpretation nil) 
			      (highlight-test nil))
  (flet ((draw-viewpoint-row (vp-index)
      	   (flet ((draw-viewpoint-element (event-index)
		    (let* ((events-so-far (subseq events 0 (1+ event-index)))
			   (element (aref (mvs:get-event-array (mvs:make-mvs nil viewpoints nil) 
							       events-so-far
							       :interpretation interpretation)
					  vp-index))
			   (highlight (if highlight-test (apply highlight-test events-so-far) nil))
			   (formatted-element (format nil "~D" (if (eql element viewpoints:+undefined+) "$\\perp$" element))))
		      (if highlight (format nil "\\textcolor{red}{~A}" formatted-element) formatted-element))))
	     (format nil "\\texttt{~A} & ~{~A ~^& ~}\\\\~%"
		     (viewpoints:viewpoint-name (nth vp-index viewpoints))
		     (mapcar #'draw-viewpoint-element (range (length events)))))))
    (format nil "~{~A~}" (mapcar #'draw-viewpoint-row (range (length viewpoints))))))
