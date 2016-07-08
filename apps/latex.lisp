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
	    (if (eql texture :grid) (generate-grid-row events) "")
	    (generate-table-rows events viewpoints 
				 :interpretations interpretations))))

(defun enumeration-row (n)
  (format nil "event index & ~{~A ~^& ~}\\\\~%" (utils:generate-integers 0 (1- n))))

(defun generate-grid-row (grid-events)
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
  (dolist (interpretation interpretations)
    (format nil "\\texttt{~A ~D ~D ~D} & ~{~A ~^& ~}\\\\~%"
	    (viewpoints:viewpoint-name viewpoint)
	    (md:barlength interpretation)
	    (md:pulses interpretation)
	    (md:interpretation-phase interpretation)
	    (mapcar #'viewpoint-element
		    (viewpoint-elements viewpoint events :interpretation interpretation)))))

(defmethod viewpoint-row (events (viewpoint viewpoints:viewpoint) &key &allow-other-keys)
    (format nil "\\texttt{~A} & ~{~A ~^& ~}\\\\~%"
	    (viewpoints:viewpoint-name viewpoint)
	    (mapcar #'viewpoint-element
		    (viewpoint-elements viewpoint events))))

(defun generate-table-rows (events viewpoints 
			    &key interpretations)
  (flet ((draw-viewpoint-row (vp)
	   (flet ((draw-viewpoint-element (element)
		    (format nil "~D"
			    (if (eql element viewpoints:+undefined+)
				"$\\perp$"
				element))))
	     (format nil "\\texttt{~A} & ~{~A ~^& ~}\\\\~%"
		     (viewpoints:viewpoint-name vp)
		     (mapcar #'draw-viewpoint-element
			     (viewpoint-elements vp events :interpretations interpretations))))))
    (format nil "~{~A~}" (mapcar #'draw-viewpoint-row viewpoints))))


