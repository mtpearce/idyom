;; Metrical viewpoint code

(cl:in-package #:music-data)

(defclass metrical-interpretation (time-signature music-object)
  ((meter-phase :initarg :phase :accessor meter-phase)
   (resolution :initarg :resolution :accessor resolution)))

(defgeneric make-metrical-interpretation (time-signature resolution &key phase))
(defmethod make-metrical-interpretation ((ts time-signature) resolution 
					 &key (timebase 96) (phase 0))
  (make-instance 'metrical-interpretation 
		 :barlength (barlength ts)
		 :pulses (pulses ts)
		 :phase phase 
		 :timebase (or (timebase ts) timebase)
		 :resolution resolution))

(defun time-signature->metrical-interpretation (numerator denominator 
						&key 
						  (phase 0)
						  (resolution 16)
						  (timebase 96))
  (make-instance 'metrical-interpretation 
		 :barlength (* (/ numerator denominator) timebase)
		 :pulses numerator
		 :phase phase
		 :timebase timebase
		 :resolution resolution))
  
  
(defgeneric meter-string (metrical-interpretation &key &allow-other-keys))
(defmethod meter-string ((m metrical-interpretation) &key &allow-other-keys)
  (format nil "(~A ~A ~A ~A)" 
	  (barlength m) (pulses m) (meter-phase m)  (timebase m)))
(defmethod meter-string ((m time-signature) &key (phase 0))
  (format nil "(~A ~A ~A ~A)" 
	  (barlength m) (pulses m) phase  (timebase m)))

(defgeneric category-string (time-signature))
(defmethod category-string ((ts time-signature))
  "Return a string representation containing the metrical category of a 
time signature."
  (format nil "(~A ~A)" 
	  (barlength m) (pulses m)))

(defgeneric create-interpretations (category resolution))
(defmethod create-interpretations (category resolution)
  "Return a list of interpretations derived from a category."
  (let ((period (md:meter-period category)))
    (flet ((make-interpretation (phase)
	     (md:make-metrical-interpretation category resolution 
					      :phase phase)))
      ;; Create an interpretation for this category in each phase
      (mapcar #'make-interpretation (utils:generate-integers 0 (1- period))))))

(defun meter-string->metrical-interpretation (meter-string resolution)
  (multiple-value-bind (values)
      (read-from-string meter-string)
    (make-instance 'md:metrical-interpretation
		   :barlength (first values)
		   :pulses (second values)
		   :phase (third values)
		   :resolution resolution
		   :timebase (fourth values))))

(defgeneric beat-division (meter))
(defmethod beat-division ((m metrical-interpretation))
  (let* ((beat-duration (/ (barlength m)
			  (pulses m)))
	(beat-division (/ (timebase m) beat-duration)))
    beat-division))

(defgeneric meter-period (meter))
(defmethod meter-period ((m metrical-interpretation))
  (rescale (barlength m) (resolution m) (timebase m)))
