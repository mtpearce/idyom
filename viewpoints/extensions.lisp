;;;; ======================================================================
;;;; File:       extensions.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2008-10-31 13:08:09 marcusp>
;;;; Time-stamp: <2014-11-27 11:16:43 marcusp>
;;;; ======================================================================

(cl:in-package #:viewpoints) 

(defparameter *alphabet-dir* 
  (ensure-directories-exist
   (merge-pathnames "data/alphabets/" (utils:ensure-directory utils:*root-dir*))))

(defparameter grid-basic-viewpoints '(:is-onset :pos :onset))
(defparameter melody-basic-viewpoints '(:ARTICULATION :COMMA :VOICE :ORNAMENT :DYN :PHRASE
 :BIOI :DELTAST :ACCIDENTAL :MPITCH :CPITCH :BARLENGTH :PULSES :TEMPO :MODE
 :KEYSIG :DUR :ONSET))

(defgeneric set-alphabet-from-dataset (viewpoint dataset))
(defgeneric set-alphabet-from-context (viewpoint events unconstrained 
				       &key interpretation))
(defgeneric alphabet->events (viewpoint events texture))

(defun get-basic-viewpoints (attributes dataset texture)
  (initialise-basic-viewpoints dataset texture)
  (set-onset-alphabet nil texture)
  ; Only applicable for :grid textures
  (when (eql texture :grid)
    (set-pos-alphabet nil))
  (get-viewpoints attributes))

(defun initialise-basic-viewpoints (dataset texture)
  "Initialises the alphabets of the relevant basic types specified in
*basic-types* to those elements which appear in <dataset>."
  (dolist (attribute (get-basic-types nil)) ;; (elt (car dataset) 0)))
    (unless (or (and (eql texture :grid) 
		     (not (member attribute grid-basic-viewpoints)))
		(and (eql texture :melody)
		     (not (member attribute melody-basic-viewpoints))))
      (set-alphabet-from-dataset (get-viewpoint attribute) dataset))))

(defun set-onset-alphabet (context texture)
  (setf (viewpoint-alphabet (get-viewpoint 'onset)) (onset-alphabet context texture)))

(defun set-pos-alphabet (context)
  (setf (viewpoint-alphabet (get-viewpoint 'pos)) (pos-alphabet context)))

(defmethod set-alphabet-from-dataset ((v viewpoint) (dataset list))
  (let ((alphabet '()))
    (dolist (composition dataset)
      (let ((viewpoint-sequence (viewpoint-sequence v composition)))
	(dolist (viewpoint-element viewpoint-sequence)
	  (unless (or (and (viewpoint-equal v (get-viewpoint 'bioi))
			   (< viewpoint-element 0))
		      (undefined-p viewpoint-element)
		      (member viewpoint-element alphabet :test #'equal))
	    (push viewpoint-element alphabet)))))
    (let ((sorted-alphabet
	   (sort alphabet #'(lambda (x y)
			      (cond ((and (numberp x) (numberp y))
				     (< x y))
				    ((and (listp x) (listp y))
				     (< (car x) (car y)))
				    (t nil))))))
      (setf (viewpoint-alphabet v) sorted-alphabet))))

(defmethod set-alphabet-from-dataset ((v viewpoint) (dataset promises:promise))
  "Initialises the alphabet of viewpoint <v> in <dataset>."
  (let ((filename (format nil "~A~A-~A" *alphabet-dir* (symbol-name (type-of v)) (promises:get-identifier dataset))))
    (unless (utils:file-exists filename)
      (let ((dataset (promises:retrieve dataset)))
	(utils:write-object-to-file (set-alphabet-from-dataset v dataset)
				    filename)
	(format t "Written alphabet for ~A to file ~A.~%"
		(symbol-name (type-of v)) filename)))))
  
(defmethod set-alphabet-from-context ((v viewpoint) events unconstrained 
				      &key 
					(interpretation nil))					
  "Sets the alphabet of derived viewpoint <v> based on the set of
sequences created by concatenating the alphabet of the basic viewpoint
from which <v> is derived onto a sequence of events
<events>. <unconstrained> is a list of basic viewpoints being
predicted which assume their full alphabets, otherwise the basic
alphabets are determined on the basis of the values of the final event
in <events>."
  (flet ((get-alphabets (attributes)
           (let ((alphabets '()))
             (when (consp unconstrained)
               (setq unconstrained (mapcar #'viewpoint-type unconstrained)))
             (dolist (a attributes (reverse alphabets))
               (if (or (null unconstrained) (member a unconstrained))
		   (push (viewpoint-alphabet (get-viewpoint a)) 
			 alphabets)
                   (push (list (md:get-attribute (car (last events)) a))
                         alphabets))))))
    (let* ((derived-alphabet '())
           (attributes (viewpoint-typeset v))
           (e (md:copy-event (car (last events))))
           (context (butlast events))
           (basic-alphabet 
            (apply #'utils:cartesian-product 
                   (get-alphabets attributes))))
      (dolist (d basic-alphabet)
        (mapc #'(lambda (element attribute) 
                  (md:set-attribute e attribute element))
              d attributes)
        (let ((ve (viewpoint-element v (append context (list e)) :interpretation interpretation)))
          (unless (or (undefined-p ve) (member ve derived-alphabet :test #'equal))
            (push ve derived-alphabet))))
      ;;(format t "~&type = ~A; alphabet = ~A~%" (viewpoint-type v) derived-alphabet) ; 
      (setf (viewpoint-alphabet v) (nreverse derived-alphabet)))))
          

(defmethod alphabet->events ((v viewpoint) (events md:music-composition) texture)
  (alphabet->events v (coerce events 'list) texture))

(defmethod alphabet->events ((b basic) events texture)
  (let ((alphabet (viewpoint-alphabet b))
        (event (car (last events)))
        ;(previous-events (butlast events))
        (type (viewpoint-type b)))
    (mapcar #'(lambda (viewpoint-element)
                (let ((e (md:copy-event event)))
                  (md:set-attribute e type viewpoint-element)
                  e))
            alphabet)))

(defmethod alphabet->events ((o onset) events texture)
  (let* ((event (car (last events)))
         (previous-events (butlast events))
         (onset-alphabet (onset-alphabet previous-events texture)))
    (mapcar #'(lambda (viewpoint-element)
                (let ((e (md:copy-event event)))
                  (md:set-attribute e 'onset viewpoint-element)
                  e))
            onset-alphabet)))
 
(defmethod alphabet->events ((p pos) events texture)
  (let* ((event (car (last events)))
         (previous-events (butlast events))
         (pos-alphabet (pos-alphabet previous-events)))
    (mapcar #'(lambda (viewpoint-element)
                (let ((e (md:copy-event event)))
                  (md:set-attribute e 'pos viewpoint-element)
                  e))
            pos-alphabet)))

(defun onset-alphabet (previous-events texture)
  ;; Based on DELTAST alphabet
  ;;   (let ((deltast-alphabet (viewpoint-alphabet (get-viewpoint 'deltast))))
  ;;     (if (null previous-events) deltast-alphabet
  ;;         (let* ((last-event (car (reverse previous-events)))
  ;;                (onset (+ (md:get-attribute last-event :onset)
  ;;                          (md:get-attribute last-event :dur))))
  ;;           (mapcar #'(lambda (a) (+ onset a)) deltast-alphabet)))))
  ;; Based on BIOI alphabet 
  (cond 
    ((eql texture :melody)
     (let ((bioi-alphabet (remove nil (viewpoint-alphabet (get-viewpoint 'bioi)))))
      (if (null previous-events) bioi-alphabet
	  (let* ((last-event (car (reverse previous-events)))
		 (onset (md:get-attribute last-event 'onset)))
	    (mapcar #'(lambda (a) (+ onset a)) bioi-alphabet)))))
    ((eql texture :grid)
     (if (null previous-events) '(0)
	 (let* ((last-event (car (reverse previous-events)))
		(last-position (md:get-attribute last-event 'pos))
		(resolution (md:get-attribute last-event 'resolution))
		(timebase (md:get-attribute last-event 'timebase))
		(position (* (1+ last-position) (/ timebase resolution))))
	  (list nil position))))))

     

(defun pos-alphabet (previous-events)
  (if (null previous-events)
      '(0)
      (let* ((last-event (car (reverse previous-events)))
	     (pos (md:get-attribute last-event 'pos))
	     (resolution (md:resolution last-event))
	     (timebase (md:timebase last-event)))
	(list (+ pos (/ timebase resolution))))))
	

(defmethod alphabet->events ((d derived) events texture)
  ;;TODO: make this work for all elements in typeset 
  (let ((typeset (get-viewpoint (car (viewpoint-typeset d)))))
    (alphabet->events typeset events texture)))
                    
(defmethod alphabet->events ((l linked) events texture)
  (let ((event (car (last events)))
        (previous-events (butlast events)))
    (labels ((get-alphabet (attribute)
               (cond
		 ((eql attribute 'onset) (onset-alphabet previous-events texture))
		 ((eql attribute 'pos) (pos-alphabet previous-events))
		 (t (viewpoint-alphabet (get-viewpoint attribute)))))
             (get-alphabets (attributes)
               (mapcar #'(lambda (a) (get-alphabet a)) attributes))
             (get-events (&rest attributes)
               (let* ((alphabets (get-alphabets attributes))
                      (alphabet (apply #'utils:cartesian-product alphabets)))
                 (mapcar #'(lambda (viewpoint-element)
                             (let ((e (md:copy-event event)))
                               (mapc #'(lambda (element attribute)
                                         (md:set-attribute e attribute element))
                                     viewpoint-element attributes)
                               e))
                         alphabet))))
      (apply #'get-events (viewpoint-typeset l)))))


