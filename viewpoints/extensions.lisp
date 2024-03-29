;;;; ======================================================================
;;;; File:       extensions.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2008-10-31 13:08:09 marcusp>
;;;; Time-stamp: <2023-04-20 13:45:44 marcusp>
;;;; ======================================================================

(cl:in-package #:viewpoints) 

(defgeneric set-alphabet-from-dataset (viewpoint dataset))
(defgeneric set-alphabet-from-context (viewpoint events unconstrained))
(defgeneric alphabet->events (viewpoint events))

(defun get-basic-viewpoints (attributes dataset)
  (initialise-basic-viewpoints dataset)
  ;; (set-onset-alphabet nil)
  (get-viewpoints attributes))

(defun initialise-basic-viewpoints (dataset &optional attributes)
  "Initialises the alphabets of the registered basic attributes specified in
*basic-types* to those elements which appear in <dataset>."
  (let ((attributes (if (null attributes) (get-basic-attributes (elt (car dataset) 0)) attributes)))
    (dolist (attribute attributes)
      (handler-case ;; not all basic viewpoints are present in all textures (e.g., articulation is not present in :harmony).
          (set-alphabet-from-dataset (get-viewpoint attribute) dataset)
        (error nil nil)))))

(defun set-onset-alphabet (events)
  (setf (viewpoint-alphabet (get-viewpoint 'onset)) (onset-alphabet events)))

(defmethod set-alphabet-from-dataset ((v viewpoint) dataset)
  "Initialises the alphabet of viewpoint <v> in <dataset>."
  (let ((alphabet '()))
    (dolist (composition dataset)
      (let ((viewpoint-sequence (viewpoint-sequence v composition)))
        (dolist (viewpoint-element viewpoint-sequence)
          (unless (or (undefined-p viewpoint-element)
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

(defmethod set-alphabet-from-context ((v viewpoint) events unconstrained)
  "Sets the alphabet of derived viewpoint <v> based on the set of
sequences created by concatenating the alphabet of the basic viewpoint
from which <v> is derived onto a sequence of events
<events>. <unconstrained> is a list of basic viewpoints being
predicted which assume their full alphabets, otherwise the basic
alphabets are determined on the basis of the values of the final event
in <events>."
  (flet ((get-alphabets (attributes events)
           (let ((alphabets '()))
             (when (consp unconstrained)
               (setq unconstrained (mapcar #'viewpoint-type unconstrained)))
             (dolist (a attributes (reverse alphabets)) ; for each basic type in v's typeset
               (if (or (null unconstrained) (member a unconstrained)) ; if the basic type is unconstrained
                   (if (eql a 'onset)
                       (push (onset-alphabet events) alphabets)
                       (push (viewpoint-alphabet (get-viewpoint a)) ; add the alphabet of the basic type to alphabets
                             alphabets))
                   (push (list (md:get-attribute (car (last events)) a)) ; otherwise just add the value at the last elt
                         alphabets))))))
    (let* ((derived-alphabet '())
           (attributes (viewpoint-typeset v))
           (e (md:copy-event (car (last events))))
           (context (butlast events))
           (basic-alphabet 
            (apply #'utils:cartesian-product 
                   (get-alphabets attributes events))))
      ;; (format t "~&type = ~A; basic-alphabet = ~A; typeset = ~A~%" (viewpoint-type v) basic-alphabet attributes)  
      (dolist (d basic-alphabet)
        (mapc #'(lambda (element attribute)
                  (md:set-attribute e attribute element))
              d attributes)
        (let ((ve (viewpoint-element v (append context (list e)))))
          (unless (or (undefined-p ve) (member ve derived-alphabet :test #'equal))
            (push ve derived-alphabet))))
      ;; (format t "~&type = ~A; alphabet = ~A~%" (viewpoint-type v) derived-alphabet)  
      (setf (viewpoint-alphabet v) (nreverse derived-alphabet)))))
          

(defmethod alphabet->events ((v viewpoint) (events md:music-composition))
  (alphabet->events v (coerce events 'list)))

(defmethod alphabet->events ((b basic) events)
  (let ((alphabet (viewpoint-alphabet b))
        (event (car (last events)))
        ;(previous-events (butlast events))
        (type (viewpoint-type b)))
    (mapcar #'(lambda (viewpoint-element)
                (let ((e (md:copy-event event)))
                  (md:set-attribute e type viewpoint-element)
                  e))
            alphabet)))

(defmethod alphabet->events ((o onset) events)
  (let* ((event (car (last events)))
         (onset-alphabet (onset-alphabet events)))
    (mapcar #'(lambda (viewpoint-element)
                (let ((e (md:copy-event event)))
                  (md:set-attribute e 'onset viewpoint-element)
                  e))
            onset-alphabet)))
 
(defun onset-alphabet (events)
  ;; Based on DELTAST alphabet a la Conklin
  ;;   (let ((deltast-alphabet (viewpoint-alphabet (get-viewpoint 'deltast))))
  ;;     (if (null previous-events) deltast-alphabet
  ;;         (let* ((last-event (car (reverse previous-events)))
  ;;                (onset (+ (md:get-attribute last-event :onset)
  ;;                          (md:get-attribute last-event :dur))))
  ;;           (mapcar #'(lambda (a) (+ onset a)) deltast-alphabet)))))
  ;; Based on BIOI alphabet
  (if (eq (length events) 1)
      ;; The first event should have a probability of one as far as onset is concerned.
      ;; This is achieved here indirectly by setting the onset alphabet to only the actual
      ;; onset of the event.
      (viewpoint-sequence (get-viewpoint 'onset) events)
      (let ((bioi-alphabet (remove nil (viewpoint-alphabet (get-viewpoint 'bioi)))))
	;; For subsequent events, the onset alphabet assumes the last onset plus all
	;; values in the BIOI alphabet, except zero.
	(let* ((last-event (car (reverse (butlast events))))
               (onset (md:get-attribute last-event 'onset)))
          (loop for bioi in bioi-alphabet if (not (eq bioi 0)) collect (+ onset bioi))))))

(defmethod alphabet->events ((d derived) events)
  ;;TODO: make this work for all elements in typeset 
  (let ((typeset (get-viewpoint (car (viewpoint-typeset d)))))
    (alphabet->events typeset events)))
                    
(defmethod alphabet->events ((l linked) events)
  (let ((event (car (last events))))
    (labels ((get-alphabet (attribute)
               (if (eql attribute 'onset) (onset-alphabet events)
                   (viewpoint-alphabet (get-viewpoint attribute))))
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

(defgeneric viewpoint-name-string (v)
  (:documentation "Returns a sanitised string identifying the viewpoint <v>.
Intended for data output purposes."))

(defmethod viewpoint-name-string ((v symbol))
  (string-downcase (symbol-name v)))

(defmethod viewpoint-name-string ((v list))
  (string-downcase
   (format nil "~{~A~^-x-~}"
          (mapcar #'symbol-name v))))
