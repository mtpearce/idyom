;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       viewpoint-extensions.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2008-10-31 13:08:09 marcusp>
;;;; Time-stamp: <2013-04-17 15:50:50 jeremy>
;;;; ======================================================================

(cl:in-package #:viewpoints) 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((symbols 
         '(set-alphabet-from-context set-alphabet-from-dataset
           alphabet->events get-basic-viewpoints)))
    (dolist (s symbols)
      (export s (find-package :viewpoints)))))

(defgeneric set-alphabet-from-dataset (viewpoint dataset))
(defgeneric set-alphabet-from-context (viewpoint events unconstrained))
(defgeneric alphabet->events (viewpoint events))

(defun get-basic-viewpoints (attributes dataset)
  (initialise-basic-viewpoints dataset)
  (get-viewpoints attributes))

(defun initialise-basic-viewpoints (dataset)
  "Initialises the alphabets of the basic types specified in
*basic-types* to those elements which appear in <dataset>."
  (dolist (attribute *basic-types*)
    (set-alphabet-from-dataset (get-viewpoint attribute) dataset)))

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
  "Sets the alphabet of derived viewpoint <derived> based on the set
of sequences created by concatenating the alphabet of basic viewpoint
<basic> onto a sequence of events <events>. <unconstrainted> is a list
of basic viewpoints being predicted which assume their full alphabets,
otherwise the basic alphabets are determined on the basis of the
values of the final event in <events>."
  ;;(print unconstrained)
  (flet ((get-alphabets (attributes context)
           (let ((alphabets '()))
             (when (consp unconstrained)
               (setq unconstrained (mapcar #'viewpoint-type unconstrained)))
             (dolist (a attributes (reverse alphabets))
               (if (or (null unconstrained) (member a unconstrained))
                   (if (eql a 'onset)
                       (push (onset-alphabet context) alphabets)
                       (push (viewpoint-alphabet (get-viewpoint a)) 
                             alphabets))
                   (push (list (md:get-attribute (car (last events)) a))
                         alphabets))))))
    (let* ((alphabet '())
           (attributes (viewpoint-typeset v))
           (e (md:copy-event (car (last events))))
           (context (butlast events))
           (derived-alphabet 
            (apply #'utils:cartesian-product 
                   (get-alphabets attributes context))))
      ;;(print (list attributes derived-alphabet))
      (dolist (d derived-alphabet)
        ;;(print (list d attributes))
        (mapc #'(lambda (element attribute) 
                  (md:set-attribute e attribute element))
              d attributes)
        ;;(print (list "cpitch" (md:get-attribute e :cpitch) "bioi" (md:get-attribute e :bioi)))
        ;;(print (list "event" (type-of e)))
        (let ((ve (viewpoint-element v (append context (list e)))))
          ;;(print (list "viewpoint-element" v (append context (list e)) "=>" ve))
          (unless (or (undefined-p ve) (member ve alphabet :test #'equal))
            (push ve alphabet))))
      ;;(format t "~&type = ~A; alphabet = ~A~%" (viewpoint-type v) alphabet)
      (setf (viewpoint-alphabet v) (nreverse alphabet)))))
          
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
         (previous-events (butlast events))
         (onset-alphabet (onset-alphabet previous-events)))
    (mapcar #'(lambda (viewpoint-element)
                (let ((e (md:copy-event event)))
                  (md:set-attribute e 'onset viewpoint-element)
                  e))
            onset-alphabet)))
 
(defun onset-alphabet (previous-events)
  ;; Based on BIOI alphabet 
  (let ((bioi-alphabet (remove (viewpoint-alphabet (get-viewpoint 'bioi)) nil)))
    (if (null previous-events) bioi-alphabet
        (let* ((last-event (car (reverse previous-events)))
               (onset (md:get-attribute last-event 'onset)))
          (mapcar #'(lambda (a) (+ onset a)) bioi-alphabet)))))

(defmethod alphabet->events ((d derived) events)
  ;;TODO: make this work for all elements in typeset 
  (let ((typeset (get-viewpoint (car (viewpoint-typeset d)))))
    (alphabet->events typeset events)))
                    
(defmethod alphabet->events ((l linked) events)
  (let ((event (car (last events)))
        (previous-events (butlast events)))
    (labels ((get-alphabet (attribute)
               (if (eql attribute 'onset) (onset-alphabet previous-events)
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



(defun strip-until-true (test-viewpoint events)
  "Return the longest prefix of the list EVENTS such that
TEST-VIEWPOINT returns true (1 rather than 0)."
  (cond ((null events) '())
        ((undefined-p (viewpoint-element test-viewpoint events))
         (strip-until-true test-viewpoint (butlast events)))
        ((= (viewpoint-element test-viewpoint events) 1) events)
        (t (strip-until-true test-viewpoint (butlast events)))))
