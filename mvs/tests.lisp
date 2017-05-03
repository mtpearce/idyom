;;;; ======================================================================
;;;; File:       tests.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-05-03 10:54:10 peter>                            
;;;; Time-stamp: <2017-05-03 11:33:08 peter>                           
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; This code defines tests for the mvs package.

(cl:in-package #:mvs)

(5am:def-suite mvs)
(5am:in-suite mvs)

(5am:def-suite mapping :in mvs)
(5am:in-suite mapping)

(defun old-mapping (derived-viewpoint basic-viewpoint events)
  "Old function for mapping derived viewpoints to basic viewpoints."
  (let* ((derived-alphabet (viewpoint-alphabet derived-viewpoint))
         (continuations (viewpoints:alphabet->events basic-viewpoint events))
         (mappings '()))
    (dolist (derived-element derived-alphabet mappings)
      (let ((mapping '()))
        (dolist (continuation continuations)
          (let* ((temp-comp (append (butlast events) (list continuation)))
                 (viewpoint-element
                  (viewpoint-element derived-viewpoint temp-comp))
                 (basic-element (viewpoint-element basic-viewpoint temp-comp)))
            (when (viewpoints:viewpoint-element-equal basic-viewpoint
                                           derived-viewpoint
                                           viewpoint-element
                                           derived-element)
              (push basic-element mapping))))
        (unless (null mapping)
          (push (list derived-element mapping) mappings))))))

(defparameter *ex-1-basic-viewpoint* (make-instance 'viewpoints::h-cpitch
						:alphabet (list '(0 4 7) '(0 3 7) '(2 6 9) '(2 5 9))))
(defparameter *ex-1-derived-viewpoint* (make-instance 'viewpoints::h-bass-cpitch :alphabet (list 0 2)))

(5am:test mapping-ex-1
  (5am:is (equal (sort (old-mapping *ex-1-derived-viewpoint* *ex-1-basic-viewpoint*
				    (coerce (viewpoints:harm-seq '((0 4 7))) 'list))
		       #'(lambda (x y) (< (car x) (car y))))
		 (sort (mapping *ex-1-derived-viewpoint* *ex-1-basic-viewpoint*
				(coerce (viewpoints:harm-seq '((0 4 7))) 'list))
		       #'(lambda (x y) (< (car x) (car y)))))))
  

(defparameter *ex-2-basic-viewpoint* (make-instance 'viewpoints::h-cpitch
						    :alphabet (list '(0 4 7) '(0 3 7) '(2 6 9) '(2 5 9)
								    '(4 8 11) '(4 7 11))))
(defparameter *ex-2-derived-viewpoint* (make-instance 'viewpoints::h-bass-cpint :alphabet (list -2 0 2)))

(5am:test mapping-ex-2
  (5am:is (equal (sort (old-mapping *ex-2-derived-viewpoint* *ex-2-basic-viewpoint*
				    (coerce (viewpoints:harm-seq '((2 6 9) (2 6 9))) 'list))
		       #'(lambda (x y) (< (car x) (car y))))
		 (sort (mapping *ex-2-derived-viewpoint* *ex-2-basic-viewpoint*
				(coerce (viewpoints:harm-seq '((2 6 9) (2 6 9))) 'list))
		       #'(lambda (x y) (< (car x) (car y)))))))


(utils:set-test-suite-dependencies 'mapping '(viewpoints::h-cpitch viewpoints::h-bass-cpitch))
