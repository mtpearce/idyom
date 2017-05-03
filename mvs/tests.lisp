;;;; ======================================================================
;;;; File:       tests.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-05-03 10:54:10 peter>                            
;;;; Time-stamp: <2017-05-03 10:55:42 peter>                           
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;;
;;;; This code defines tests for the mvs package.

(cl:in-package #:mvs)

(5am:def-suite mvs)
(5am:in-suite mvs)

(5am:def-suite mapping :in music-data)
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
