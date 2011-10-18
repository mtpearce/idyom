;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       apps.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2005-11-27 16:27:35 marcusp>
;;;; Time-stamp: <2011-10-18 10:56:30 marcusp>
;;;; ======================================================================

(cl:in-package #:apps) 

;;; Paths 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *root-dir*
    (concatenate 'string
                 #+cmu (namestring (car (ext:search-list "home:")))
                 #+sbcl (sb-ext:posix-getenv "HOME")
                 #+allegro "/home/marcusp"
                 "/research/projects/idm/code/lisp/"))
  (setf mvs:*ep-cache-dir* (string-append *root-dir* "data/cache/")))

;;; A way of generating filenames to store results, cached data etc.

(defun dataset-modelling-filename (dataset-id basic-attributes attributes
                                   &key (extension "")
                                   pretraining-ids (k 10) (models :both+)
                                   resampling-indices
                                   (ltm-order-bound mvs::*ltm-order-bound*)
                                   (ltm-mixtures mvs::*ltm-mixtures*)
                                   (ltm-update-exclusion mvs::*ltm-update-exclusion*)
                                   (ltm-escape mvs::*ltm-escape*)
                                   (stm-order-bound mvs::*stm-order-bound*)
                                   (stm-mixtures mvs::*stm-mixtures*)
                                   (stm-update-exclusion mvs::*stm-update-exclusion*)
                                   (stm-escape mvs::*stm-escape*))
  (flet ((format-list (list)
           (when list
             (let ((flist (format nil "~{~A_~}" list)))
               (subseq flist 0 (1- (length flist)))))))
    (let ((string (format nil "~(~{~A-~}~)" 
                          (list dataset-id 
                                (format-list basic-attributes)
                                (format-list attributes)
                                (format-list pretraining-ids)
                                (format-list resampling-indices)
                                k models
                                ltm-order-bound ltm-mixtures ltm-update-exclusion ltm-escape
                                stm-order-bound stm-mixtures stm-update-exclusion stm-escape))))
      (concatenate 'string (subseq string 0 (1- (length string))) extension))))
