;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       apps.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2005-11-27 16:27:35 marcusp>
;;;; Time-stamp: <2008-11-03 20:14:29 marcusp>
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


