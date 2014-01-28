;;;; ======================================================================
;;;; File:       generics.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@eecs.qmul.ac.uk>
;;;; Created:    <2004-10-28 11:56:59 marcusp>
;;;; Time-stamp: <2014-01-28 09:51:00 marcusp>
;;;; ======================================================================

(cl:in-package #:mtp-admin)

(defgeneric export-data (object type path))
(defgeneric import-data (type path description id))
(defgeneric insert-composition (dataset composition id))
(defgeneric insert-event (composition event id))
(defgeneric get-attribute (event attribute))
(defgeneric set-attribute (event attribute value))
(defgeneric copy-event (object))

