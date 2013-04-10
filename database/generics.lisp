;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       generics.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2004-10-28 11:56:59 marcusp>
;;;; Time-stamp: <2008-10-31 16:44:26 marcusp>
;;;; ======================================================================

(cl:in-package #:mtp-admin)

(defgeneric export-data (object type path))
(defgeneric import-data (type path description id))
(defgeneric insert-composition (dataset composition id))
(defgeneric insert-event (composition event id))
(defgeneric get-attribute (event attribute))
(defgeneric set-attribute (event attribute value))
(defgeneric copy-event (object))

