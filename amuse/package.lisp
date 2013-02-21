;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2008-09-30 18:54:34 marcusp>
;;;; Time-stamp: <2013-02-21 16:39:59 jeremy>
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage #:music-data
  (:use #:common-lisp)
  (:nicknames #:md)
  (:export "GET-EVENT-SEQUENCE" "GET-EVENT-SEQUENCES" 
           "GET-ATTRIBUTE" "SET-ATTRIBUTE"
	   "COMPOSITION-VIEWPOINT" "DATASET-VIEWPOINT"
           "GET-ALPHABET" "COPY-EVENT" "COUNT-COMPOSITIONS")
  (:documentation "Interface to amuse music data."))
