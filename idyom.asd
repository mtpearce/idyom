;;;; ======================================================================
;;;; File:       IDyOM.asd
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-05-04 21:29:04 marcusp>
;;;; Time-stamp: <2017-07-23 12:31:47 peter>
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage #:idyom-system (:use #:asdf #:cl))
(in-package #:idyom-system)

(defsystem idyom
  :name "IDyOM"
  :version "1.4"
  :author "Marcus Pearce"
  :licence "GPL (see COPYING file for details)"
  :description "Information Dynamics of Music (see README for details)"
  :depends-on (clsql cl-ppcre midi closer-mop psgraph cl-csv fiveam
		     fare-memoization iterate cl-store)
  :serial t
  :components
  ;; Interfacing with external programs
  ((:module interfaces
	   :serial t
	   :components
	   ((:file "package")
	    (:file "r")
	    (:file "tests")))
  ;; General utilities  
   (:module utils 
	    :serial t
            :components 
            ((:file "package")
             (:file "utils")
	     (:file "tests")))
   ;; Database for storage and retrieval of music
   (:module database
	    :serial t
	    :components
	    (;; General administrative utilities  
	     (:file "package")
	     (:file "generics")
	     (:file "music-data")
	     ;; Data import 
	     (:module data-import
		      :components 
		      ((:file "kern2db")
		       (:file "kern2db-tests")
		       (:file "midi2db")
		       (:file "text2db")
		       (:file "conklin2db")
		       (:file "mcgill2db")))
	     ;; Data export 
	     (:module data-export
		      :components 
		      (;;(:file "db2cmn")
		       (:file "db2midi")
		       (:file "db2lilypond")
		       (:file "db2score" :depends-on ("db2lilypond"))))))
   ;; Representation language for music objects
   (:module music-objects
	    :serial t
	    :components
	    ((:file "package")
	     (:file "extended-sequence")
	     (:file "time")
             (:file "music-objects")
	     (:file "tests")))
   ;; Viewpoints
   (:module viewpoints
	    :serial t
	    :components
	    ((:file "package")
	     (:file "globals")
	     (:file "generics")
	     (:file "classes")
	     (:file "methods")
	     (:file "functions")
	     (:file "macros")
             (:module melody :serial t
		      :components
		      ((:file "basic-viewpoints")
                       (:file "pitch")
		       (:file "scales")
		       (:file "temporal")
                       (:file "phrase")
		       (:file "threaded")
		       (:file "implication-realisation")))
	     (:module harmony :serial t
		      :components
		      ((:file "basic-viewpoints")
		       (:file "general-chord-type")
		       (:file "key-finding")
		       (:file "pitch")
		       (:file "hutchinson-knopoff")
		       (:file "milne")
		       (:file "tymoczko")))
             ;; useful extensions for modelling 
             ;; (not strictly part of the representation scheme)
	     (:file "extensions")
	     (:file "tests")))
   ;; PPM* Statistical Models
   (:module ppm-star
	    :serial t
	    :components
	    ((:file "package")
	     (:file "generics")
	     (:file "ppm-star")
	     (:file "ppm-io")
	     (:file "ppm-ui")))
   ;; Prediction using multiple viewpoint systems (MVS)
   (:module mvs
            :serial t 
            :components
            ((:file "package")
             (:file "params") 
             (:file "generics")
             (:file "prediction-sets")		
             (:file "multiple-viewpoint-system")
	     (:file "tests")))
   ;; Applications 
   (:module apps 
            :serial t
            :components
            ((:file "package")
             (:file "apps")
             (:file "resampling")
	     (:file "resampling-tests")
             (:file "viewpoint-selection")
             (:file "main")
             (:file "segmentation")
	     (:file "descriptives")
             (:file "generation")))
   ;; Studies
   (:module pmch-studies
	    :serial t
	    :components
	    ((:file "package")
	     (:file "study-1")))))
