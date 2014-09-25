;;;; ======================================================================
;;;; File:       IDyOM.asd
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-05-04 21:29:04 marcusp>
;;;; Time-stamp: <2014-09-25 11:20:58 marcusp>
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage #:idyom-system (:use #:asdf #:cl))
(in-package #:idyom-system)

(defsystem idyom
  :name "IDyOM"
  :version "1.0"
  :author "Marcus Pearce"
  :licence "GPL (see COPYING file for details)"
  :description "Information Dynamics of Music"
  :depends-on (clsql cl-ppcre midi closer-mop psgraph)
  :serial t
  :components
  (;; General utilities  
   (:module utils 
	    :serial t
            :components 
            ((:file "package")
             (:file "utils")))
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
		       (:file "midi2db")
		       (:file "conklin2db")
		       (:file "tobias2db")))
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
             (:file "music-objects")))
   ;; Viewpoints
   (:module viewpoints
	    :serial t
	    :components
	    ((:file "package")
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
		       (:file "misc")
		       (:file "implication-realisation")))
             ;; useful extensions for modelling (not part of the representation scheme)
	     (:file "extensions")))
   ;; PPM Statistical Models
   (:module ppm-star
	    :serial t
	    :components
	    ((:file "package")
	     (:file "generics")
	     (:file "ppm-star")
	     (:file "ppm-io")
	     (:file "ppm-ui")))
   (:module mvs
            :serial t 
            :components
            ((:file "package")
             (:file "params") 
             (:file "generics")
             (:file "prediction-sets")		
             (:file "multiple-viewpoint-system")))
   ;; Applications 
   (:module apps 
            :serial t
            :components
            ((:file "package")
             (:file "apps")
             (:file "resampling")
             (:file "viewpoint-selection")
             (:file "main")
             (:file "generation")))))
