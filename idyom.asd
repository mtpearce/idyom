;;;; ======================================================================
;;;; File:       IDyOM.asd
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2003-05-04 21:29:04 marcusp>
;;;; Time-stamp: <2013-04-16 08:52:14 jeremy>
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage #:idyom-system (:use #:asdf #:cl))
(in-package #:idyom-system)

(defsystem idyom
  :depends-on (amuse amuse-mtp clsql cl-ppcre midi closer-mop psgraph)
  :serial t
  :components
  (;; General utilities  
   (:module utils 
	    :serial t
            :components 
            ((:file "package")
             (:file "utils")))
   ;; Data Representation and Access
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
   ;; Viewpoints
   (:module viewpoints
	    :serial t
	    :components
	    ((:file "package")
                 (:file "utils")
                 (:file "generics")
                 (:file "classes")
                 (:file "methods")
                 (:file "functions")
                 (:file "macros")
		 (:file "basic-viewpoints")
		 ;; Derived viewpoints
		 (:module derived-viewpoints
			  :components
			  ((:file "pitch")
			   (:file "scales")
			   (:file "temporal")
			   (:file "misc")
			   (:file "implication-realisation")))))
   (:module amuse
            :serial t 
            :components
            ((:file "package")
             (:file "amuse-interface")
             (:file "viewpoint-extensions")))
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
