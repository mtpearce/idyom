;;;; ======================================================================
;;;; File:       IDyOM.asd
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2003-05-04 21:29:04 marcusp>
;;;; Time-stamp: <2011-06-30 17:24:12 marcusp>
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage #:idyom-system (:use #:asdf #:cl))
(in-package #:idyom-system)

(defsystem idyom
  :depends-on (amuse amuse-viewpoints amuse-mtp ppm-star mtp-admin)
  :serial t
  :components
  (;; General utilities  
   (:module utils 
            :components 
            ((:file "package")
             (:file "utils" :depends-on ("package"))))
   ;; Data Representation and Access
   (:module amuse
            :serial t 
            :components
            ((:file "package")
             (:file "amuse-interface")
             (:file "viewpoint-extensions")))
   ;; PPM Statistical Models
   (:module ppm 
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
