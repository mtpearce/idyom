;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       mtp-admin.asd
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2008-10-31 15:44:34 marcusp>
;;;; Time-stamp: <2010-02-25 11:53:37 marcusp>
;;;; ======================================================================

(cl:in-package #:cl-user)

(defpackage #:mtp-admin-system (:use #:asdf #:cl))
(in-package #:mtp-admin-system)

(defsystem mtp-admin
  :depends-on (clsql cl-ppcre midi amuse-mtp)
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
