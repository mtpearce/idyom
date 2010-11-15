;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-             
;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2003-04-05 18:54:17 marcusp>                           
;;;; Time-stamp: <2010-02-25 12:21:07 marcusp>                           
;;;; ======================================================================

(cl:in-package #:cl-user)

;;; ======================================================================
;;; REPRESENTATION OF MUSIC OBJECTS
;;; ======================================================================

(defpackage #:mtp-admin
  (:use #:common-lisp #:clsql)
  (:export "MTP-EVENT" "MTP-COMPOSITION" "MTP-DATASET"
           "CONNECT-TO-DATABASE" "INITIALISE-DATABASE"
           "IMPORT-DATA" "EXPORT-DATA" "INSERT-DATASET" "DELETE-DATASET" 
           "DESCRIBE-DATASET" "DESCRIBE-DATABASE" 
           "DATASET-COMPOSITION" "COMPOSITION-EVENTS" 
           "GET-ID" "GET-DATASET" "GET-COMPOSITION" "GET-EVENT" 
           "COPY-EVENT" "GET-DESCRIPTION"
           "GET-COMPOSITIONS" "GET-EVENT-ATTRIBUTE"
           "GET-ATTRIBUTE" "SET-ATTRIBUTE" "GET-TIMEBASE" "GET-MIDC" 
           "GET-NEXT-FREE-ID" "COUNT-COMPOSITIONS" "COUNT-EVENTS" "GET-DOMAIN" 
           "GET-MAX-EVENT-COUNT")
  ;;(:import-from #:amuse-mtp "MTP-DATASET" "MTP-COMPOSITION" "MTP-EVENT")
  (:documentation "Interface to an SQL database of music objects."))

;;; ======================================================================
;;; DATA IMPORT 
;;; ======================================================================

(defpackage #:kern2db
  (:use #:common-lisp #:mtp-admin #:cl-ppcre)
  (:documentation "Importing Kern data into the database."))

(defpackage #:midi2db
  (:use #:common-lisp #:mtp-admin #:midi)
  (:documentation "Importing MIDI data into the database."))

(defpackage #:conklin2db
  (:use #:common-lisp #:mtp-admin)
  (:documentation "Import Darrell Conklin's lisp format to the database"))

(defpackage #:tobias2db
  (:use #:common-lisp #:mtp-admin)
  (:documentation "Import tabular pitch values where each row is a
melody and each column a pitch into the database"))

;;; ======================================================================
;;; DATA EXPORT 
;;; ======================================================================

(defpackage #:db2midi
  (:use #:common-lisp #:mtp-admin #:midi)
  (:documentation "Exporting music objects as midi files."))

(defpackage #:db2lilypond
  (:use #:common-lisp #:mtp-admin)
  (:documentation "Exporting music objects as lilypond scores and
thence to postscript and pdf."))

;; (defpackage #:db2cmn
;;   (:use #:common-lisp #:mtp-admin #:cmn #:cm)
;;   (:documentation "Exports data from the database in CMN format."))

