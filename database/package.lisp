;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-05 18:54:17 marcusp>                           
;;;; Time-stamp: <2017-05-15 17:40:32 peter>                           
;;;; ======================================================================

(cl:in-package #:cl-user)

;;; ======================================================================
;;; REPRESENTATION OF MUSIC OBJECTS
;;; ======================================================================

(defpackage #:idyom-db
  (:use #:common-lisp #:clsql)
  (:export "MTP-EVENT" "MTP-COMPOSITION" "MTP-DATASET" "DB-BACKUP"
	   "CONNECT-TO-DATABASE" "INITIALISE-DATABASE" "CHECK-DB"
           "IMPORT-DATA" "EXPORT-DATA" "PREVIEW"
	   "INSERT-DATASET" "DELETE-DATASET" 
           "DESCRIBE-DATASET" "DESCRIBE-DATABASE" "COPY-DATASETS"
           "DATASET-COMPOSITION" "COMPOSITION-EVENTS" 
           "GET-ID" "GET-DATASET" "GET-COMPOSITION" "GET-EVENT" 
           "COPY-EVENT" "GET-DESCRIPTION"
           "GET-COMPOSITIONS" "GET-EVENT-ATTRIBUTE"
           "GET-ATTRIBUTE" "SET-ATTRIBUTE" "GET-TIMEBASE" "GET-MIDC" 
           "GET-NEXT-FREE-ID" "COUNT-COMPOSITIONS" "COUNT-EVENTS" "GET-DOMAIN" 
           "GET-MAX-EVENT-COUNT"
	   "IMPORT-COMMON-PRACTICE-TONAL" "IMPORT-POP" "IMPORT-JAZZ"
	   "IMPORT-FOUR-BACH-CHORALES")
  (:documentation "Interface to an SQL database of music objects."))

;;; ======================================================================
;;; DATA IMPORT 
;;; ======================================================================

(defpackage #:kern2db
  (:use #:common-lisp #:idyom-db #:cl-ppcre)
  (:export "*SPINES*")
  (:documentation "Importing Kern data into the database."))

(defpackage #:midi2db
  (:use #:common-lisp #:idyom-db #:midi)
  (:documentation "Importing MIDI data into the database."))

(defpackage #:conklin2db
  (:use #:common-lisp #:idyom-db)
  (:documentation "Import Darrell Conklin's lisp format to the database"))

(defpackage #:text2db
  (:use #:common-lisp #:idyom-db)
  (:documentation "Import tabular pitch values where each row is a
melody and each column a pitch into the database"))

(defpackage #:mcgill2db
  (:use #:common-lisp #:idyom-db #:cl-ppcre)
  (:documentation "Importing the McGill Billboard corpus to the database."))

;;; ======================================================================
;;; DATA EXPORT 
;;; ======================================================================

(defpackage #:db2midi
  (:use #:common-lisp #:idyom-db #:midi)
  (:documentation "Exporting music objects as midi files."))

(defpackage #:db2lilypond
  (:use #:common-lisp #:idyom-db)
  (:documentation "Exporting music objects as lilypond scores and
thence to postscript and pdf."))

;; (defpackage #:db2cmn
;;   (:use #:common-lisp #:idyom-db #:cmn #:cm)
;;   (:documentation "Exports data from the database in CMN format."))

