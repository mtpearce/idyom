;;;; ======================================================================
;;;; File:       package.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2003-04-05 18:54:17 marcusp>                           
;;;; Time-stamp: <2023-05-22 13:20:14 marcusp>                           
;;;; ======================================================================

(cl:in-package #:cl-user)

;;; ======================================================================
;;; REPRESENTATION OF MUSIC OBJECTS
;;; ======================================================================

(defpackage #:idyom-db
  (:use #:common-lisp #:clsql)
  (:nicknames db)
  (:export #:mtp-event #:mtp-composition #:mtp-dataset
           #:connect-to-database #:initialise-database
           #:import-data #:export-data #:insert-dataset #:delete-dataset 
           #:describe-dataset #:describe-database #:copy-datasets
           #:dataset-composition #:composition-events 
           #:get-id #:get-dataset #:get-composition #:get-event 
           #:copy-event #:get-description
           #:get-compositions #:get-event-attribute
           #:get-attribute #:set-attribute #:get-timebase #:get-midc 
           #:get-next-free-id #:count-compositions #:count-events #:get-alphabet 
           #:get-max-event-count #:preview)
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

