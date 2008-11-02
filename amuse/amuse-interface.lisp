;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ======================================================================
;;;; File:       amuse-interface.lisp
;;;; Author:     Marcus Pearce <m.pearce@gold.ac.uk>
;;;; Created:    <2008-09-30 17:25:38 marcusp>
;;;; Time-stamp: <2008-10-31 15:40:28 marcusp>
;;;; ======================================================================

(cl:in-package #:music-data)

(defun get-event-sequence (dataset-id composition-id)
  (amuse:monody
   (amuse:get-composition 
    (amuse-mtp:make-mtp-composition-identifier dataset-id composition-id))))

(defun get-event-sequences (&rest dataset-ids)
  (let ((compositions '()))
    (dolist (dataset-id dataset-ids (nreverse compositions))
      (let ((d (amuse-mtp:get-dataset 
                (amuse-mtp:make-mtp-dataset-identifier dataset-id))))
        (sequence:dosequence (c d)
          (push (amuse:monody c) compositions))))))

(defgeneric get-attribute (event attribute))
(defmethod get-attribute ((e amuse-mtp:mtp-event) attribute)
  "Returns the value for slot <attribute> in event object <e>."
  (let* ((accessor-name 
          (concatenate 'string "EVENT-" (string-upcase (symbol-name attribute))))
         (accessor-symbol (find-symbol accessor-name (find-package :amuse-mtp))))
    (funcall accessor-symbol e)))

(defgeneric set-attribute (event attribute value))
(defmethod set-attribute ((e amuse-mtp:mtp-event) attribute value)
  (let* ((attribute (case attribute
                      (:onset :time)
                      (onset :time)
                      (:dur :interval)
                      (dur :interval)
                      (t attribute)))
         (accessor-name (string-upcase (symbol-name attribute)))
         (accessor-symbol (find-symbol accessor-name (find-package :amuse-mtp))))
    (setf (slot-value e accessor-symbol) value)))

#.(clsql:locally-enable-sql-reader-syntax)
(defun get-alphabet (attribute &rest dataset-ids)
  (clsql:select attribute :from 'mtp-event
                :where [in [slot-value 'mtp-event 'dataset-id] dataset-ids]
                :order-by attribute
                :flatp t 
                :field-names nil 
                :distinct t))
#.(clsql:restore-sql-reader-syntax-state)

(defgeneric copy-event (event))
(defmethod copy-event ((l list))
  (make-instance 'amuse-mtp:mtp-event))

;; TODO: these should be replaced with a new amuse generic: 
;; (defgeneric get-identifier (object))
(defun dataset-id (event) (amuse-mtp::dataset-id event))
(defun composition-id (event) (amuse-mtp::composition-id event))
(defun event-id (event) (amuse-mtp::event-id event))

(defmethod copy-event ((e amuse-mtp:mtp-event))
  (make-instance 'amuse-mtp:mtp-event
                 :dataset-id (amuse-mtp::dataset-id e)
                 :composition-id (amuse-mtp::composition-id e)
                 :event-id (amuse-mtp::event-id e)
                 :time (amuse:timepoint e)
                 :deltast (amuse-mtp::%mtp-deltast e)
                 :bioi (amuse-mtp::%mtp-bioi e)
                 :cpitch (amuse-mtp::%mtp-cpitch e)
                 :mpitch (amuse-mtp::%mtp-mpitch e)
                 :interval (amuse:duration e)
                 :keysig (amuse-mtp::%mtp-keysig e)
                 :accidental (amuse-mtp::%mtp-accidental e)
                 :mode (amuse-mtp::%mtp-mode e)
                 :barlength (amuse-mtp::%mtp-barlength e)
                 :pulses (amuse-mtp::%mtp-pulses e)
                 :phrase (amuse-mtp::%mtp-phrase e)
                 :dyn (amuse-mtp::%mtp-dyn e)
                 :tempo (amuse-mtp::%mtp-tempo e)
                 :voice (amuse-mtp::%mtp-voice e)))

(defun count-compositions (dataset-id)
  (length (amuse-mtp:get-dataset 
           (amuse-mtp:make-mtp-dataset-identifier dataset-id))))

;; (defun get-timebase (dataset-id)
;;   (* 4 (amuse:crotchet 
;;         (amuse-mtp:get-dataset 
;;          (amuse-mtp:get-dataset-identifier dataset-id)))))

;; (defun get-midc (dataset-id))

;; (defvar *db-file* nil) 
;; (defmethod make-load-form ((e event) &optional environment)
;; (defun connect-to-database (&optional (db-file *db-file*))
;; (defmethod import-data ((type (eql :lisp)) filename description id)
;; (defmethod export-data ((d dataset) (type (eql :lisp)) filename)
;; (defun insert-dataset (data id)
;; (defmethod insert-composition ((d dataset) composition id)
;; (defmethod insert-event ((composition composition) event id)
;; (defun delete-dataset (dataset-id)
;; (defun initialise-database (&optional (db *default-database*))

;(defgeneric get-id (object))
;(defun get-dataset (dataset-id)
;(defun get-event (dataset-id composition-id event-id))
;(defun get-description (dataset-id &optional composition-id)
;(defun get-compositions (dataset-id)
;(defun get-composition (dataset-id composition-id)
;(defun get-event-attribute (attribute dataset-id composition-id event-id)
;(defmethod composition-description ((e event))
;(defmacro do-events ((event dataset-id composition-id) &body body)
;(defmacro do-compositions ((composition dataset-id) &body body)
;(defun get-next-free-id (&optional dataset-id composition-id)
;(defun count-events (dataset-id &optional (composition-id nil))
;(defun describe-dataset (dataset-id &key (attributes nil)
;(defun describe-database (&key (attributes nil)
;(defun get-sequence (attribute dataset-id composition-id event-id length)
