;;;; ======================================================================
;;;; File:       music-db.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2013-04-01 12:11:04 jeremy>
;;;; Time-stamp: <2014-03-06 10:15:34 marcusp>
;;;; ======================================================================
;;;
;;; Basic version of amuse-mtp package.
;;; 
;;; From amuse/implementations/mtp/

(cl:in-package #:mtp-data)

;;; Identifiers 

(defclass mtp-dataset-identifier (music-data:dataset-identifier)
  ((dataset-id :initarg :dataset-index :accessor dataset-index :type (integer 0 *))))
   
(defclass mtp-composition-identifier (mtp-dataset-identifier music-data:composition-identifier)
  ((composition-id :initarg :composition-index :accessor composition-index :type (integer 0 *))))

(defclass mtp-event-identifier (mtp-composition-identifier music-data:event-identifier)
  ((event-id :initarg :event-index :accessor event-index :type (integer 0 *))))


(defun make-dataset-id (dataset-index) 
  (make-instance 'mtp-dataset-identifier
		 :dataset-index dataset-index))

(defun make-composition-id (dataset-index composition-index)
  (make-instance 'mtp-composition-identifier
		 :dataset-index dataset-index
		 :composition-index composition-index))

(defun make-event-id (dataset-index composition-index event-index)
  (make-instance 'mtp-event-identifier
		 :dataset-index dataset-index
		 :composition-index composition-index
		 :event-index event-index))



#.(clsql:locally-enable-sql-reader-syntax)
(defun get-mtp-alphabet (attribute &rest dataset-ids)
  (clsql:select attribute :from 'mtp-event
                :where [in [slot-value 'mtp-event 'dataset-id] dataset-ids]
                :order-by attribute
                :flatp t 
                :field-names nil 
                :distinct t))
#.(clsql:restore-sql-reader-syntax-state)


;;(defun get-db-event-sequence (dataset-id composition-id)
;;  (music-data:composition->monody
;;   (music-data:get-composition 
;;    (make-composition-id dataset-id composition-id))))

;; (defun get-db-event-sequences (&rest dataset-ids)
;;   (let ((compositions '()))
;;     (dolist (dataset-id dataset-ids (nreverse compositions))
;;       (let ((d (md:get-dataset 
;;                 (make-dataset-id dataset-id))))
;;         (sequence:dosequence (c d)
;;           (push (md:composition->monody c) compositions))))))


(defun db-event->mtp-event (db-event timebase)
  (let* ((event-id (make-event-id (first db-event)
				 (second db-event)
				 (third db-event)))
         (mtp-event (make-instance 'music-data:music-event
				   :id event-id)))
    (do* ((slts music-data:*md-music-slots* (cdr slts))
          (db-atts (nthcdr 3 db-event) (cdr db-atts)))
         ((null slts) mtp-event)
      (if (member (car slts) music-data:*md-time-slots* :test #'eql)
          (setf (slot-value mtp-event (car slts)) (convert-time-slot (car db-atts) timebase))
          (setf (slot-value mtp-event (car slts)) (car db-atts))))))

(defun convert-time-slot (value timebase)
  "Convert native representation of time into a representation where
    a crotchet has a value of 96."
  (if (or (null value) (null timebase))
      nil
      (let ((multiplier (/ 96 timebase)))
        (* value multiplier))))



(defmethod md:copy-identifier ((id mtp-dataset-identifier))
  (make-instance 'mtp-dataset-identifier
		 :dataset-index (dataset-index id)))

(defmethod md:copy-identifier ((id mtp-composition-identifier))
  (make-instance 'mtp-composition-identifier
		 :dataset-index (dataset-index id)
		 :composition-index (composition-index id)))

(defmethod md:copy-identifier ((id mtp-event-identifier))
  (make-instance 'mtp-event-identifier
		 :dataset-index (dataset-index id)
		 :composition-index (composition-index id)
		 :event-index (event-index id)))


;; EXTEND MUSIC-DATA PACKAGE
;;
(cl:in-package #:music-data)

;; Extract indices from identifier
(defmethod get-dataset-index ((id dataset-identifier))
  (mtp-data:dataset-index id))

(defmethod get-composition-index ((id composition-identifier))
  (mtp-data:composition-index id))

(defmethod get-event-index ((id event-identifier))
  (mtp-data:event-index id))

(defparameter *idyom-datasource* :sql)

(defmethod lookup-dataset (dataset-index (source (eql :sql)))  
  (mtp-data:make-dataset-id dataset-index))

(defmethod lookup-composition (dataset-index composition-index (source (eql :sql)))
  (mtp-data:make-composition-id dataset-index composition-index))






;;; Compositions 

#.(clsql:locally-enable-sql-reader-syntax)

(defvar *event-attributes* 
  (list [dataset-id] [composition-id] [event-id]
        [onset] [dur] [deltast] [cpitch] 
	[mpitch] [accidental] [keysig] [mode]
        [barlength] [pulses] [phrase] [tempo] [dyn] [voice] [bioi] 
        [ornament] [comma] [articulation]))


(defmethod get-dataset ((identifier mtp-data:mtp-dataset-identifier))
  (let* ((dataset-id (get-dataset-index identifier))
         (where-clause [= [dataset-id] dataset-id])
         (db-dataset (car (clsql:select [*] :from [mtp-dataset] :where where-clause)))
         (db-compositions (clsql:select [composition-id][description][timebase]
                                        :from [mtp-composition] 
                                        :order-by '(([composition-id] :asc))
                                        :where where-clause))
         (db-events (apply #'clsql:select 
                           (append *event-attributes* 
                                   (list :from [mtp-event] 
                                         :order-by '(([composition-id] :asc)
                                                     ([event-id] :asc))
                                         :where where-clause))))
	 (dataset (make-instance 'music-dataset
				 :id identifier
				 :description (second db-dataset) 
				 :timebase (third db-dataset) 
				 :midc (fourth db-dataset)))
         (compositions nil)
         (events nil))
    ;; for each db-composition 
    (dolist (dbc db-compositions)
      (let ((composition-id (first dbc))
            (description (second dbc))
            (timebase (third dbc)))
        ;; for each db-event 
        (do* ((dbes db-events (cdr dbes))
              (dbe (car dbes) (car dbes))
              (cid (second dbe) (second dbe)))
             ((or (null dbes) (not (= cid composition-id)))
              (setf db-events dbes))
          (when dbe
            (push (mtp-data:db-event->mtp-event dbe timebase) events)))
        (when events
          (let* (;(interval (+ (md:onset (car events)) (md:duration (car events))))
		 (comp-id (mtp-data:make-composition-id dataset-id composition-id))
                 (composition
                  (make-instance 'music-composition
				 :id comp-id
				 :description description
				 :timebase timebase)))
				 ;:time 0
				 ;:interval interval)))
            (sequence:adjust-sequence composition (length events)
                                      :initial-contents (nreverse events))
            (setf events nil)
            (push composition compositions)))))
    (sequence:adjust-sequence dataset (length compositions)
                              :initial-contents (nreverse compositions))
    dataset))

(defmethod get-composition ((identifier mtp-data:mtp-composition-identifier))
  (let* ((dataset-id (get-dataset-index identifier))
         (composition-id (get-composition-index identifier))
         (where-clause [and [= [dataset-id] dataset-id]
                            [= [composition-id] composition-id]])
         (description 
          (car (clsql:select [description] :from [mtp-composition] 
                             :where where-clause :flatp t :field-names nil)))
         (timebase 
          (car (clsql:select [timebase] :from [mtp-composition] 
                             :where where-clause :flatp t :field-names nil)))
         (db-events (apply #'clsql:select 
                           (append *event-attributes* 
                                   (list :from [mtp-event] 
                                         :order-by '(([event-id] :asc))
                                         :where where-clause))))
         (events nil))
    (dolist (e db-events)
      (push (mtp-data:db-event->mtp-event e timebase) events))
    (let* ((composition 
            (make-instance 'music-composition
			   :id identifier
			   :description description
			   :timebase timebase)))
      (sequence:adjust-sequence composition (length events)
                                :initial-contents (nreverse events))
      composition)))


;;; Constituents from compositions: time-signatures 


(defmethod crotchet ((event music-event))
  (let ((timebase 
         (car (clsql:select [timebase] :from [mtp-composition]
                            :where [and [= [dataset-id] (get-dataset-index (ident event))] [= [composition-id] (get-composition-index (ident event))]]
                            :flatp t 
                            :field-names nil))))
    (/ timebase 4)))
#.(clsql:restore-sql-reader-syntax-state) 

