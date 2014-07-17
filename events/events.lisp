;;;; ======================================================================
;;;; File:       events.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2013-04-12 12:46:19 jeremy>
;;;; Time-stamp: <2014-07-17 19:40:15 marcusp>
;;;; ======================================================================

(cl:in-package #:music-data)

#.(clsql:locally-enable-sql-reader-syntax)
(defvar *event-attributes* 
  (list [dataset-id] [composition-id] [event-id]
        [onset] [dur] [deltast] [cpitch] 
	[mpitch] [accidental] [keysig] [mode]
        [barlength] [pulses] [phrase] [tempo] [dyn] [voice] [bioi] 
        [ornament] [comma] [articulation]))
#.(clsql:restore-sql-reader-syntax-state)

(defun music-symbol (x)
  (find-symbol (string-upcase (symbol-name x))
	       (find-package :music-data)))

; the order must match *event-attributes*
(defvar *music-slots* '(onset dur deltast cpitch mpitch accidental 
            keysig mode barlength pulses phrase tempo dyn voice bioi 
            ornament comma articulation))

(defvar *md-music-slots* (mapcar #'music-symbol *music-slots*))

(defvar *time-slots* '(onset dur deltast barlength bioi))
(defvar *md-time-slots* (mapcar #'music-symbol *time-slots*))

(defparameter *idyom-datasource* :sql) ;; FIXME: remove globals



;;; Classes for music objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass music-object ()
  ((identifier :initarg :id :accessor ident :type identifier)
   (description :initarg :description :accessor description)
   (timebase :initarg :timebase :accessor timebase)
   (midc :initarg :midc :accessor midc)))

(defclass music-dataset (list-slot-sequence music-object) ())

(defclass music-composition (list-slot-sequence music-object) ())

(defclass music-event (music-object) 
  ((onset :initarg :onset :accessor onset) 
   (dur :initarg :dur :accessor duration)
   (bioi :initarg :bioi :accessor bioi)
   (deltast :initarg :deltast :accessor deltast)
   (cpitch :initarg :cpitch :accessor chromatic-pitch)
   (mpitch :initarg :mpitch :accessor morphetic-pitch)
   (accidental :initarg :accidental :accessor accidental)
   (keysig :initarg :keysig :accessor key-signature)
   (mode :initarg :mode :accessor mode)
   (barlength :initarg :barlength :accessor barlength)
   (pulses :initarg :pulses :accessor pulses)
   (phrase :initarg :phrase :accessor phrase)
   (tempo :initarg :tempo :accessor tempo)
   (dyn :initarg :dyn :accessor dynamics)
   (ornament :initarg :ornament :accessor ornament)
   (comma :initarg :comma :accessor comma)
   (articulation :initarg :articulation :accessor articulation)
   (voice :initarg :voice :accessor voice)))


;; Accessing data
(defgeneric get-attribute (event attribute))
(defmethod get-attribute ((e music-event) attribute)
  "Returns the value for slot <attribute> in event object <e>."
  (slot-value e (music-symbol attribute)))

(defgeneric set-attribute (event attribute value))
(defmethod set-attribute ((e music-event) attribute value)
  (setf (slot-value e (music-symbol attribute))
	value))

(defgeneric copy-event (music-event))
(defmethod copy-event ((e music-event))
  (make-instance 'music-event
                 :id (copy-identifier (ident e))
                 :timebase (timebase e)
                 ;;:description (description e)
                 ;;:midc (midc e)
                 :onset (onset e)
		 :dur (duration e)
                 :deltast (deltast e)
                 :bioi (bioi e)
                 :cpitch (chromatic-pitch e)
                 :mpitch (morphetic-pitch e)
                 :keysig (key-signature e)
                 :accidental (accidental e)
                 :mode (mode e)
                 :barlength (barlength e)
                 :pulses (pulses e)
                 :phrase (phrase e)
                 :dyn (dynamics e)
                 :tempo (tempo e)
                 :ornament (ornament e)
                 :comma (comma e)
                 :articulation (articulation e)
                 :voice (voice e)))


;;; Identifiers 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass dataset-identifier ()
  ((dataset-id :initarg :dataset-index :accessor dataset-index :type (integer 0 *))))
   
(defclass composition-identifier (dataset-identifier)
  ((composition-id :initarg :composition-index :accessor composition-index :type (integer 0 *))))

(defclass event-identifier (composition-identifier)
  ((event-id :initarg :event-index :accessor event-index :type (integer 0 *))))

(defun make-dataset-id (dataset-index) 
  (make-instance 'dataset-identifier :dataset-index dataset-index))

(defun make-composition-id (dataset-index composition-index)
  (make-instance 'composition-identifier
                 :dataset-index dataset-index
		 :composition-index composition-index))

(defun make-event-id (dataset-index composition-index event-index)
  (make-instance 'event-identifier
		 :dataset-index dataset-index
		 :composition-index composition-index
		 :event-index event-index))

;; Datasets, compositions and events are required to have an unique integer index.
(defgeneric get-dataset-index (dataset-identifier)
  (:documentation "Integer index for dataset"))
(defgeneric get-composition-index (composition-identifier)
  (:documentation "Integer index for composition"))
(defgeneric get-event-index (event-identifier)
  (:documentation "Integer index for event"))

(defmethod get-dataset-index ((id dataset-identifier))
  (dataset-index id))

(defmethod get-composition-index ((id composition-identifier))
  (composition-index id))

(defmethod get-event-index ((id event-identifier))
  (event-index id))

;; Get an identifier
(defgeneric get-identifier (object))

(defmethod get-identifier ((mo music-object))
  (ident mo))

;; Copy identifier
(defgeneric copy-identifier (id))

(defmethod copy-identifier ((id dataset-identifier))
  (make-instance 'dataset-identifier
		 :dataset-index (dataset-index id)))

(defmethod copy-identifier ((id composition-identifier))
  (make-instance 'composition-identifier
		 :dataset-index (dataset-index id)
		 :composition-index (composition-index id)))

(defmethod copy-identifier ((id event-identifier))
  (make-instance 'event-identifier
		 :dataset-index (dataset-index id)
		 :composition-index (composition-index id)
		 :event-index (event-index id)))


;; Lookup identifiers
(defgeneric lookup-dataset (dataset-index datasource)
  (:documentation "Returns the identifier for the dataset that has
  this index in the given datasource"))

(defgeneric lookup-composition (dataset-index composition-index datasource)
  (:documentation "Returns the identifier for the composition that has
  these indices in the given datasource"))

(defgeneric lookup-event (dataset-index composition-index event-index datasource)
  (:documentation "Returns the identifier for the event that has
  these indices in the given datasource"))

(defmethod lookup-dataset (dataset-index (source (eql :sql)))  
  (make-dataset-id dataset-index))

(defmethod lookup-composition (dataset-index composition-index (source (eql :sql)))
  (make-composition-id dataset-index composition-index))

(defmethod lookup-event (dataset-index composition-index event-index (source (eql :sql)))
  (make-event-id dataset-index composition-index event-index))


;;; Getting music objects from the database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun get-event-sequence (dataset-index composition-index)
  (composition->monody (get-composition (lookup-composition dataset-index
					  composition-index
					  *idyom-datasource*))))

(defun get-event-sequences (&rest dataset-ids)
  (let ((compositions '()))
    (dolist (dataset-id dataset-ids (nreverse compositions))
      (let ((d (get-dataset dataset-id)))
        (sequence:dosequence (c d)
          (push (composition->monody c) compositions))))))

(defun composition->monody (composition)
  ;; using the voice of the first event in the piece
  (let ((monody (make-instance 'music-composition 
                               :id (copy-identifier (ident composition))
                               :description (description composition)
                               :timebase (timebase composition)))
        (events nil)
        (monody-voice nil))
    (sequence:dosequence (event composition)
      (when (null monody-voice)
        (setf monody-voice (voice event)))
      (when (= (voice event) monody-voice)
        (push event events)))
    (sequence:adjust-sequence 
     monody (length events)
     :initial-contents (sort events #'< :key #'onset))
    monody))

(defun count-compositions (dataset-id)
  (length (get-dataset dataset-id)))

(defgeneric get-dataset (dataset-identifier))
(defgeneric get-composition (composition-identifier))
(defgeneric get-event (event-identifier))

(defmethod get-dataset ((index integer))
  (get-dataset (lookup-dataset index *idyom-datasource*)))

#.(clsql:locally-enable-sql-reader-syntax)
(defmethod get-dataset ((identifier dataset-identifier))
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
    (when db-dataset
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
              (push (db-event->music-event dbe timebase) events)))
          (when events
            (let* (;(interval (+ (md:onset (car events)) (md:duration (car events))))
                   (comp-id (make-composition-id dataset-id composition-id))
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
      dataset)))
#.(clsql:restore-sql-reader-syntax-state)

#.(clsql:locally-enable-sql-reader-syntax)
(defmethod get-composition ((identifier composition-identifier))
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
    (when (and db-events timebase)
      (dolist (e db-events)
        (push (db-event->music-event e timebase) events))
      (let* ((composition 
              (make-instance 'music-composition
                             :id identifier
                             :description description
                             :timebase timebase)))
        (sequence:adjust-sequence composition (length events)
                                  :initial-contents (nreverse events))
        composition))))
#.(clsql:restore-sql-reader-syntax-state) 

#.(clsql:locally-enable-sql-reader-syntax)
(defmethod get-event ((identifier event-identifier))
  "Returns nil when the event doesn't exist."
  (let* ((dataset-id (get-dataset-index identifier))
         (composition-id (get-composition-index identifier))
         (event-id (get-event-index identifier))
         (composition-where-clause [and [= [dataset-id] dataset-id]
                                   [= [composition-id] composition-id]])
         (event-where-clause [and [= [dataset-id] dataset-id]
                             [= [composition-id] composition-id]
                             [= [event-id] event-id]])
         (timebase
          (car (clsql:select [timebase] :from [mtp-composition]
                             :where composition-where-clause
                             :flatp t :field-names nil)))
         (db-event (car (apply #'clsql:select
                               (append *event-attributes*
                                       (list :from [mtp-event]
                                             :where event-where-clause))))))
    (when (and timebase db-event)
      (db-event->music-event db-event timebase))))
#.(clsql:restore-sql-reader-syntax-state) 

(defun db-event->music-event (db-event timebase)
  (let* ((event-id (make-event-id (first db-event)
				 (second db-event)
				 (third db-event)))
         (music-event (make-instance 'music-event
				   :id event-id
                                   :timebase timebase)))
    (do* ((slts *md-music-slots* (cdr slts))
          (db-atts (nthcdr 3 db-event) (cdr db-atts)))
         ((null slts) music-event)
      (if (member (car slts) *md-time-slots* :test #'eql)
          (setf (slot-value music-event (car slts)) (convert-time-slot (car db-atts) timebase))
          (setf (slot-value music-event (car slts)) (car db-atts))))))

(defun convert-time-slot (value timebase)
  "Convert native representation of time into a representation where
    a crotchet has a value of 96."
  (if (or (null value) (null timebase))
      nil
      (let ((multiplier (/ 96 timebase)))
        (* value multiplier)))) 




;; Detritus

;; (defmethod crotchet ((mo music-object))
;;   (/ (timebase mo) 4))

;; (defmethod timebase ((id composition-identifier))
;;   (timebase (get-composition id)))

;; (defgeneric get-alphabet (attribute dataset))


;; #.(clsql:locally-enable-sql-reader-syntax)
;; (defmethod crotchet ((event music-event))
;;   (let ((timebase 
;;          (car (clsql:select [timebase] :from [mtp-composition]
;;                             :where [and [= [dataset-id] (get-dataset-index (ident event))] [= [composition-id] (get-composition-index (ident event))]]
;;                             :flatp t 
;;                             :field-names nil))))
;;     (/ timebase 4)))
;; #.(clsql:restore-sql-reader-syntax-state)

;; #.(clsql:locally-enable-sql-reader-syntax)
;; (defun get-mtp-alphabet (attribute &rest dataset-ids)
;;   (clsql:select attribute :from 'mtp-event
;;                 :where [in [slot-value 'mtp-event 'dataset-id] dataset-ids]
;;                 :order-by attribute
;;                 :flatp t 
;;                 :field-names nil 
;;                 :distinct t))
;; #.(clsql:restore-sql-reader-syntax-state)


;;(defun get-db-event-sequence (dataset-id composition-id)
;;  (composition->monody
;;   (get-composition 
;;    (make-composition-id dataset-id composition-id))))

;; (defun get-db-event-sequences (&rest dataset-ids)
;;   (let ((compositions '()))
;;     (dolist (dataset-id dataset-ids (nreverse compositions))
;;       (let ((d (md:get-dataset 
;;                 (make-dataset-id dataset-id))))
;;         (sequence:dosequence (c d)
;;           (push (md:composition->monody c) compositions))))))
