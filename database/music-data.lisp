;;;; ======================================================================
;;;; File:       music-data.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@eecs.qmul.ac.uk>
;;;; Created:    <2002-10-09 18:54:17 marcusp>                           
;;;; Time-stamp: <2014-03-28 13:06:20 marcusp>                           
;;;; ======================================================================
;;;;
;;;; DESCRIPTION 
;;;; 
;;;; Provides a database structure for storing and querying melodic
;;;; data in an sql database. The database contains tables for
;;;; datasets, mtp-compositions and mtp-events where each event is identified
;;;; a unique set of three keys: dataset-id composition-id and
;;;; event-id. Selector functions are provided for selecting
;;;; attributes of events, compositions and datasets through their
;;;; ids.
;;;;
;;;; ======================================================================

(cl:in-package #:mtp-admin)

#.(clsql:locally-enable-sql-reader-syntax)

(clsql:def-view-class mtp-dataset ()
  ((dataset-id
    :db-kind :key
    :db-constraints :not-null
    :column dataset_id 
    :type integer
    :reader dataset-id 
    :initarg :dataset-id)
   (description
    :type string
    :initarg :description
    :column description
    :reader dataset-description)
   (timebase
    :type integer
    :initarg :timebase
    :reader dataset-timebase)
   (midc
    :type integer
    :initarg :midc
    :reader dataset-midc)
   (compositions
    :db-kind :join
    :reader dataset-compositions
    :db-info (:join-class mtp-composition
                          :home-key dataset-id
                          :foreign-key dataset-id
                          :set t)))
  (:base-table mtp_dataset)
  (:documentation "A view class for a table containing datasets
providing the following slots: <dataset-id> which is a unique integer
key value for the dataset, <dataset-description> which is an arbitrary
string describing the dataset; <timebase> and <midc> which are integers
representing the timebase and chromatic mapping for middle c of the
original encoding; and <compositions> which defines a one to many join
with the composition table based on dataset-id in both tables."))

(clsql:def-view-class mtp-composition ()
  ((composition-id
    :db-kind :key
    :db-constraints :not-null
    :column composition_id 
    :type integer
    :reader composition-id 
    :initarg :composition-id)
   (dataset-id
    :db-kind :key
    :db-constraints :not-null
    :column dataset_id
    :reader dataset-id 
    :type integer)
   (timebase
    :type integer
    :initarg :timebase
    :reader composition-timebase)
   (description
    :type string
    :initarg :description
    :column description
    :reader composition-description)
   (events
    :db-kind :join
    :reader composition-events
    :db-info (:join-class mtp-event
                          :home-key (composition-id dataset-id)
                          :foreign-key (composition-id dataset-id)
                          :set t)))
  (:base-table mtp_composition)
  (:documentation "A view class for a table containing compositions which
provides the following slots: <dataset-id> which is an integer key
defining which dataset the composition is in; <composition-id> an integer key
for the composition; <composition-description> which is an arbitrary string
describing the composition; and <events> which defines a one to many join
with the event table based on the dataset-id and composition-id keys of
each table."))
                  
(clsql:def-view-class mtp-event ()
  ((event-id 
    :db-kind :key
    :db-constraints :not-null
    :column event_id 
    :type integer
    :reader event-id 
    :initarg :event-id)
   (composition-id
    :db-kind :key
    :db-constraints :not-null
    :initarg :composition-id 
    :column composition_id
    :reader composition-id 
    :type integer)
   (dataset-id
    :db-kind :key
    :db-constraints :not-null
    :initarg :dataset-id 
    :column dataset_id
    :reader dataset-id 
    :type integer)
   (onset
    :type integer
    :initarg :onset
    :initform 0 
    :reader event-onset)
   (cpitch 
    :type integer
    :initarg :cpitch
    :initform 60 
    :reader event-cpitch)
   (mpitch 
    :type integer
    :initarg :mpitch
    :initform 35 
    :reader event-mpitch)
   (accidental
    :type integer
    :initarg :accidental
    :initform 0
    :reader event-accidental)
   (dur
    :type integer
    :initarg :dur
    :initform 24
    :reader event-dur)
   (deltast
    :type integer
    :initarg :deltast
    :initform 0
    :reader event-deltast)
   (bioi
    :type integer
    :initarg :bioi
    :initform 0 
    :reader event-bioi)
   (keysig
    :type integer
    :initarg :keysig
    :initform 0
    :reader event-keysig)
   (mode
    :type integer
    :initarg :mode
    :initform 0
    :reader event-mode)
   (barlength
    :type integer
    :initarg :barlength
    :initform 96
    :reader event-barlength)
   (pulses
    :type integer
    :initarg :pulses
    :initform 4
    :reader event-pulses)
   (phrase
    :type integer
    :initarg :phrase
    :initform 0
    :reader event-phrase)
   (tempo
    :type integer
    :initarg :tempo
    :initform nil
    :reader event-tempo)
   (dyn
    :type integer
    :initarg :dyn
    :initform nil
    :reader event-dyn)
   (ornament
    :type integer
    :initarg :ornament
    :initform nil
    :reader event-ornament)
   (comma
    :type integer
    :initarg :comma
    :initform nil
    :reader event-comma)
   (articulation
    :type integer
    :initarg :articulation
    :initform nil
    :reader event-articulation)
   (voice
    :type integer
    :initarg :voice
    :initform 1
    :reader event-voice))
  (:base-table mtp_event)
  (:documentation "A view class defining a table for events. The
<dataset-id>, <composition-id> and <event-id> slots are integers which
together uniquely identify an eveappnt. The remaining slots contain event
attribute values as follows: <onset> the onset time of the event; <cpitch>
the chromatic pitch of the event; <mpitch> the morphetic pitch of the
event; <keysig> an integer representing the number of sharps or flats in
the key signature; <mode> 0 for major and 9 for minor; <barlength> an
integer representing the number of basic time units in a bar; <pulses>
an integer number of pulses in a bar; <phrase> 1 if the event is the
first in a phrase and 0 otherwise; <tempo> is the tempo in crotchet bpm;
<dyn> represents the dynamics of the event; and <voice> is the voice
no. in which the event occurs." ))

(defmethod make-load-form ((e mtp-event) &optional environment)
  (declare (ignore environment))
  `(make-instance ',(type-of e)
    :event-id       ',(event-id e)
    :composition-id ',(composition-id e)
    :dataset-id     ',(dataset-id e)
    :onset          ',(event-onset e)
    :deltast        ',(event-deltast e)
    :bioi           ',(event-bioi e)
    :cpitch         ',(event-cpitch e)
    :mpitch         ',(event-mpitch e)
    :accidental     ',(event-accidental e)
    :dur            ',(event-dur e)
    :keysig         ',(event-keysig e)
    :mode           ',(event-mode e)
    :barlength      ',(event-barlength e)
    :pulses         ',(event-pulses e)
    :phrase         ',(event-phrase e)
    :tempo          ',(event-tempo e)
    :dyn            ',(event-dyn e)
    :ornament       ',(event-ornament e)
    :articulation   ',(event-articulation e)
    :comma          ',(event-comma e)
    :voice          ',(event-voice e)))
    

;; Inserting and deleting datasets 
;;================================

(defmethod import-data ((type (eql :lisp)) filename description id)
  (declare (ignore description))
  (with-open-file (s filename :direction :input)
    (insert-dataset (utils:read-object-from-file filename) id)))

(defmethod export-data ((d mtp-dataset) (type (eql :lisp)) filename)
  (let ((dataset nil))
    (dolist (c (dataset-compositions d))
      (let ((composition (list (composition-description c))))
        (dolist (e (composition-events c))
          (push (list (list :onset (event-onset e))
                      (list :deltast (event-deltast e))
                      (list :bioi (event-bioi e))
                      (list :dur (event-dur e))
                      (list :cpitch (event-cpitch e))
                      (list :mpitch (event-mpitch e))
                      (list :accidental (event-accidental e))
                      (list :keysig (event-keysig e))
                      (list :mode (event-mode e))
                      (list :barlength (event-barlength e))
                      (list :pulses (event-pulses e))
                      (list :phrase (event-phrase e))
                      (list :voice (event-voice e))
                      (list :ornament (event-ornament e))
                      (list :comma (event-comma e))
                      (list :articulation (event-articulation e))
                      (list :dyn (event-dyn e)))
                composition))
        (push (nreverse composition) dataset)))
    (with-open-file (s filename :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
      (write (append (list (dataset-description d) 
                           (dataset-timebase d)
                           (dataset-midc d))
                     (nreverse dataset))
             :stream s)))
  nil)
  
(defun insert-dataset (data id)
  "Takes a list of compositions <compositions> and creates a dataset
   with id <id>, description <description>, timebase <timebase>, midc
   <midc> and inserts it into the dataset table of the default database.
   Calls <insert-composition> to insert each composition in
   <compositions> into the composition table." 
  (if (null (nthcdr 3 data))
      (print "Cowardly refusing to import an empty dataset.")
      (let* ((id (or id (get-next-free-id)))
             (description (nth 0 data))
             (timebase (nth 1 data))
             (midc (nth 2 data))
             (compositions (nthcdr 3 data))
             (count (length compositions))
             (dataset-object (make-instance 'mtp-dataset
                                            :dataset-id id
                                            :description description
                                            :timebase timebase
                                            :midc midc)))
        (format t "~%Inserting data into database: dataset ~A." id)
        (clsql:with-transaction () 
          (clsql:update-records-from-instance dataset-object)
          (dotimes (composition-id count)
            (insert-composition dataset-object (nth composition-id compositions)
                                composition-id))))))
        
(defmethod insert-composition ((d mtp-dataset) composition id)
  "Takes a preprocessed composition <composition> and
   creates a compositon object with composition-id <id> and the same
   dataset-id as <dataset> and inserts it into the composition table of
   the default database. Calls <insert-event> to insert each
   event of <composition> into the event table."
  (let* ((description (format nil "~D" (car composition)))
         (events (cdr composition))
         (count (length events))
         (composition-object (make-instance 'mtp-composition
                                            :dataset-id (dataset-id d)
                                            :timebase (dataset-timebase d)
                                            :composition-id id
                                            :description description)))
    (setf (slot-value composition-object 'dataset-id) (dataset-id d))
    (clsql:update-records-from-instance composition-object)
    (dotimes (event-id count)
      (insert-event composition-object (nth event-id events) event-id))))

(defmethod insert-event ((composition mtp-composition) event id)
  "Takes a preprocessed event tuple <event> and
   creates an event object with event-id <id> and the same dataset-id and
   composition-id as <composition> and inserts it into the event table of
   the default database."
  (let ((event-object 
         (make-instance 'mtp-event
                        :dataset-id (dataset-id composition)
                        :composition-id (composition-id composition)
                        :event-id id
                        :onset      (cadr (assoc :onset event))
                        :deltast    (cadr (assoc :deltast event))
                        :bioi       (cadr (assoc :bioi event))
                        :cpitch     (cadr (assoc :cpitch event))
                        :mpitch     (cadr (assoc :mpitch event))
                        :accidental (cadr (assoc :accidental event))
                        :dur        (cadr (assoc :dur event))
                        :keysig     (cadr (assoc :keysig event))
                        :mode       (cadr (assoc :mode event))
                        :barlength  (cadr (assoc :barlength event))
                        :pulses     (cadr (assoc :pulses event))
                        :phrase     (cadr (assoc :phrase event))
                        :dyn        (cadr (assoc :dyn event))
                        :tempo      (cadr (assoc :tempo event))
                        :ornament      (cadr (assoc :ornament event))
                        :comma      (cadr (assoc :comma event))
                        :articulation      (cadr (assoc :articulation event))
                        :voice      (cadr (assoc :voice event)))))
    (clsql:update-records-from-instance event-object)))

 
(defun delete-dataset (dataset-id)
  "Deletes the dataset and all compositions and events whose dataset-id
   key value is <dataset-id> from the database." 
  (if (clsql:select 'mtp-dataset :where [= [slot-value 'mtp-dataset 'dataset-id]
                                       dataset-id])
      (progn 
        (clsql:delete-records :from 'mtp_dataset
                              :where [= [slot-value 'mtp-dataset 'dataset-id]
                                        dataset-id])
        (clsql:delete-records :from 'mtp_composition
                              :where [= [slot-value 'mtp-composition 'dataset-id]
                                        dataset-id])
        (clsql:delete-records :from 'mtp_event
                              :where [= [slot-value 'mtp-event 'dataset-id]
                                        dataset-id])
        (format t "~%Dataset ~S deleted from the database." dataset-id))
      (format t "~%Dataset ~S does not exist in the database." dataset-id)))


(defun initialise-database (&optional (db *default-database*))
  "Drops the dataset, composition and event tables from <db> and recreates
   fresh tables in their place." 
  (ignore-errors
    (drop-table 'mtp-dataset :database db)
    (drop-table 'mtp-composition :database db)
    (drop-table 'mtp-event :database db))
  (create-view-from-class 'mtp-dataset :database db)
  (create-view-from-class 'mtp-composition :database db)
  (create-view-from-class 'mtp-event :database db))


;; selector functions
;;====================

 
(defun get-dataset (dataset-id)
  "Returns the dataset object whose datsed-id is <dataset-id>." 
  (car (clsql:select 'mtp-dataset :where [= [slot-value 'mtp-dataset 'dataset-id]
                                        dataset-id]
                     :flatp t)))

(defun get-timebase (dataset-id &optional composition-id)
  "Returns the timebase of the dataset whose id is <dataset-id>." 
  (if (null composition-id)
      (dataset-timebase (get-dataset dataset-id))
      (composition-timebase (get-composition dataset-id composition-id))))

(defun get-midc (dataset-id)
  "Returns the middle-c mapping of the dataset whose id is <dataset-id>." 
  (dataset-midc (get-dataset dataset-id)))

(defun get-description (dataset-id &optional composition-id)
  "If <composition-id> is null returns the description of the dataset whose
   id is <dataset-id>, else returns the description of the composition whose
   dataset-id is <dataset-id> and whose composition-id is <composition-id>."
  (if (null composition-id)
      (dataset-description (get-dataset dataset-id))
      (composition-description (get-composition dataset-id composition-id))))

(defun get-compositions (dataset-id)
  "Returns alist of composition objects whose dataset-id is <dataset-id>." 
  (dataset-compositions (get-dataset dataset-id)))
 
(defun get-composition (dataset-id composition-id)
  "Returns the composition whose dataset-id is <dataset-id> and whose
   composition-id is <composition-id>." 
  (car (clsql:select 'mtp-composition :flatp t
                     :where [and [= [slot-value 'mtp-composition 'dataset-id]
                                    dataset-id]
                                 [= [slot-value 'mtp-composition 'composition-id]
                                    composition-id]])))

(defun get-event (dataset-id composition-id event-id)
  "Returns the event whose dataset-id is <dataset-id>, whose composition-id
   is <composition-id> and whose event-id is <event-id>." 
  (car (clsql:select 'mtp-event :flatp t 
                     :where [and [= [slot-value 'mtp-event 'dataset-id]
                                    dataset-id]
                                 [= [slot-value 'mtp-event 'composition-id]
                                    composition-id]
                                 [= [slot-value 'mtp-event 'event-id]
                                    event-id]])))

(defun get-event-sequence (dataset-id composition-id)
  "Returns a list of event objects whose dataset-id is <dataset-id> and
   whose composition-id is <composition-id>." 
  (composition-events (get-composition dataset-id composition-id)))

(defun get-event-attribute (attribute dataset-id composition-id event-id)
  "Returns the attribute value denoted by <attribute> of the event
   with key values <dataset-id>, <composition-id> and <event-id>." 
  (let ((event (get-event dataset-id composition-id event-id)))
    (get-attribute event attribute)))

(defmethod composition-description ((e mtp-event))
  (composition-description 
   (get-composition (dataset-id e) (composition-id e))))

(defgeneric get-id (object) 
  (:method ((d mtp-dataset)) (list (dataset-id d)))
  (:method ((m mtp-composition)) (list (dataset-id m) (composition-id m)))
  (:method ((e mtp-event)) (list (dataset-id e) (composition-id e) (event-id e)))
  (:documentation "Selector function returning the identifier keys of
a dataset, composition or event object <object>. For events a list of
dataset-id, composition-id and event-id is returned; for compositions
a list of dataset-id and composition-id is returned; and for datasets,
a list containing the dataset-id is returned."))

(defmethod set-attribute ((e mtp-event) attribute value)
  "Sets the value for slot <attribute> in event object <e>."
  (let* ((accessor-name (string-upcase (symbol-name attribute)))
         (accessor-symbol (find-symbol accessor-name (find-package :mtp-admin))))
    (setf (slot-value e accessor-symbol) value)))

(defmethod get-attribute ((e mtp-event) attribute)
  "Returns the value for slot <attribute> in event object <e>."
  (let* ((accessor-name (string-upcase (symbol-name attribute)))
	 (accessor-name 
	  ;; for dataset-id, composition-id, event-id
	  (if (string= (subseq (reverse accessor-name) 0 2) "DI")
	      accessor-name
	      (concatenate 'string "EVENT-" accessor-name)))
         (accessor-symbol (find-symbol accessor-name (find-package :mtp-admin))))
    (funcall accessor-symbol e)))

(defmethod get-attribute ((e music-data::music-event) attribute)
  "Returns the value for slot <attribute> in event object <e>."
  (music-data:get-attribute e attribute))

(defmethod copy-event ((l list))
  (make-instance 'mtp-event))

(defmethod copy-event ((e mtp-event))
  (make-instance 'mtp-event
                 :dataset-id (dataset-id e)
                 :composition-id (composition-id e)
                 :event-id (event-id e)
                 :onset (event-onset e)
                 :deltast (event-deltast e)
                 :bioi (event-bioi e)
                 :cpitch (event-cpitch e)
                 :mpitch (event-mpitch e)
                 :dur (event-dur e)
                 :keysig (event-keysig e)
                 :accidental (event-accidental e)
                 :mode (event-mode e)
                 :barlength (event-barlength e)
                 :pulses (event-pulses e)
                 :phrase (event-phrase e)
                 :dyn (event-dyn e)
                 :tempo (event-tempo e)
                 :ornament (event-ornament e)
                 :comma (event-comma e)
                 :articulation (event-articulation e)
                 :voice (event-voice e)))


;; Utility functions
;;===================

 (defun get-next-free-id (&optional dataset-id composition-id)
  "If <dataset-id> is null returns the lowest whole number which is
   not used as a dataset identifier; 2. else if <composition-id> is null
   returns the lowest whole number not being used as a composition-id
   in the dataset with dataset-id <dataset-id>; otherwise returns the
   lowest whole number not being used as an event-id in the composition
   with composition-id <composition-id> and dataset-id <dataset-id>." 
  (let* ((max-id 
          (cond ((null dataset-id)
                 (clsql:select [max[dataset-id]] :from 'mtp_dataset))
                ((null composition-id)
                 (clsql:select [max[composition-id]] :from 'mtp_composition
                               :where [= [slot-value 'mtp-composition 'dataset-id]
                                         dataset-id]))
                (t (clsql:select [max[event_id]] :from 'mtp_event
                                 :where 
                                 [and [= [slot-value 'mtp-event 'mtp-composition-id]
                                         composition-id]
                                      [= [slot-value 'mtp-event 'dataset-id]
                                         dataset-id]]))))
         (max-id (caar max-id)))
    (if (null max-id) 0 (1+ max-id))))

(defun count-compositions (dataset-id)
  "Returns the number of compositions in the compositions join of the
   dataset with dataset-id <dataset-id>." 
  (length (get-compositions dataset-id)))

(defun count-events (dataset-id &optional (composition-id nil))
  "If <composition-id> is null returns the number of events in the
   dataset with dataset-id <dataset-id>, otherwise returns the number of
   events in the composition with dataset-id <dataset-id> and
   composition-id <composition-id>." 
  (if (null composition-id)
      (car (clsql:select [count[event_id]]
                         :where [= [slot-value 'mtp-event 'dataset-id] dataset-id]
                         :flatp t
                         :from 'mtp_event))
      (length (get-event-sequence dataset-id composition-id))))
               
(defun get-alphabet (attribute &rest dataset-ids)
  "Returns the alphabet from which <attribute> is composed in the
   dataset with dataset <dataset>." 
  (clsql:select attribute :from 'mtp_event
                :where [in [slot-value 'mtp-event 'dataset-id] dataset-ids]
                :order-by attribute
                :flatp t 
                :field-names nil 
                :distinct t))

(defun describe-dataset (dataset-id &key (attributes nil)
                         (output-stream *standard-output*)
                         (mappings nil) (verbose nil))
  "Prints information about the dataset with dataset-id <dataset-id>
including its description, timebase, middle-c mapping, the number of
composition and events in the dataset and the average number of events
per composition, as well as the domains of each event attribute."
  (flet ((print-info (dataset-id)
           (let* ((composition-count (count-compositions dataset-id))
                  (event-count (count-events dataset-id))
                  (average (float (/ event-count composition-count))))
             (format output-stream
                     "~% ~10A ~20A ~20A ~20A" "ID" "No. Compositions"
                     "No. events" "Mean events/composition")
             (format output-stream "~% ~10A ~20A ~20A ~20A"
                     dataset-id composition-count event-count average)))
         (print-description (dataset-id)
           (format output-stream "~2% ~A" (get-description dataset-id)))
         (print-alphabet (attribute dataset-id)
           (let ((alphabet (get-alphabet attribute dataset-id)))
             (format output-stream "~% ~A (~A):~1,18T ~A"
                     attribute (length alphabet) alphabet)))
         (print-timebase-and-midc (dataset-id) 
           (format output-stream "~% ~10A ~A  |  ~10A ~A" "Timebase: "
                   (get-timebase dataset-id) "Middle C: "
                   (get-midc dataset-id)))
         (print-separator (&optional (columns 77))
           (format output-stream "~% ~A"
                   (make-sequence 'string columns :initial-element #\-))))
    (let ((attributes
           (if (null attributes)
               '(cpitch mpitch accidental dur deltast bioi keysig mode barlength
                 pulses phrase dyn tempo voice ornament articulation comma)
               (if (atom attributes) (list attributes) attributes))))
      (print-description dataset-id)
      (print-separator)
      (print-info dataset-id)
      (print-separator)
      (when mappings 
        (print-timebase-and-midc dataset-id) (print-separator))
      (when verbose
        (mapc #'(lambda (a) (print-alphabet a dataset-id)) attributes)
        (print-separator)))))

(defun describe-database (&key (attributes nil)
                          (output-stream *standard-output*)
                          (mappings nil) (verbose nil) (quick t))
  "Prints information about all the datasets in database <db>." 
  (if quick
      (clsql:print-query [select [dataset-id][description] :from [mtp-dataset] :order-by [dataset-id]])
      (mapc #'(lambda (id) (describe-dataset id 
					     :attributes attributes
					     :output-stream output-stream
					     :mappings mappings
					     :verbose verbose))
	    (clsql:select [dataset-id] :from 'mtp_dataset :order-by [dataset-id]
			  :flatp t :field-names nil))))
  
(defun get-sequence (attribute dataset-id composition-id event-id length)
  "Returns a list of the values of <attribute> for the events with
composition-id <composition-id>, dataset-id <dataset-id> and event-ids
from <event-id> to (+ <event-id> <length>)."
  (let ((event-id-2 (+ event-id length)))
    (clsql:select attribute :from 'mtp-event :order-by [event-id]
                  :where [and [= [slot-value 'mtp-event 'dataset-id] dataset-id]
                              [= [slot-value 'mtp-event 'composition-id]
                                 composition-id]
                              [>= [slot-value 'mtp-event 'event-id] event-id]
                              [< [slot-value 'mtp-event 'event-id] event-id-2]]
                  :field-names nil :flatp t)))


(defun list-contents ()
  (clsql:print-query [select [dataset-id][description] :from [mtp-dataset] :order-by [dataset-id]]))


#.(clsql:restore-sql-reader-syntax-state)
