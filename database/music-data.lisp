;;;; ======================================================================
;;;; File:       music-data.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2002-10-09 18:54:17 marcusp>                           
;;;; Time-stamp: <2017-06-20 14:49:41 peter>                           
;;;; ======================================================================
;;;;
;;;; Description ==========================================================
;;;; ======================================================================
;;;; 
;;;; Provides a database structure for storing and querying melodic
;;;; data in an sql database. The database contains tables for
;;;; datasets, mtp-compositions and mtp-events where each event is identified
;;;; a unique set of three keys: dataset-id, composition-id and
;;;; event-id. Selector functions are provided for selecting
;;;; attributes of events, compositions and datasets through their
;;;; ids.
;;;;
;;;;
;;;; Todo (features) ======================================================
;;;; ======================================================================
;;;;
;;;; 
;;;; Todo (coding) ========================================================
;;;; ======================================================================
;;;;
;;;; 1. Rewrite with less copying and pasting.
;;;; 2. Update status messages to use utils:message
;;;; 3. Incorporate additional progress bars where appropriate
;;;; 4. Equate mtp-event class with music-event
;;;;

(cl:in-package #:idyom-db)

(defun import-common-practice-tonal (&key (id 1))
  (idyom-db:import-data :krn (merge-pathnames
			      (make-pathname :directory
					     '(:relative "music-corpora"
					       "polyphony" "common-practice-tonal-music"
					       "krn"))
			      cl-user::*idyom-code-root*)
			"A selection of Western common-practice tonal music"
			id))

(defun import-pop (&key (id 2))
  (idyom-db:import-data :mcgill (merge-pathnames
			      (make-pathname :directory
					     '(:relative "music-corpora"
					       "polyphony" "pop-billboard"))
			      cl-user::*idyom-code-root*)
			"The McGill Billboard corpus"
			id))

(defun import-jazz (&key (id 3))
  (idyom-db:import-data :jazz (merge-pathnames
			      (make-pathname :directory
					     '(:relative "music-corpora"
					       "polyphony" "jazz-irb"))
			      cl-user::*idyom-code-root*)
			"The iRb jazz corpus"
			id))

(defun import-four-bach-chorales (&key (id 4))
  (idyom-db:import-data :krn (merge-pathnames
			      (make-pathname :directory
					     '(:relative "music-corpora"
					       "polyphony" "four-bach-chorales"
					       "krn"))
			      cl-user::*idyom-code-root*)
			"Four example Bach chorales"
			id))

(defun import-four-pop-pieces (&key (id 5))
  (idyom-db:import-data :mcgill (merge-pathnames
			      (make-pathname :directory
					     '(:relative "music-corpora"
					       "polyphony" "four-pop-pieces"))
			      cl-user::*idyom-code-root*)
			"Four example pop pieces"
			id))

(defun db-backup ()
  (let* ((db-filename (pathname (slot-value clsql-sys:*default-database*
				  'CLSQL-SYS:NAME)))
	 (backups-folder (make-pathname 
			  :directory (append (pathname-directory
					      db-filename)
					     '("backups"))))
	 (backup-file-name (pathname (format nil "db-backup-~A.sqlite"
				   (get-universal-time))))
	 (backup-full-filename
	  (merge-pathnames backups-folder backup-file-name)))
    (utils:message
     (format nil "Backing up the IDyOM database to ~A.~%"
	     (namestring backup-full-filename))
     :detail 1)
    (ensure-directories-exist backups-folder)
    (utils:copy-file db-filename backup-full-filename)))

(defun check-db ()
  "Top-level function for checking that the database has an 
   up-to-date structure. If out-of-date structure is found, the 
   user is given the option to upgrade it."
  (if (not (and (db-check-attribute-existence "MTP_EVENT" "TONIC")
		(db-check-attribute-existence "MTP_EVENT" "SUBVOICE")
		(db-check-attribute-existence "MTP_EVENT" "INSTRUMENT")
		(db-check-attribute-existence "MTP_EVENT" "INSTRUMENT_CLASS")
		(db-check-attribute-existence "MTP_EVENT" "INSTRUMENT_GROUP")
		(db-check-attribute-existence "MTP_EVENT" "BAR")
		(db-check-attribute-existence "MTP_EVENT" "POSINBAR")
		(db-check-attribute-class "MTP_EVENT" "ONSET" :float)
		(db-check-attribute-class "MTP_EVENT" "CPITCH" :float)
		(db-check-attribute-class "MTP_EVENT" "DUR" :float)
		(db-check-attribute-class "MTP_EVENT" "DELTAST" :float)
		(db-check-attribute-class "MTP_EVENT" "BIOI" :float)
		(db-check-attribute-class "MTP_EVENT" "BARLENGTH" :float)
		(db-check-attribute-class "MTP_EVENT" "PULSES" :float)
		(db-check-attribute-class "MTP_EVENT" "TEMPO" :float)
		(db-check-attribute-class "MTP_EVENT" "VERTINT12" :float)
		(db-check-table-existence "MTP_BARLINE")))
      (if (utils:ask-user-y-n-question
	   "Old database structure detected. Would you like to upgrade it?")
	  (progn
	    (db-backup)
	    (utils:message "Attempting to upgrade database.")
	    (utils:message "Adding new event attributes...")
	    ;; When adding new basic viewpoints, new db-check-attribute-existence
	    ;; functions should be added to the END of the current block.
	    (db-check-attribute-existence "MTP_EVENT" "SUBVOICE"
					  :desired-attribute-type "VARCHAR"
					  :update t)
	    (db-check-attribute-existence "MTP_EVENT" "INSTRUMENT"
					  :desired-attribute-type "VARCHAR"
					  :update t)
	    (db-check-attribute-existence "MTP_EVENT" "INSTRUMENT_CLASS"
					  :desired-attribute-type "VARCHAR"
					  :update t)
	    (db-check-attribute-existence "MTP_EVENT" "INSTRUMENT_GROUP"
					  :desired-attribute-type "VARCHAR"
					  :update t)
	    (db-check-attribute-existence "MTP_EVENT" "TONIC"
					  :desired-attribute-type "INTEGER"
					  :update t)
	    (db-check-attribute-existence "MTP_EVENT" "BAR"
					  :desired-attribute-type "INTEGER"
					  :update t)
	    (db-check-attribute-existence "MTP_EVENT" "POSINBAR"
					  :desired-attribute-type "FLOAT"
					  :update t)
	    (utils:message "Changing event attribute types...")
	    ;; Warning: an error will be thrown if the old database
	    ;; (with the columns just added) doesn't have a matching
	    ;; column structure to that specified in this package.
	    (db-update-types "MTP_EVENT" 'mtp-event :verbose nil)
	    (utils:message "Adding barlines database...")
	    (db-check-table-existence "MTP_BARLINE" :update t)
	    (utils:message "Database upgrade complete."))
	  (format t "Old database left unmodified."))))

(defun db-check-attribute-existence (table-sqlite-name
				     attribute-name &key desired-attribute-type
						      (update nil) (verbose nil))
  "Checks whether an attribute with name <attribute-name>
  is present in table <table-name>.
  Returns t if this is true and nil if not. Additionally,
  if <update> is t, the attribute is added to the table.
  <update> may only be set to t if the type of this 
  attribute is provided in <desired-attribute-type>.
  <table-sqlite-name> should be a string, e.g. \"MTP_EVENT\".
  <attribute-name> should be a string, e.g. \"ONSET\".
  <desired-attribute-type> should be a keyword, e.g. :integer.
  <update> and <verbose> should both be Booleans."
  (if (and update (null desired-attribute-type))
      (error "<update> may only be set to t if <desired-attribute-type> is provided."))
  (let ((current-attributes (clsql:list-attributes table-sqlite-name)))
    (if (not (member attribute-name current-attributes :test #'string=))
	(progn
	  (if verbose (format t "Attribute ~A not found in database.~%"
			      attribute-name))
	  (if update
	      (db-insert-attribute table-sqlite-name attribute-name
				   desired-attribute-type :verbose verbose))
	  nil)
	t)))

(defun db-check-table-existence (table-sqlite-name
				 &key (update nil) (db *default-database*))
  (if (member table-sqlite-name (clsql:list-tables) :test #'string=)
      t
      (if update
	  (create-view-from-class 'mtp-barline :database db))))

(defun db-check-attribute-class (table-sqlite-name
				 attribute-name desired-attribute-type
				 &key (verbose nil))
  "Checks whether the type of the attribute <attribute-name>
   is equal to <desired-attribute-type>, as desired.
   Returns t if this is true and nil if not.
  <table-sqlite-name> should be a string, e.g. \"MTP_EVENT\".
  <table-clsql-name> should be a symbol, e.g. 'mtp-event.
  <attribute-name> should be a string, e.g. \"ONSET\".
  <desired-attribute-type> should be a keyword, e.g. :integer.
  <verbose> should be a Boolean."
  (let ((current-type (second (car (remove-if-not
				    #'(lambda (x)
					(string= (first x) attribute-name))
				    (clsql:list-attribute-types table-sqlite-name))))))
    (if (eql current-type desired-attribute-type)
	t
	(progn
	  (if verbose (format t "Attribute ~A had type ~A, not ~A as required.~%"
			      attribute-name current-type desired-attribute-type))
	  nil))))

(defun db-insert-attribute (table-sqlite-name
			    attribute-name attribute-type
			    &key (verbose t))
  "Inserts an attribute with name <attribute-name> and type
  <attribute-type> to the table with name <table-sqlite-name>. If
  <verbose> is t, a status message is also printed."
  (if verbose
      (format t "Adding column ~A (~A) to table ~A...~%"
	      attribute-name attribute-type table-sqlite-name))
  (clsql:execute-command
   (format nil "ALTER TABLE ~A ADD COLUMN ~A ~A;"
	   table-sqlite-name attribute-name attribute-type)))

(defun db-update-types (table-sqlite-name table-clsql-name
			&key (verbose t))
  "Updates the types of the SQL table with name <table-sqlite-name>
   to match the types in the corresponding view-class
   named <table-clsql-name>. This is achieved by creating a new 
   table with the correct structure and then copying the data 
   over to this table.
   Warning 1: this function assumes that the original database
   has the same structure as the view-class (in terms of number
   and order of columns).
   Warning 2: this function will not preserve 
   any structure of the table (e.g. triggers) that is not specified
   within the view-class.
   Example usage: (db-update-types \"MTP_EVENT\" 'mtp-event),
   without the escape backslashes."
  (if verbose
      (format t "Updating the type definitions for table ~A/~A.~%"
	      table-sqlite-name table-clsql-name))
  (clsql:start-transaction)
  (clsql:execute-command
   (format nil "ALTER TABLE ~A RENAME TO BACKUP;"
	   table-sqlite-name))
  (create-view-from-class table-clsql-name)
  ;; Check that the columns match up
  (let* ((old-attributes (clsql:list-attributes "BACKUP"))
	 (new-attributes (clsql:list-attributes table-sqlite-name)))
    (if (equal old-attributes new-attributes)
	(progn
	  (clsql:execute-command
	   (format nil "INSERT INTO ~A SELECT * FROM BACKUP"
		   table-sqlite-name))
	  (clsql:execute-command "DROP TABLE BACKUP")
	  (clsql:commit))
	(progn
	  (clsql:rollback)
	  (error (format nil "Could not update column types because the original attributes:~%~A~% did not match up with the new attributes specified in the idyom-db package:~%~A.~%"
			 old-attributes new-attributes))))))

(defclass thread-safe-db-obj (clsql-sys:standard-db-object)
  nil
  (:metaclass clsql-sys::standard-db-class))

(defmethod clsql-sys::choose-database-for-instance
    ((object thread-safe-db-obj) &optional database)
  (or database clsql-sys:*default-database*))

#.(clsql:locally-enable-sql-reader-syntax)

(clsql:def-view-class mtp-dataset (thread-safe-db-obj)
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
representing the timebase and chromatic mapping for middle C of the
original encoding; and <compositions> which defines a one to many join
with the composition table based on dataset-id in both tables."))

(clsql:def-view-class mtp-composition (thread-safe-db-obj)
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

(clsql:def-view-class mtp-event (thread-safe-db-obj)
  ;; Any new basic viewpoints should be added to the END of the current block.
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
    :type float ;; originally integer
    :initarg :onset
    :initform 0.0 
    :reader event-onset)
   (cpitch 
    :type float ;; originally integer
    :initarg :cpitch
    :initform 60.0
    :reader event-cpitch)
   (mpitch 
    :type integer
    :initarg :mpitch
    :initform 35.0
    :reader event-mpitch)
   (accidental
    :type integer
    :initarg :accidental
    :initform 0
    :reader event-accidental)
   (dur
    :type float ;; originally integer
    :initarg :dur
    :initform 24.0
    :reader event-dur)
   (deltast
    :type float ;; originally integer
    :initarg :deltast
    :initform 0.0
    :reader event-deltast)
   (bioi
    :type float ;; originally integer
    :initarg :bioi
    :initform 0.0 
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
    :type float ;; originally integer
    :initarg :barlength
    :initform 96.0
    :reader event-barlength)
   (pulses
    :type float ;; originally integer
    :initarg :pulses
    :initform 4.0
    :reader event-pulses)
   (phrase
    :type integer
    :initarg :phrase
    :initform 0
    :reader event-phrase)
   (tempo
    :type float ;; originally integer
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
   (vertint12
    :type float ;; originally integer
    :initarg :vertint12
    :initform nil
    :reader event-vertint12)
   (voice
    :type integer
    :initarg :voice
    :initform 1
    :reader event-voice)
   (subvoice
    :type string
    :initarg :subvoice
    :initform "(1)"
    :reader event-subvoice)
   (instrument
    :type string
    :initarg :instrument
    :initform nil
    :reader event-instrument)
   (instrument-class
    :type string
    :initarg :instrument-class
    :initform nil
    :reader event-instrument-class)
   (instrument-group
    :type string
    :initarg :instrument-group
    :initform nil
    :reader event-instrument-group)
   (tonic
    :type integer
    :initarg :tonic
    :initform 0
    :reader event-tonic)
   (bar
    :type integer
    :initarg :bar
    :initform nil
    :reader event-bar)
   (posinbar
    :type float
    :initarg :posinbar
    :initform nil
    :reader event-posinbar))
  (:base-table mtp_event)
  (:documentation "A view class defining a table for events. The
<dataset-id>, <composition-id> and <event-id> slots are integers which
together uniquely identify an event. The remaining slots contain event
attribute values as follows: <onset> the onset time of the event; <cpitch>
the chromatic pitch of the event; <mpitch> the morphetic pitch of the
event; <keysig> an integer representing the number of sharps or flats in
the key signature; <tonic> an integer representing the pitch class of
the tonic; <mode> 0 for major and 9 for minor; <barlength> an
integer representing the number of basic time units in a bar; <pulses>
an integer number of pulses in a bar; <phrase> 1 if the event is the
first in a phrase and 0 otherwise; <tempo> is the tempo in crotchet bpm;
<dyn> represents the dynamics of the event; <voice> is the voice
no. in which the event occurs; <subvoice> identifies the subvoice 
in which the event occurs; <instrument>, <instrument-class>, and
<instrument-group> are strings describing the instrumentation
of the musical event at various levels of detail; <bar> identifies
the bar in which the event occurs; <posinbar> indicates the distance
of the current event from the start of the current bar in basic
time units." ))

(clsql:def-view-class mtp-barline (thread-safe-db-obj)
  ((bar 
    :db-kind :key
    :db-constraints :not-null
    :initarg :bar
    :column bar
    :type integer
    :reader bar
    :initarg bar)
   (dataset-id
    :db-kind :key
    :db-constraints :not-null
    :initarg :dataset-id 
    :column dataset_id
    :reader dataset-id 
    :type integer)
   (composition-id
    :db-kind :key
    :db-constraints :not-null
    :initarg :composition-id 
    :column composition_id
    :reader composition-id 
    :type integer)
   (onset
    :type float
    :initarg :onset
    :column onset
    :reader onset))
  (:base-table mtp_barline)
  (:documentation "A view class for a table containing barline
information. <bar> corresponds to the bar number under consideration.
<dataset-id> and <composition-id> identify which composition we 
are talking about. <onset> identifies the onset of the bar
under consideration."))

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
		  :tonic          ',(event-tonic e)
		  :mode           ',(event-mode e)
		  :barlength      ',(event-barlength e)
		  :pulses         ',(event-pulses e)
		  :phrase         ',(event-phrase e)
		  :tempo          ',(event-tempo e)
		  :dyn            ',(event-dyn e)
		  :ornament       ',(event-ornament e)
		  :articulation   ',(event-articulation e)
		  :comma          ',(event-comma e)
		  :voice          ',(event-voice e)
		  :vertint12      ',(event-vertint12 e)
		  :subvoice       ',(event-subvoice e)
		  :instrument     ',(event-instrument e)
		  :instrument-class      ',(event-instrument-class e)
		  :instrument-group      ',(event-instrument-group e)
		  :bar            ',(event-bar e)
		  :posinbar       ',(event-posinbar e)))


;; Inserting and deleting datasets 
;;================================

(defmethod import-data ((type (eql :lisp)) filename description id)
  (declare (ignore description))
  (with-open-file (s filename :direction :input)
    (insert-dataset (utils:read-object-from-file filename) id)))

(defmethod export-data ((d mtp-dataset) (type (eql :lisp)) dir &key filename)
  (let* ((dir-path (pathname dir))
	 (filename (if filename filename "dataset.lisp"))
	 (file-path (merge-pathnames dir-path (pathname filename))))
    (with-open-file (s file-path :direction :output :if-exists :supersede
		       :if-does-not-exist :create)
      (write (dataset->lisp d) :stream s))
    nil))

(defun copy-datasets (target-id source-ids &optional description exclude)
  "Copy datasets specified by SOURCE-IDS to a new dataset specified by
TARGET-ID. Optionally provide a DESCRIPTION for the new dataset (the
default is the description of the first dataset in
SOURCE-IDS). EXCLUDE is a list of lists, containing compositions-ids
to exclude for each dataset specified in SOURCE-IDS."
  (let* ((datasets (mapcar #'get-dataset source-ids))
         (datasets (mapcar #'dataset->lisp datasets))
         (result (subseq (car datasets) 0 3)))
    (do ((d datasets (cdr d))
         (e exclude (cdr e)))
        ((null d))
      (setf result (append result (utils:remove-by-position (subseq (car d) 3) (car e)))))
    (insert-dataset result target-id))
  (when description
    (clsql:update-records [mtp-dataset] :av-pairs `((description ,description)) :where [= [dataset-id] target-id]))
  nil)

(defun dataset->lisp (mtp-dataset)
  "Converts a dataset to a LISP representation."
  (let ((dataset nil))
    (dolist (c (dataset-compositions mtp-dataset))
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
		      (list :tonic (event-tonic e))
                      (list :mode (event-mode e))
                      (list :barlength (event-barlength e))
                      (list :pulses (event-pulses e))
                      (list :phrase (event-phrase e))
                      (list :voice (event-voice e))
                      (list :ornament (event-ornament e))
                      (list :comma (event-comma e))
                      (list :vertint12 (event-vertint12 e))
                      (list :articulation (event-articulation e))
                      (list :dyn (event-dyn e))
		      (list :tempo (event-tempo e))
		      (list :subvoice (event-subvoice e))
		      (list :instrument (event-instrument e))
		      (list :instrument-class (event-instrument-class e))
		      (list :instrument-group (event-instrument-group e))
		      (list :bar (event-bar e))
		      (list :posinbar (event-posinbar e)))
                composition))
        (push (nreverse composition) dataset)))
    (append (list (dataset-description mtp-dataset) 
                  (dataset-timebase mtp-dataset)
                  (dataset-midc mtp-dataset))
            (nreverse dataset))))

(defun insert-dataset (data id)
  "Takes a list of compositions <compositions> and creates a dataset
   with id <id>, description <description>, timebase <timebase>, midc
   <midc> and inserts it into the dataset table of the default database.
   Calls <insert-composition> to insert each composition in
   <compositions> into the composition table." 
  (if (null (nthcdr 3 data))
      (utils:message "Cowardly refusing to import an empty dataset."
		     :detail 1)
      (let* ((id (or id (get-next-free-id)))
             (description (nth 0 data))
             (timebase (nth 1 data))
             (midc (nth 2 data))
             (compositions (nthcdr 3 data))
	     (num-compositions (length compositions))
             (composition-id 0)
             (dataset-object (make-instance 'mtp-dataset
                                            :dataset-id id
                                            :description description
                                            :timebase timebase
                                            :midc midc)))
	(utils:message
	 (format nil "Inserting ~A compositions into database: dataset ~A."
		 num-compositions id) :detail 1)
        (clsql:with-transaction () 
          (clsql:update-records-from-instance dataset-object)
          (utils:dolist-pb (c compositions)
            (insert-composition dataset-object c composition-id)
            (when (cdr c) (incf composition-id)))))))

(defmethod insert-composition ((d mtp-dataset) composition id)
  "Takes a preprocessed composition <composition> and
   creates a composition object with composition-id <id> and the same
   dataset-id as <dataset> and inserts it into the composition table of
   the default database. Calls <insert-event> to insert each
   event of <composition> into the event table."
  ;; Originally <composition> objects took the form of a list,
  ;; the first element of which was the file description and the
  ;; second element of which was the list of processed events.
  ;; We are in the process of upgrading to a new format where
  ;; <composition> objects take the form of assoc-lists.
  ;; However, not all import modules support this format, so
  ;; currently both formats must be supported in this function.
  ;; We can test for the new format by seeing if the first element
  ;; of <composition> is of type list.
  (let* ((format (if (listp (car composition)) :new :old))
	 (description (case format
			(:old (format nil "~D" (car composition)))
			(:new (cdr (assoc :description composition)))
			(otherwise (error "Invalid case"))))
	 (events (case format
		   (:old (cdr composition))
		   (:new (cdr (assoc :events composition)))
		   (otherwise (error "Invalid case"))))
	 (bar-nums (if (eql format :new) (cdr (assoc :bar-nums composition))))
	 (bar-onsets (if (eql format :new) (cdr (assoc :bar-onsets composition)))))
    (assert (eql (length bar-nums) (length bar-onsets)))
    (if (null events)
        (format t "~&Skipping empty composition: ~A.~%" description)
        (let ((count (length events))
              (composition-object (make-instance 'mtp-composition
                                                 :dataset-id (dataset-id d)
                                                 :timebase (dataset-timebase d)
                                                 :composition-id id
                                                 :description description)))
          (setf (slot-value composition-object 'dataset-id) (dataset-id d))
          (clsql:update-records-from-instance composition-object)
          (dotimes (event-id count)
            (insert-event composition-object (nth event-id events) event-id))
	  (loop
	     for bar-num in bar-nums
	     for bar-onset in bar-onsets
	     do (insert-barline composition-object bar-num bar-onset))))))

(defmethod insert-event ((composition mtp-composition) event id)
  "Takes a preprocessed event tuple <event> and
   creates an event object with event-id <id> and the same dataset-id and
   composition-id as <composition> and inserts it into the event table of
   the default database."
  (flet ((coerce-to-float (x)
	   (if (null x) x (coerce x 'float)))
	 (coerce-to-string (x)
	   (if (null x) x (prin1-to-string x))))
    (let ((event-object 
	   (make-instance
	    'mtp-event
	    :dataset-id (dataset-id composition)
	    :composition-id (composition-id composition)
	    :event-id id
	    :onset      (coerce-to-float (cadr (assoc :onset event)))
	    :deltast    (coerce-to-float (cadr (assoc :deltast event)))
	    :bioi       (coerce-to-float (cadr (assoc :bioi event)))
	    :cpitch     (coerce-to-float (cadr (assoc :cpitch event)))
	    :mpitch     (cadr (assoc :mpitch event))
	    :accidental (cadr (assoc :accidental event))
	    :dur        (coerce-to-float (cadr (assoc :dur event)))
	    :keysig     (cadr (assoc :keysig event))
	    :tonic      (cadr (assoc :tonic event))
	    :mode       (cadr (assoc :mode event))
	    :barlength  (coerce-to-float (cadr (assoc :barlength event)))
	    :pulses     (coerce-to-float (cadr (assoc :pulses event)))
	    :phrase     (cadr (assoc :phrase event))
	    :dyn        (cadr (assoc :dyn event))
	    :tempo      (coerce-to-float (cadr (assoc :tempo event)))
	    :ornament   (cadr (assoc :ornament event))
	    :comma      (cadr (assoc :comma event))
	    :articulation (cadr (assoc :articulation event))
	    :vertint12  (coerce-to-float (cadr (assoc :vertint12 event)))
	    :voice      (cadr (assoc :voice event))
	    :subvoice   (coerce-to-string (cadr (assoc :subvoice event)))
	    :instrument       (coerce-to-string (cadr (assoc :instrument event)))
	    :instrument-class (coerce-to-string (cadr (assoc :instrument-class event)))
	    :instrument-group (coerce-to-string (cadr (assoc :instrument-group event)))
	    :bar (cadr (assoc :bar event))
	    :posinbar (coerce-to-float (cadr (assoc :posinbar event))))))
      (clsql:update-records-from-instance event-object))))

(defmethod insert-barline ((composition mtp-composition) bar onset)
  "Inserts a barline into MTP_BARLINE with the same dataset-id and 
composition-id as <composition>.
<bar> is an integer corresponding to the bar number; <onset> is a number
corresponding to the onset of that bar."
  ;;(assert integerp bar) ;; for some reason these assertions cause compilation errors
  ;;(assert numberp onset)
  (flet ((coerce-to-float (x)
	   (if (null x) x (coerce x 'float))))
    (let ((barline-object (make-instance 'mtp-barline
					 :dataset-id (dataset-id composition)
					 :composition-id (composition-id composition)
					 :bar bar
					 :onset (coerce-to-float onset))))
      (clsql:update-records-from-instance barline-object))))

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
	(clsql:delete-records :from 'mtp_barline
                              :where [= [slot-value 'mtp-barline 'dataset-id]
			      dataset-id])
        (format t "~%Dataset ~S deleted from the database." dataset-id))
      (format t "~%Dataset ~S does not exist in the database." dataset-id)))


(defun initialise-database (&optional (db *default-database*))
  "Drops the dataset, composition and event tables from <db> and recreates
   fresh tables in their place." 
  (ignore-errors
    (drop-table 'mtp-dataset :database db)
    (drop-table 'mtp-composition :database db)
    (drop-table 'mtp-event :database db)
    (drop-table 'mtp-barline :database db))
  (create-view-from-class 'mtp-dataset :database db)
  (create-view-from-class 'mtp-composition :database db)
  (create-view-from-class 'mtp-event :database db)
  (create-view-from-class 'mtp-barline :database db))


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
  "Returns a list of composition objects whose dataset-id is <dataset-id>." 
  (sort (dataset-compositions (get-dataset dataset-id))
        #'< :key #'composition-id))
 
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
  (sort (composition-events (get-composition dataset-id composition-id))
        #'< :key #'event-id))

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
         (accessor-symbol (find-symbol accessor-name (find-package :idyom-db))))
    (setf (slot-value e accessor-symbol) value)))

(defmethod get-attribute ((e mtp-event) attribute)
  "Returns the value for slot <attribute> in event object <e>."
  (let* ((accessor-name (string-upcase (symbol-name attribute)))
	 (accessor-name 
	  ;; for dataset-id, composition-id, event-id
	  (if (string= (subseq (reverse accessor-name) 0 2) "DI")
	      accessor-name
	      (concatenate 'string "EVENT-" accessor-name)))
         (accessor-symbol (find-symbol accessor-name (find-package :idyom-db))))
    (funcall accessor-symbol e)))

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
		 :tonic (event-tonic e)
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
                 :voice (event-voice e)
		 :vertint12 (event-vertint12 e)
		 :subvoice (event-subvoice e)
		 :instrument (event-instrument e)
		 :instrument-class (event-instrument-class e)
		 :instrument-group (event-instrument-group e)
		 :bar (event-bar e)
		 :posinbar (event-posinbar e)))


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
                                 [and [= [slot-value 'mtp-event 'composition-id]
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
               '(cpitch mpitch accidental dur deltast bioi keysig tonic mode barlength
                 pulses phrase dyn tempo voice subvoice ornament articulation comma
		 instrument instrument-class instrument-group)
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
  (when (clsql:table-exists-p [mtp-dataset])
    (if quick
        (clsql:print-query [select [dataset-id][description] :from [mtp-dataset] :order-by [dataset-id]])
        (mapc #'(lambda (id) (describe-dataset id 
                                               :attributes attributes
                                               :output-stream output-stream
                                               :mappings mappings
                                               :verbose verbose))
              (clsql:select [dataset-id] :from 'mtp_dataset :order-by [dataset-id]
                            :flatp t :field-names nil)))))
  
(defun get-sequence (attribute dataset-id composition-id event-id length)
  "Returns a list of the values of <attribute> for the events with
composition-id <composition-id>, dataset-id <dataset-id> and event-ids
from <event-id> to (+ <event-id> <length>)."
  (let ((event-id-2 (+ event-id length)))
    (clsql:select attribute :from 'mtp-event :order-by [event-id]
                  :where [and [= [slot-value 'mtp-event 'dataset-id] dataset-id]
                              [= [slot-value 'mtp-event 'composition-id] composition-id]
                              [>= [slot-value 'mtp-event 'event-id] event-id]
                              [< [slot-value 'mtp-event 'event-id] event-id-2]]
                  :field-names nil :flatp t)))

#.(clsql:restore-sql-reader-syntax-state)
