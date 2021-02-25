#!/usr/bin/sbcl --script

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
				       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))


;;; Data representations:
;;; IDyOM DB (event-sequences)
;;; kern2db parser output (name plist-sequence pairs)
;;; PLIST (name uid plist-sequence triples)
;;; Tabular (name uid event-index plist)
;;; CSV (output of csv module)
;;; Sequences (list of lists of event vectors) used as input to model

(let ((idyom-root (sb-ext:posix-getenv "IDYOM_ROOT")))
  (defvar *idyom-root* (or idyom-root
			   "/home/bastiaan/projects/idyom/")))

(let ((*standard-output* *error-output*)) ; re-route standard output to error
  (push *idyom-root* asdf:*central-registry*)
  (ql:quickload "idyom"))

;(cl:in-package #:commands)

(defun connect-to-db (&optional
			(database "/home/bastiaan/projects/idyom/database.sqlite"))
  (clsql:connect (list database) :if-exists :old :database-type :sqlite3))

(defparameter commands nil)

(defmacro defcommand (name args &body body)
  (pushnew name commands)
  `(defun ,name (&rest cli-args)
     (with-cli-options (cli-args)
	 (,@args)
       ,@body)))

(defun music-slot (symbol-or-string)
  (let* ((str (if (stringp symbol-or-string)
		  symbol-or-string
		  (symbol-name symbol-or-string)))
	 (symb (find-symbol (string-upcase str) :music-data)))
    (when (not (member symb md::*music-slots*))
      (error "~A is not a basic attribute in the music data package."
	     symbol-or-string))
    symb))

(defun slot->property (slot)
  (intern (symbol-name slot) :keyword))

(defparameter *music-properties* (mapcar #'slot->property md::*music-slots*))

;; Subset of music-properties
(defparameter *kern-slots*
  '(:onset :dur :deltast :bioi :cpitch :mpitch :accidental :keysig :mode :barlength
    :pulses :phrase :voice :tempo))

(defun unique-representation (filename sequence)
  (cons filename
	(loop for slot in *music-properties* collect
	     (map 'list (lambda (e) (getf e slot)) sequence))))

(defun create-uid (filename sequence)
  (let* ((sequence-data (unique-representation filename sequence))
	 (md5 (sb-md5:md5sum-string (format nil "~a" sequence-data))))
    (string-downcase
     (format nil "~{~X~}" (loop for i below 4 collect (elt md5 i))))))

(defun parse-token (token)
  "Parse a token ocurring in a CSV file into lisp datatype by 
READing it. If token is empty string, return NIL."
  (unless (string-equal token "")
    (read-from-string token)))

(defun read-csv-stream (stream)
  (fare-csv:with-rfc4180-csv-syntax ()
    (fare-csv:read-csv-stream stream)))

(defun read-csv-file (path)
  (fare-csv:with-rfc4180-csv-syntax ()
    (fare-csv:read-csv-file path)))

(defun plists->csv (plists attributes stream)
  (let ((rows (plists->tabular plists attributes)))
    (fare-csv:with-rfc4180-csv-syntax ()
      (fare-csv:write-csv-line
       (append (list "name" "uid" "event")
	       (mapcar (lambda (a) (string-downcase (symbol-name a))) attributes))
       stream)
      (fare-csv:write-csv-lines rows stream))))

(defun tabular->sequences (rows)
  "Find all sequence UIDs, collect all events per sequence uid,
ensure events are consecutive, create rows consisting of event
sequences. Each event is a vector, the first elements of which is
UID, and subsequent elements correspond to (cddr columns)."
  (unless (equal (subseq (car rows) 0 2)
		 (list "uid" "event"))
    (error "First two columns of CSV must be 'uid' and 'event'."))
  (let* ((columns (cddr (car rows)))
	 (rows (cdr rows))
	 (max-indices (make-hash-table :test #'equal)))
    ;; Find sequence lengths
    (dolist (row rows)
      (let ((uid (car row)) (event-index (parse-integer (cadr row))))
	(when (> event-index (gethash uid max-indices 0))
	  (setf (gethash uid max-indices) event-index))))
    ;; Create sequences
    (let ((dataset (make-hash-table :test #'equal)))
      (dolist (row rows)
	(let* ((uid (car row))
	       (event-index (parse-integer (cadr row)))
	       (events (jackdaw:get-or-create-and-set
			uid dataset (make-array (1+ (gethash uid max-indices)))))
	       (event (mapcar #'parse-token (cddr row))))
	  (setf (svref events event-index) (apply #'vector event))))
      ;; Convert sequences to lists
      (values
       columns
       (loop for uid being the hash-keys of max-indices collect
	    (let ((events (map 'list #'identity (gethash uid dataset))))
	      (when (find 0 events)
		(warn "Some events not found in composition ~a" uid))
	      (cons uid events)))))))

(defun plists->tabular (dataset attributes)
  "Construct a tabular representation of dataset with columns: name and uid
followed by ATTRIBUTES. DATASET is a LIST of items. Each item is a LIST of
three elements: the description (name) of a sequence, its uid, the event index
and the event. The sequence is a LIST of PLISTs representing events.
The properties of the PLIST are keyword versions of MD:*MUSIC-SLOTS*."
  (let ((rows))
    (dolist (item dataset)
      (let ((description (car item))
	    (uid (cadr item))
	    (sequence (cddr item)))
	(loop for event in sequence
	   for i below (length sequence) do
	     (let ((row (mapcar (lambda (a) (getf event a)) attributes)))
	       (push (cons description (cons uid (cons i row))) rows)))))
    (reverse rows)))

(defun alist->plist (alist)
  (let ((values
	 (loop for p in *music-properties* collect
	      (cadr (assoc p alist)))))
    (apply #'append (mapcar #'list *music-properties* values))))

(defun kern->plists (dataset)
  (loop for item in dataset collect
       (let* ((name (car item))
	      (sequence (cdr item))
	      (uid (create-uid name sequence))
	      (plists (map 'list #'alist->plist sequence)))
	 (cons name (cons uid plists)))))

(defun idyomdb->-plists (dataset)
  (let ((rows))
    (labels ((event->values (event)
	       (loop for slot in md::*music-slots* collect
		    (when (slot-boundp event slot)
		      (slot-value event slot))))
	     (event->plist (event)
	       (let ((values (event->values event)))
		 (apply #'append (mapcar #'list *music-properties* values)))))
      (dolist (sequence dataset)
	(let* ((description (md:description sequence))
	       (events (map 'list #'event->plist sequence))
	       (uid (create-uid description events)))
	  (push (cons description (cons uid events)) rows)))
      (reverse rows))))

(defun observation-slot (name)
  (find-symbol (format nil "OBSERVE-~a" (string-upcase name))
	       :generative-models))

(defun model-dataset-impl (model dataset
			   &key predict? construct? observe-latent?
			     writer freeze-to read-from)
  "Initialize the output writer, observe or hide latent variables,
Set observation function slots of the model, flush the cache, optionally
read the prior model from a file, model the dataset, and optionally freeze the
posterior model to a file."
  (let* ((writer (find-symbol (string-upcase (or writer "full-writer")) 'gm))
	 (writer (make-instance writer :graph model :destination t)))
    (apply #'f:hide (gm::latent model))
    (when observe-latent?
      (apply #'f:observe (gm::observable model)))
    (gm::flush-cache model)
    (if (not (null read-from))
	(with-open-file (s read-from :direction :input)
	  (gm::from-printable (read s) model)))
    (dolist (sequence dataset)
      (gm::set-uid writer (car sequence))
      (gm:model-sequence model (cdr sequence)
			 :writers nil;(list writer)
			 :predict? predict?
			 :construct? construct?))
    (when (not (null freeze-to))
      (with-open-file (s freeze-to :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
	(print (gm::printable model) s)))))

(defun obtain-dataset (dataset-id database attributes)
  "If DATASET-ID is specified, obtain database from the database,
otherwise read CSV data from *STANDARD-INPUT*.
Return (VALUES ATTRIBUTES DATASET)."
  (if (null dataset-id)
      (multiple-value-bind (columns rows)
	  (with-open-file (s "/home/bastiaan/data/simulations/last_used_training.csv")
	    (tabular->sequences (read-csv-stream s)))
	(assert (null (set-difference attributes columns :test #'equal))
		nil "Not all provided attributes found in CSV header line. 
Attributes: ~A. Header: ~A." attributes columns)
	(values columns rows))
      (values ;; This option doesn work anymore since we now use vectors for events
       attributes
       (progn
	 (apply #'connect-to-db (unless (null database) (list database)))
	 (md:get-event-sequences (list dataset-id))))))

(defcommand krn2csv (&parameters path timebase &free attributes)
  (let* ((timebase (parse-integer (or timebase md::*md-timebase*)))
	 (kern2db::*default-timebase* timebase)
	 (converted 
	  (mapcar #'kern2db::convert-kern-file
		  (directory (concatenate 'string
					  (directory-namestring path)
					  "*" kern2db::*input-file-extension*))))
	 (plists (kern->plists converted))
	 (attributes (mapcar (lambda (str) (intern (string-upcase str) :keyword))
			     attributes)))
    (plists->csv plists attributes t)))

(defcommand model-dataset (predict construct observe-latent
				   &parameters freeze-to read-from log-level
				   attributes dataset-id database
				   &free rest)
  (multiple-value-bind (attributes dataset)
      (obtain-dataset dataset-id database attributes)
    (let* ((model-name (car rest))
	   ;; These are attributes encoded in events for which observation
	   ;; functions are defined.
	   (jackdaw:*log-level* (if (null log-level) 0 (read-from-string log-level)))
	   (model (make-instance (find-symbol (string-upcase model-name)
					      :generative-models))))
      (model-dataset-impl model dataset dataset
			:read-from read-from
			:freeze-to freeze-to
			:observe-latent? observe-latent
			:predict? predict
			:construct? construct))))

(defcommand idyom-phase-model (predict construct observe-latent 
				       &parameters read-from
				       dataset-id database writer
				       freeze-to log-level
				       meter-domain ioi-domain
				       &free rest)
  (multiple-value-bind (attributes dataset)
      (obtain-dataset dataset-id database '("bioi" "barlength" "pulses"))
  (let* ((jackdaw:*log-level* (if (null log-level) 3 (read-from-string log-level)))
	 (meter-domain (unless (null meter-domain) (read-from-string meter-domain)))
	 (obs-functions
	  (mapcar (lambda (attrib i)
		    (list
		     (find-symbol (format nil "OBSERVE-~A" (string-upcase attrib)) :keyword)
		     (lambda (e) (svref e i))))
		  attributes (loop for i below (length attributes) collect i)))
	 (model (apply #'make-instance
		       (cons 'gm::idyom-phase-model
			     (reduce #'append obs-functions)))))
      (unless (every (lambda (attr) (member attr attributes :test #'string-equal))
		     (if construct
			 (list "bioi" "barlength" "pulses")
			 (list "bioi")))
	(error "Dataset must contain BIOI, BARLENGTH and PULSES attributes."))
      (model-dataset-impl model dataset
			  :read-from read-from
			  :writer writer
			  :freeze-to freeze-to
			  :observe-latent? observe-latent
			  :predict? predict
			  :construct? construct))))

(defcommand downbeat-distance (predict construct observe-latent 
				       &parameters read-from
				       dataset-id database writer
				       meter-domain ioi-domain
				       freeze-to log-level &free rest)
  (multiple-value-bind (attributes dataset)
      (obtain-dataset dataset-id database '("ioi" "period" "meter"))
    (let* ((jackdaw:*log-level* (if (null log-level) 3 (read-from-string log-level)))
	   (meter-domain (read-from-string meter-domain))
	   (ioi-domain (read-from-string ioi-domain))
	   (obs-functions
	    (mapcar (lambda (attrib i)
		      (progn
			(warn "~A ~A" attrib i)
			(list
			 (find-symbol (format nil "OBSERVE-~A" (string-upcase attrib)) :keyword)
			 (lambda (e) (svref e i)))))
		    attributes (loop for i below (length attributes) collect i)))
	   (model (apply #'make-instance
			 (append (list 'gm::downbeat-distance
				       :meter-domain meter-domain :ioi-domain ioi-domain)
				 (reduce #'append obs-functions)))))
      (model-dataset-impl model dataset
			  :read-from read-from
			  :writer writer
			  :freeze-to freeze-to
			  :observe-latent? observe-latent
			  :predict? predict
			  :construct? construct))))

(defcommand dump-events (&parameters database dataset-id path timebase &free attributes)
  "Load a dataset from the database and write it to a a CSV file at PATH."
  (apply #'connect-to-db (unless (null database) (list database)))
  (let* ((timebase (parse-integer (or timebase md::*md-timebase*)))
	 (md::*md-timebase* timebase)
	 (dataset (md:get-event-sequences (list dataset-id))))
    (if (null path)
	(plists->csv dataset attributes t)
	(with-open-file (s path :direction :output :if-exists :supersede
			   :if-does-not-exist :create)
	  (plists->csv dataset attributes s)))))

(defcommand dump-dataset (&parameters database dataset-id path timebase &free attributes)
  "Load a dataset from the database and write it to a a CSV file at PATH."
  (apply #'connect-to-db (unless (null database) (list database)))
  (let* ((timebase (parse-integer (or timebase (format nil "~d" md::*md-timebase*))))
	 (md::*md-timebase* timebase)
	 (dataset (md:get-event-sequences (list dataset-id))))
    (if (null path)
	(write-dataset dataset attributes t)
	(with-open-file (s path :direction :output :if-exists :supersede
			   :if-does-not-exist :create)
	  (write-dataset dataset attributes s)))))

(defcommand show-database (&parameters database)
  (apply #'connect-to-db (unless (null database) (list database)))
  (idyom-db:describe-database)
  (format t "~%"))

(let* ((argv sb-ext:*posix-argv*)
       (args (cdr argv))
       (command (car args))
       (command-args (cdr args)))
  (unless (null command)
    (let ((command-symbol (intern (string-upcase command))))
      (if (member command-symbol commands)
	  (apply command-symbol command-args)
	  (error "Unknown command: ~a." command-symbol)))))
