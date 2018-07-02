;;;; ======================================================================
;;;; File:       music-objects.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2014-09-07 12:24:19 marcusp>
;;;; Time-stamp: <2018-07-02 15:42:17 marcusp>
;;;; ======================================================================

(cl:in-package #:music-data)

#.(clsql:locally-enable-sql-reader-syntax)
(defvar *event-attributes* 
  (list [dataset-id] [composition-id] [event-id]
        [onset] [dur] [deltast] [cpitch] 
	[mpitch] [accidental] [keysig] [mode]
        [barlength] [pulses] [phrase] [tempo] [dyn] [voice] [bioi] 
        [ornament] [comma] [articulation][vertint12]))
#.(clsql:restore-sql-reader-syntax-state)

; the order must match *event-attributes*
(defvar *music-slots* '(onset dur deltast cpitch mpitch accidental 
            keysig mode barlength pulses phrase tempo dyn voice bioi 
            ornament comma articulation vertint12))

(defun music-symbol (x)
  (find-symbol (string-upcase (symbol-name x))
	       (find-package :music-data)))

(defvar *md-music-slots* (mapcar #'music-symbol *music-slots*))

(defvar *time-slots* '(onset dur deltast barlength bioi))
(defvar *md-time-slots* (mapcar #'music-symbol *time-slots*))

(defparameter *md-timebase* 96 "Target timebase for music objects")


;;; Classes for music objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass music-object ()
  ((identifier :initarg :id :accessor get-identifier :type identifier)
   (description :initarg :description :accessor description)
   (timebase :initarg :timebase :accessor timebase)
   (midc :initarg :midc :accessor midc)))

(defclass music-dataset (list-slot-sequence music-object) ())

(defclass music-temporal-object (music-object anchored-time-interval) ())

(defclass music-sequence (list-slot-sequence music-temporal-object) ()) ; a sequence of music objects ordered in time 
(defclass music-composition (music-sequence) ())                        ; a composition is an unconstrained sequence of music objects
(defclass melodic-sequence (music-sequence) ())                         ; a sequence of non-overlapping notes
(defclass harmonic-sequence (music-sequence) ())                        ; a sequence of harmonic slices

(defclass key-signature ()
  ((keysig :initarg :keysig :accessor key-signature)
   (mode :initarg :mode :accessor mode)))

(defclass time-signature ()
  ((barlength :initarg :barlength :accessor barlength)
   (pulses :initarg :pulses :accessor pulses)))

(defclass tempo ()
  ((tempo :initarg :tempo :accessor tempo)))
  
(defclass music-environment (key-signature time-signature tempo) ())

(defclass music-phrase ()
  ((phrase :initarg :phrase :accessor phrase)))

(defclass music-temporal-event (music-temporal-object)
  ((bioi :initarg :bioi :accessor bioi)
   (deltast :initarg :deltast :accessor deltast)))

(defclass music-element (music-temporal-event music-environment music-phrase) ())

(defclass music-slice (list-slot-sequence music-element) ())  ; set of music objects overlapping in time, ordered by voice

(defclass music-event (music-element)
  ((cpitch :initarg :cpitch :accessor chromatic-pitch)
   (mpitch :initarg :mpitch :accessor morphetic-pitch)
   (accidental :initarg :accidental :accessor accidental)
   (dyn :initarg :dyn :accessor dynamics)
   (ornament :initarg :ornament :accessor ornament)
   (comma :initarg :comma :accessor comma)
   (articulation :initarg :articulation :accessor articulation)
   (vertint12 :initarg :vertint12 :accessor vertint12)
   (voice :initarg :voice :accessor voice)))
  

;;; Identifiers 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass dataset-identifier ()
  ((dataset-id :initarg :dataset-index :accessor get-dataset-index :type (integer 0 *))))
   
(defclass composition-identifier (dataset-identifier)
  ((composition-id :initarg :composition-index :accessor get-composition-index :type (integer 0 *))))

(defclass event-identifier (composition-identifier)
  ((event-id :initarg :event-index :accessor get-event-index :type (integer 0 *))))

;; Make identifiers
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

;; Copy identifiers
(defgeneric copy-identifier (id))

(defmethod copy-identifier ((id dataset-identifier))
  (make-instance 'dataset-identifier
		 :dataset-index (get-dataset-index id)))

(defmethod copy-identifier ((id composition-identifier))
  (make-instance 'composition-identifier
		 :dataset-index (get-dataset-index id)
		 :composition-index (get-composition-index id)))

(defmethod copy-identifier ((id event-identifier))
  (make-instance 'event-identifier
		 :dataset-index (get-dataset-index id)
		 :composition-index (get-composition-index id)
		 :event-index (get-event-index id)))

;; Lookup identifiers (to facilitate extension to other data sources)
(defgeneric lookup-dataset (dataset-index)
  (:documentation "Returns the identifier for the dataset that has
  this index in the given datasource"))

(defgeneric lookup-composition (dataset-index composition-index)
  (:documentation "Returns the identifier for the composition that has
  these indices in the given datasource"))

(defgeneric lookup-event (dataset-index composition-index event-index)
  (:documentation "Returns the identifier for the event that has
  these indices in the given datasource"))

(defmethod lookup-dataset (dataset-index)
  (make-dataset-id dataset-index))

(defmethod lookup-composition (dataset-index composition-index)
  (make-composition-id dataset-index composition-index))

(defmethod lookup-event (dataset-index composition-index event-index)
  (make-event-id dataset-index composition-index event-index))


;;; Accessing properties of music objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-attribute (event attribute))
(defmethod get-attribute ((e music-element) attribute)
  "Returns the value for slot <attribute> in event object <e>."
  (slot-value e (music-symbol attribute)))

(defgeneric set-attribute (event attribute value))
(defmethod set-attribute ((e music-element) attribute value)
  (setf (slot-value e (music-symbol attribute)) value))

(defmethod set-attribute ((ms music-slice) attribute value)
  (if (string= (symbol-name attribute) "H-CPITCH")
      (let ((i 0))
        (sequence:dosequence (e ms)
          (set-attribute e 'cpitch (nth i value))
          (incf i)))
      (call-next-method)))

(defgeneric copy-event (music-event))
(defmethod copy-event ((e music-element))
  (utils:copy-instance e))
(defmethod copy-event ((ms music-slice))
  (let ((ms-copy (utils:copy-instance ms)))
    (setf (%list-slot-sequence-data ms-copy)
          (mapcar #'md:copy-event (coerce ms 'list)))
    ms-copy))

(defun count-compositions (dataset-id)
  ;;(length (get-dataset (lookup-dataset dataset-id))))
  (car (clsql:query (format nil "SELECT count(composition_id) FROM mtp_composition WHERE (dataset_id = ~A);" dataset-id) :flatp t)))

(defun get-description (dataset-id &optional composition-id)
  (if (null composition-id)
      ;; (description (get-dataset (lookup-dataset dataset-id)))
      ;; (description (get-composition (lookup-composition dataset-id composition-id)))))
      (car (clsql:query (format nil "SELECT description FROM mtp_dataset WHERE (dataset_id = ~A);" dataset-id) :flatp t))
      (car (clsql:query (format nil "SELECT description FROM mtp_composition WHERE (dataset_id = ~A AND composition_id = ~A);" dataset-id composition-id) :flatp t))))


;;; Getting music objects from the database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-music-objects (dataset-indices composition-indices &key voices (texture :melody))
  "Return music objects from the database corresponding to
  DATASET-INDICES, COMPOSITION-INDICES which may be single numeric IDs
  or lists of IDs. COMPOSITION-INDICES is only considered if
  DATASET-INDICES is a single ID. TEXTURE determines whether the returned
  object is a melody (using the Skyline algorithm if necessary) or a
  sequence of harmonic slices using full expansion (cf. Conklin,
  2002). The voices specified by VOICES are used. If VOICES is nil,
  the melody corresponding to the voice of the first event is
  extracted or the harmony corresponding to all voices is used."
  (let ((result nil))
    (cond ((eq texture :melody)
           (setf result (get-melodic-objects dataset-indices composition-indices :voices voices)))
         ((eq texture :harmony)
          (setf result (get-harmonic-objects dataset-indices composition-indices :voices voices)))
         (t 
          (print "Unrecognised texture for the music object. Current options are :melody or :harmony.")))
    result))

;; harmonic sequences

(defun get-harmonic-objects (dataset-indices composition-indices &key voices)
  (if (numberp dataset-indices)
      (cond ((null composition-indices)
             (get-harmonic-sequences (list dataset-indices) :voices voices))
            ((numberp composition-indices)
             (get-harmonic-sequence dataset-indices composition-indices :voices voices))
            (t 
             (mapcar #'(lambda (c) (get-harmonic-sequence dataset-indices c :voices voices)) composition-indices)))
      (get-harmonic-sequences dataset-indices :voices voices)))

(defun get-harmonic-sequence (dataset-index composition-index &key voices)
  (composition->harmony 
   (get-composition (lookup-composition dataset-index composition-index))
   :voices voices))
                    
(defun get-harmonic-sequences (dataset-ids &key voices)
    (let ((compositions '()))
    (dolist (dataset-id dataset-ids (nreverse compositions))
      (let ((d (get-dataset (lookup-dataset dataset-id))))
        (sequence:dosequence (c d)
          (push (composition->harmony c :voices voices) compositions))))))

(defun composition->harmony (composition &key voices)
   "Extract a sequence of harmonic slices from a composition according
to the VOICE argument, which should be a list of integers. This uses
full expansion (cf. Conklin, 2002)."
   (let* ((hs (make-instance 'harmonic-sequence
                             :onset 0
                             :duration (duration composition)
                             :midc (midc composition)
                             :id (copy-identifier (get-identifier composition))
                             :description (description composition)
                             :timebase (timebase composition)))
          (sorted-composition (sort composition #'< :key #'md:onset))
          (event-list (coerce sorted-composition 'list))
          (event-list (if (null voices) 
                          event-list 
                          (remove-if #'(lambda (x) (not (member x voices))) event-list :key #'md:voice)))
          (onsets (remove-duplicates (mapcar #'onset event-list)))
          (l (length onsets))
          (previous-onset nil)
          (previous-dur nil)
          (slices nil))
     ;; Extract the slices
     (dotimes (i l)
       ;; For each onset
       (let* ((onset (nth i onsets))
              (bioi (if previous-onset (- onset previous-onset) 0))
              (deltast (if previous-dur (- onset (+ previous-onset previous-dur)) 0))
              ;; find the events that are sounding at that onset
              (matching-events (remove-if-not #'(lambda (x) 
                                                  (and (<= (onset x) onset) 
                                                       (> (onset (end-time x)) onset)))
                                              event-list))
              ;; change onset and, if necessary, shorten duration to avoid overlap with next onset
              (matching-events (mapcar #'(lambda (x) 
                                           (let ((e (md:copy-event x)))
                                             (md:set-attribute e 'onset onset)
                                             (if (< i (1- l))
                                                 (md:set-attribute e 'dur (min (duration x) (- (nth (1+ i) onsets) onset)))
                                                 (md:set-attribute e 'dur (apply #'max (mapcar #'duration matching-events))))
                                             e))
                                       matching-events))
              ;; sort them by voice
              (matching-events (sort matching-events #'< :key #'voice))
              (dur (apply #'max (mapcar #'duration matching-events)))
              ;; create a slice object containing those events
              (slice (make-instance 'music-slice 
                                    :onset onset
                                    :bioi bioi
                                    :deltast deltast
                                    :duration dur
                                    :tempo (tempo (car matching-events))
                                    :barlength (barlength (car matching-events))
                                    :pulses (pulses (car matching-events))
                                    :keysig (key-signature (car matching-events))
                                    :mode (mode (car matching-events))
                                    :midc (midc composition)
                                    :id (copy-identifier (get-identifier composition))
                                    :description (description composition)
                                    :timebase (timebase composition))))
         (setf previous-onset onset)
         (setf previous-dur dur)
         (sequence:adjust-sequence 
          slice (length matching-events)
          :initial-contents (sort matching-events #'< :key #'voice))
         (push slice slices)))
     ;; return the new harmonic sequence
     (sequence:adjust-sequence 
      hs (length slices)
      :initial-contents (sort slices #'< :key #'onset))
     hs))


;; melodic sequences

(defun get-melodic-objects (dataset-indices composition-indices &key voices)
  (if (numberp dataset-indices)
      (cond ((null composition-indices)
             (get-event-sequences (list dataset-indices) :voices voices))
            ((numberp composition-indices)
             (get-event-sequence dataset-indices composition-indices :voices voices))
            (t 
             (mapcar #'(lambda (c) (get-event-sequence dataset-indices c :voices voices)) composition-indices)))
      (get-event-sequences dataset-indices :voices voices)))

(defun get-event-sequence (dataset-index composition-index &key voices)
  (composition->monody 
   (get-composition (lookup-composition dataset-index composition-index))
   :voices voices))

(defun get-event-sequences (dataset-ids &key voices)
  (let ((compositions '()))
    (dolist (dataset-id dataset-ids (nreverse compositions))
      (let ((d (get-dataset (lookup-dataset dataset-id))))
        (sequence:dosequence (c d)
          (push (composition->monody c :voices voices) compositions))))))

(defun composition->monody (composition &key voices)
  "Extract a melody from a composition according to the VOICE
argument, which should be an integer. If VOICE is null the voice of
the first event in the piece is extracted."
  (let ((monody (make-instance 'melodic-sequence
                               :onset 0
                               :duration (duration composition)
                               :midc (midc composition)
                               :id (copy-identifier (get-identifier composition))
                               :description (description composition)
                               :timebase (timebase composition)))
        (events nil)
        (voice (if (listp voices) (car voices) voices)))
    (if (and (or (null voices) (integerp voices) (= (length voices) 1)) (ensure-monody composition :voices voices))
        ;; return the specified voice
        (sequence:dosequence (event composition)
          (when (null voices)
            (setf voice (voice event)))
          (when (= (voice event) voice)
            (push event events)))
        ;; else use skyline algorithm to extract monody
        (setf events (skyline composition :voices voices)))
    (sequence:adjust-sequence 
     monody (length events)
     :initial-contents (sort events #'< :key #'onset))
    monody))

(defun ensure-monody (composition &key voices) 
  (let* ((sorted-composition (sort composition #'< :key #'md:onset))
         (event-list (coerce sorted-composition 'list))
         (event-list (if (null voices) 
                         event-list 
                         (remove-if #'(lambda (x) (not (member x voices))) event-list :key #'md:voice)))
         (result t))
    (dotimes (i (1- (length event-list)) result)
      (let ((e1 (elt event-list i))
            (e2 (elt event-list (1+ i))))
        ;;(print (list i "e1" (onset e1) (onset (end-time e1)) "e2" (onset e2) (onset (end-time e2)) "diff" (- (onset e2) (onset e1)) (disjoint e1 e2)))
        (unless (disjoint e1 e2)
          (setf result nil))))))

(defun skyline (composition &key voices) 
  "For each event onset in a composition, retain only the voice with
the highest pitch sounding at that onset position."
  (let ((hs (composition->harmony composition :voices voices))
        (result nil)
        (previous-event nil))
    (sequence:dosequence (slice hs (nreverse result))
      (let ((top (elt (sort slice #'> :key #'md:chromatic-pitch) 0)))
        (unless (null previous-event)
          (when (not (= (bioi top) (- (onset top) (onset previous-event))))
            (md:set-attribute top 'bioi (- (onset top) (onset previous-event))))
          (when (before previous-event top)
            (md:set-attribute top 'deltast (- (onset top) (onset (end-time previous-event))))))
        ;; (print (list top (chromatic-pitch top) (length slice)))
        (setf previous-event top)
        (push top result)))))

;; low-level database access functions

(defgeneric get-dataset (dataset-identifier))
(defgeneric get-composition (composition-identifier))
(defgeneric get-event (event-identifier))

#.(clsql:locally-enable-sql-reader-syntax)
(defmethod get-dataset ((identifier dataset-identifier))
  (let* ((dataset-id (get-dataset-index identifier))
         (where-clause [= [dataset-id] dataset-id])
         (db-dataset (car (clsql:select [*] :from [mtp-dataset] :where where-clause)))
         (midc (fourth db-dataset))
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
				 :midc midc))
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
              (push (db-event->music-event dbe timebase midc) events)))
          (when events
            (let* ((interval (onset (end-time (car events))))
                   (comp-id (make-composition-id dataset-id composition-id))
                   (composition
                    (make-instance 'music-composition
                                   :id comp-id
                                   :description description
                                   :onset 0
                                   :duration interval
                                   :midc midc
                                   :timebase timebase)))
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
         (midc (car (clsql:select [midc] :from [mtp-dataset] :where [= [dataset-id] dataset-id] :flatp t)))
         (db-events (apply #'clsql:select 
                           (append *event-attributes* 
                                   (list :from [mtp-event] 
                                         :order-by '(([event-id] :asc))
                                         :where where-clause))))
         (events nil))
    (when (and db-events timebase)
      (dolist (e db-events)
        (push (db-event->music-event e timebase midc) events))
      (let* ((interval (onset (end-time (car events))))
             (composition 
              (make-instance 'music-composition
                             :id identifier
                             :onset 0
                             :duration interval
                             :description description
                             :midc midc
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
         (midc (car (clsql:select [midc] :from [mtp-dataset] :where [= [dataset-id] dataset-id] :flatp t)))
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
      (db-event->music-event db-event timebase midc))))
#.(clsql:restore-sql-reader-syntax-state) 

(defun db-event->music-event (db-event timebase midc)
  (let* ((event-id (make-event-id (first db-event)
				 (second db-event)
				 (third db-event)))
         (music-event (make-instance 'music-event
				   :id event-id
                                   :description ""
                                   :midc midc
                                   :timebase timebase)))
    (do* ((slts *md-music-slots* (cdr slts))
          (db-atts (nthcdr 3 db-event) (cdr db-atts)))
         ((null slts) music-event)
      (if (member (car slts) *md-time-slots* :test #'eql)
          (setf (slot-value music-event (car slts)) (convert-time-value (car db-atts) timebase))
          (setf (slot-value music-event (car slts)) (car db-atts))))))


;; converting time resolutions

(defun convert-time-value (time-value old-timebase &optional (new-timebase *md-timebase*) (fraction-warning nil))
  "Convert <time-value> from <old-timebase> to <new-timebase> where
    the latter defaults to the value of *MD-TIMEBASE*. For example,
    convert a time value from a native representation of time into a
    representation where a bar has a value of *md-timebase*."
  (if (or (null time-value) (null old-timebase))
      nil
      (let* ((multiplier (/ new-timebase old-timebase))
             (new-time-value (* time-value multiplier))
             (fractional (fractional? new-time-value)))
        (when (and fractional fraction-warning)
          (warn (format nil "WARNING: converting ~F from timebase ~D to timebase ~D resulted in a fractional value (~F) ~%"
                        time-value old-timebase new-timebase new-time-value)))
        new-time-value)))

(defun fractional? (n)
  (not (equalp (mod n 1) 0)))
