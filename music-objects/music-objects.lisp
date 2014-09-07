;;;; ======================================================================
;;;; File:       music-objects.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2014-09-07 12:24:19 marcusp>
;;;; Time-stamp: <2014-09-07 13:06:27 marcusp>
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

(defclass music-temporal-object (music-object anchored-time-interval) ())

(defclass music-sequence (list-slot-sequence music-temporal-object) ()) ; sequence of music objects ordered in time 
(defclass music-slice (list-slot-sequence music-temporal-object) ())    ; set of music objects overlapping in time, ordered by voice

(defclass music-composition (music-sequence) ())               ; a composition is an unconstrained sequence of music objects
(defclass music-melodic-sequence (music-sequence) ())          ; a sequence of non-overlapping notes
(defclass music-harmonic-sequence (music-sequence) ())         ; a sequence of harmonic slices

(defclass music-event (music-temporal-object) 
  ((bioi :initarg :bioi :accessor bioi)
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
		 :duration (duration e)
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

(defun count-compositions (dataset-id)
  (length (get-dataset dataset-id)))


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

(defun get-music-objects (dataset-indices composition-indices &key voices (type :melody))
  "Return music objects from the database corresponding to
  DATASET-INDICES, COMPOSITION-INDICES which may be single numeric IDs
  or lists of IDs. COMPOSITION-INDICES is only considered if
  DATASET-INDICES is a single ID. TYPE determines whether the returned
  object is a melody (using the Skyline algorithm if necessary) or a
  sequence of harmonic slices using full expansion (cf. Conklin,
  2002). The voices specified by VOICES are used. If VOICES is nil,
  the melody corresponding to the voice of the first event is
  extracted or the harmony corresponding to all voices is used."
  (cond ((eq type :melody)
         (if (numberp dataset-indices)
             (cond ((null composition-indices)
                    (get-event-sequences dataset-indices :voices voices))
                   ((numberp composition-indices)
                    (get-event-sequence dataset-indices composition-indices :voices voices))
                   (t 
                    (mapcar #'(lambda (c) (get-event-sequence dataset-indices c :voices voices)) composition-indices)))
             (get-event-sequences dataset-indices :voices voices)))
        ((eq type :harmony)
         (if (numberp dataset-indices)
             (cond ((null composition-indices)
                    (get-harmonic-sequences dataset-indices :voices voices))
                   ((numberp composition-indices)
                    (get-harmonic-sequence dataset-indices composition-indices :voices voices))
                   (t 
                    (mapcar #'(lambda (c) (get-harmonic-sequence dataset-indices c :voices voices)) composition-indices)))
             (get-harmonic-sequences dataset-indices :voices voices)))
        (t 
         (print "Unrecognised type of music object. Options are :melody or :harmony."))))


;; harmonic sequences

(defun get-harmonic-sequence (dataset-index composition-index &key voices)
  (composition->harmony 
   (get-composition (lookup-composition dataset-index
                                        composition-index
                                        *idyom-datasource*))
   :voices voices))
                    
(defun get-harmonic-sequences (dataset-ids &key voices)
    (let ((compositions '()))
    (dolist (dataset-id dataset-ids (nreverse compositions))
      (let ((d (get-dataset dataset-id)))
        (sequence:dosequence (c d)
          (push (composition->harmony c :voices voices) compositions))))))

(defun composition->harmony (composition &key voices)
   "Extract a sequence of harmonic slices from a composition according
to the VOICE argument, which should be a list of integers. This uses
full expansion (cf. Conklin, 2002)."
   (let* ((hs (make-instance 'music-harmonic-sequence
                             :onset 0
                             :duration (duration composition)
                             :midc (midc composition)
                             :id (copy-identifier (ident composition))
                             :description (description composition)
                             :timebase (timebase composition)))
          (sorted-composition (sort composition #'< :key #'md:onset))
          (event-list (coerce sorted-composition 'list))
          (event-list (if (null voices) 
                          event-list 
                          (remove-if #'(lambda (x) (not (member x voices))) event-list :key #'md:voice)))
          (onsets (remove-duplicates (mapcar #'onset event-list)))
          (l (length onsets))
          (slices nil))
     ;; Extract the slices
     (dotimes (i l)
       ;; For each onset
       (let* ((onset (nth i onsets))
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
              ;; create a slice object containing those events
              (slice (make-instance 'music-slice 
                                    :onset onset 
                                    :duration (apply #'max (mapcar #'duration matching-events))
                                    :midc (midc composition)
                                    :id (copy-identifier (ident composition))
                                    :description (description composition)
                                    :timebase (timebase composition))))
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

(defun get-event-sequence (dataset-index composition-index &key voices)
  (composition->monody 
   (get-composition (lookup-composition dataset-index
					  composition-index
					  *idyom-datasource*))
   :voices voices))

(defun get-event-sequences (dataset-ids &key voices)
  (let ((compositions '()))
    (dolist (dataset-id dataset-ids (nreverse compositions))
      (let ((d (get-dataset dataset-id)))
        (sequence:dosequence (c d)
          (push (composition->monody c :voices voices) compositions))))))

(defun composition->monody (composition &key voices)
  "Extract a melody from a composition according to the VOICE
argument, which should be an integer. If VOICE is null the voice of
the first event in the piece is extracted."
  (let ((monody (make-instance 'music-composition ;; 'music-melodic-sequence
                               :onset 0
                               :duration (duration composition)
                               :midc (midc composition)
                               :id (copy-identifier (ident composition))
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
        (print (list top (chromatic-pitch top) (length slice)))
        (setf previous-event top)
        (push top result)))))


;; low-level database access functions

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
