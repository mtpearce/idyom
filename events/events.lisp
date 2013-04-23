;;;; ======================================================================
;;;; File:       events.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@eecs.qmul.ac.uk>
;;;; Created:    <2013-04-12 12:46:19 jeremy>
;;;; Time-stamp: <2013-04-23 09:32:41 jeremy>
;;;; ======================================================================

(cl:in-package #:music-data)

;; Identifiers refer to a particular dataset and its constiuent parts.
;; We assume a dataset contains multiple compositions (event
;; sequences), each made up of a list of events.
(defclass dataset-identifier () ())
(defclass composition-identifier (dataset-identifier) ())
(defclass event-identifier (composition-identifier) ())

;; Datasets, compositions and events are required to have an unique
;; integer index.
(defgeneric get-dataset-index (dataset-identifier)
  (:documentation "Integer index for dataset"))
(defgeneric get-composition-index (composition-identifier)
  (:documentation "Integer index for composition"))
(defgeneric get-event-index (event-identifier)
  (:documentation "Integer index for event"))

;; Copy identifier
(defgeneric copy-identifier (id))


(defgeneric lookup-dataset (dataset-index datasource)
  (:documentation "Returns the identifier for the dataset that has
  this index in the given datasource"))

(defgeneric lookup-composition (dataset-index composition-index datasource)
  (:documentation "Returns the identifier for the composition that has
  these indices in the given datasource"))

(defgeneric lookup-event (dataset-index composition-index event-index datasource)
  (:documentation "Returns the identifier for the event that has
  these indices in the given datasource"))

;; Accessing data
(defgeneric get-dataset (dataset-identifier))
(defgeneric get-composition (composition-identifier))
(defgeneric get-event (event-identifier))


(defmethod get-dataset ((index integer))
  (get-dataset (lookup-dataset index *idyom-datasource*)))

(defmethod get-composition ((index integer))
  (get-dataset (lookup-dataset index *idyom-datasource*)))

(defmethod get-event ((index integer))
  (get-dataset (lookup-dataset index *idyom-datasource*)))


(defun get-event-sequence (composition-index)
  (composition->monody (get-compsition composition-id)))

(defun get-event-sequences (&rest dataset-ids)
  (let ((compositions '()))
    (dolist (dataset-id dataset-ids (nreverse compositions))
      (let ((d (get-dataset dataset-id)))
        (sequence:dosequence (c d)
          (push (md:composition->monody c) compositions))))))

(defun count-compositions (dataset-id)
  (length (get-dataset dataset-id)))

(defun count-events (composition-id)
  (length (get-composition composition-id)))



;;; Events

(defclass music-event () 
  ((identifier :initarg :id :accessor ident :type event-identifier)
   (onset :initarg :onset :accessor onset) 
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

(defclass music-composition (list-slot-sequence) 
  ((identifier :initarg :id :accessor ident :type composition-identifier)
   (description :initarg :description :accessor description)
   (timebase :initarg :timebase :accessor composition-timebase)))

(defclass music-dataset (list-slot-sequence)
  ((identifier :initarg :id :accessor ident :type dataset-identifier)
   (description :initarg :description :accessor description)
   (timebase :initarg :timebase :accessor dataset-timebase)
   (midc :initarg :midc :accessor dataset-midc)))


;; From idyom/amuse/amuse-interface.lisp

(defun music-symbol (x)
  (find-symbol (string-upcase (symbol-name x))
	       (find-package :music-data)))


;(defgeneric get-attribute (event attribute))
(defmethod get-attribute ((e music-event) attribute)
  "Returns the value for slot <attribute> in event object <e>."
  (slot-value e (music-symbol attribute)))

;; (defgeneric set-attribute (event attribute value))
(defmethod set-attribute ((e music-event) attribute value)
  (setf (slot-value e (music-symbol attribute))
	value))

(defvar *time-slots* '(onset dur deltast barlength bioi))
(defvar *md-time-slots* (mapcar #'music-symbol *time-slots*))

; the order must match *event-attributes* in mtp-data
(defvar *music-slots* '(onset dur deltast cpitch mpitch accidental 
            keysig mode barlength pulses phrase tempo dyn voice bioi 
            ornament comma articulation))
(defvar *md-music-slots* (mapcar #'music-symbol *music-slots*))



(defgeneric get-alphabet (attribute dataset))


(defmethod copy-event ((e music-event))
  (make-instance 'music-event
                 :id (copy-identifier (ident e))
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




;; Moved from amuse/implementations/mtp

;; Monodies: use only first voice of composition.
(defun composition->monody (composition)
  ;; using the voice of the first event in the piece
  (let ((monody (make-instance 'music-composition 
                               :id (copy-identifier (ident composition))
                               :description (description composition)
                               :timebase (composition-timebase composition)))
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


;;; Constituents from compositions: time-signatures 

(defmethod crotchet ((dataset music-dataset))
  (/ (dataset-timebase dataset) 4))

(defmethod crotchet ((composition music-composition))
  (/ (composition-timebase composition) 4))

;(defmethod crotchet ((event music-event))
;  (/ (timebase event) 4))

(defmethod timebase ((event music-event))
  (timebase (identifier event)))

(defmethod timebase ((id composition-identifier))
  (composition-timebase (get-composition id)))

