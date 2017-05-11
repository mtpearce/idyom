;;;; ======================================================================
;;;; File:       music-objects.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2014-09-07 12:24:19 marcusp>
;;;; Time-stamp: <2017-05-11 20:38:34 peter>
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

;; the order must match *event-attributes*
(defvar *music-slots* '(onset dur deltast cpitch mpitch accidental 
			keysig mode barlength pulses phrase tempo dyn voice bioi 
			ornament comma articulation vertint12))

(defun music-symbol (x)
  (find-symbol (string-upcase (symbol-name x))
	       (find-package :music-data)))

(defvar *md-music-slots* (mapcar #'music-symbol *music-slots*))

(defvar *time-slots* '(onset dur deltast barlength bioi))
(defvar *md-time-slots* (mapcar #'music-symbol *time-slots*))


;;; Classes for music objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass music-object ()
  ((identifier :initarg :id :accessor get-identifier :type identifier)
   (description :initarg :description :accessor description)
   (timebase :initarg :timebase :accessor timebase
	     :documentation "Number of time units in a semibreve")
   (midc :initarg :midc :accessor midc
	 :documentation "MIDI note number corresponding to middle C")))

(defclass music-dataset (list-slot-sequence music-object) ())

(defclass music-temporal-object (music-object anchored-time-interval) ())

(defclass music-sequence (list-slot-sequence music-temporal-object)
  ;; a sequence of music objects ordered in time
  ((bars :initarg :bars :accessor bars :initform nil
	 :documentation "List of bar numbers present in the object")
   (bar-onsets :initarg :bar-onsets :accessor bar-onsets :initform nil
	       :documentation "List of bar onsets, synchronised with bars slot")))
(defclass music-composition (music-sequence) ())
;; a composition is an unconstrained sequence of music objects
(defclass melodic-sequence (music-sequence) ())
;; a sequence of non-overlapping notes
(defclass harmonic-sequence (music-sequence) ())
;; a sequence of harmonic slices
(defclass harmonic-sequence-bar (harmonic-sequence) ())
;; a bar of a harmonic sequence

(defclass key-signature ()
  ((keysig :initarg :keysig :accessor key-signature
	   :documentation "Number of sharps (+ve) or flats (-ve) in key signature")
   (mode :initarg :mode :accessor mode
	 :documentation "Mode: 0 = major, 9 = minor, etc.")))

(defclass time-signature ()
  ((barlength :initarg :barlength :accessor barlength
	      :documentation "Number of basic time units (ticks) in bar")
   (pulses :initarg :pulses :accessor pulses
	   :documentation "Number of basic pulses/tactus units in bar")))

(defclass tempo ()
  ((tempo :initarg :tempo :accessor tempo
	  :documentation "Tempo in beats per minute")))

(defclass music-environment (key-signature time-signature tempo) ())

(defclass music-phrase ()
  ((phrase :initarg :phrase :accessor phrase)))

(defclass music-element (music-temporal-object music-environment music-phrase) ())

(defclass music-slice (list-slot-sequence music-element) ())
;; set of music objects overlapping in time, ordered by voice

(defclass music-chord (music-element)
  ((h-cpitch :initarg :h-cpitch :accessor h-cpitch
	     :documentation "List of chromatic pitches present in chord")))

(defclass music-event (music-element)
  ((bioi
    :initarg :bioi :accessor bioi
    :documentation "Basic interonset interval between last note and its predecessor")
   (deltast
    :initarg :deltast :accessor deltast :documentation
    "Length of immediately preceding rest")
   (cpitch
    :initarg :cpitch :accessor chromatic-pitch
    :documentation "MIDI pitch number")
   (mpitch
    :initarg :mpitch :accessor morphetic-pitch
    :documentation "Meredith's morphetic pitch")
   (accidental
    :initarg :accidental :accessor accidental
    :documentation "Inflection of named note: 0 = natural, 1 = single sharp,
2 = double sharp, -1 = single flat , etc.")
   (dyn
    :initarg :dyn :accessor dynamics
    :documentation "ppppp = -11; pppp = -9; ppp = -7; pp = -5; p = -3;
mp = -1; mf = 1; f = 3; ff = 5; fff = 7; ffff = 9; fffff = 11")
   (ornament
    :initarg :ornament :accessor ornament
    :documentation "0 = no ornament; 1 = accacciatura; 2 = mordent; 3 = trill")
   (comma
    :initarg :comma :accessor comma
    :documentation "0 = no comma; 1 = comma (breath mark)")
   (articulation
    :initarg :articulation :accessor articulation
    :documentation "0 = no articulation mark; 1 = staccato;
2 = staccatissimo; 3 = sforzando; 4 = marcato")
   (vertint12 :initarg :vertint12 :accessor vertint12)
   (voice
    :initarg :voice :accessor voice
    :documentation "Voice number in a score (Voice 1 assumed to be the monody)")))

(defclass continuation-event (music-event) ())


;;; Identifiers 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass dataset-identifier ()
  ((dataset-id :initarg :dataset-index :accessor get-dataset-index
	       :type (integer 0 *))))

(defclass composition-identifier (dataset-identifier)
  ((composition-id :initarg :composition-index :accessor get-composition-index
		   :type (integer 0 *))))

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

(defmethod get-attribute ((ms music-slice) attribute)
  (if (string= (symbol-name attribute) "H-CPITCH")
      (mapcar #'chromatic-pitch (coerce ms 'list))
      (call-next-method)))

(defgeneric set-attribute (event attribute value))
(defmethod set-attribute ((e music-element) attribute value)
  "Sets the value for slot <attribute> in event object <e>."
  (setf (slot-value e (music-symbol attribute)) value))

(defmethod set-attribute ((ms music-slice) attribute value)
  (if (string= (symbol-name attribute) "H-CPITCH")
      (let ((h-cpitch value)
	    (onset (get-attribute ms 'onset))
	    (dur (get-attribute ms 'dur))
	    (template-event (copy-event (car (%list-slot-sequence-data ms)))))
	(assert (listp h-cpitch))
	(assert (every #'numberp h-cpitch))
	(assert (typep template-event 'music-event))
	(set-attribute template-event 'onset onset)
	(set-attribute template-event 'dur dur)
	(set-attribute template-event 'voice nil)
	(setf (%list-slot-sequence-data ms)
	      (loop
		 for pitch in h-cpitch
		 collect (let ((e (copy-event template-event)))
			   (set-attribute e 'cpitch pitch)
			   e))))
      (call-next-method)))

(defgeneric copy-event (music-event))
(defmethod copy-event ((e music-element))
  (utils:copy-instance e))
(defmethod copy-event ((ms music-slice))
  (let ((ms-copy (utils:copy-instance ms :check-atomic nil)))
    (setf (%list-slot-sequence-data ms-copy)
          (mapcar #'md:copy-event (coerce ms 'list)))
    ms-copy))
(defmethod copy-event ((chord music-chord))
  (let ((chord-copy (utils:copy-instance chord :check-atomic nil)))
    (setf (h-cpitch chord)
          (copy-list (h-cpitch chord)))
    chord-copy))

(defun count-compositions (dataset-id)
  "Gets the number of compositions in dataset indexed by DATASET-ID"
  ;;(length (get-dataset (lookup-dataset dataset-id))))
  (car (clsql:query (format nil "SELECT count(composition_id) FROM mtp_composition WHERE (dataset_id = ~A);" dataset-id) :flatp t)))

(defun get-description (dataset-id &optional composition-id)
  "Gets the description of a dataset (if just DATASET-ID is provided) or of
  a composition (if both DATASET-ID and COMPOSITION-ID are provided)"
  (if (null composition-id)
      ;; (description (get-dataset (lookup-dataset dataset-id)))
      ;; (description (get-composition (lookup-composition dataset-id composition-id)))))
      (car (clsql:query (format nil "SELECT description FROM mtp_dataset WHERE (dataset_id = ~A);" dataset-id) :flatp t))
      (car (clsql:query (format nil "SELECT description FROM mtp_composition WHERE (dataset_id = ~A AND composition_id = ~A);" dataset-id composition-id) :flatp t))))


;;; Getting music objects from the database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-music-objects (dataset-indices composition-indices
			  &key voices (texture :melody)
			    (polyphonic-expansion :full)
			    (harmonic-reduction :regular-harmonic-rhythm)
			    (slices-or-chords :slices))
  "Returns music objects from the database corresponding to
DATASET-INDICES, COMPOSITION-INDICES which may be single numeric IDs
or lists of IDs. COMPOSITION-INDICES is only considered if
DATASET-INDICES is a single ID. TEXTURE determines whether the returned
object is a melody (using the Skyline algorithm if necessary) or a
sequence of harmonic slices using full expansion (cf. Conklin,
2002). The voices specified by VOICES are used. If VOICES is nil,
the melody corresponding to the voice of the first event is
extracted or the harmony corresponding to all voices is used."
  (assert (member harmonic-reduction (list :none :regular-harmonic-rhythm)))
  (cond
    ((eq texture :melody)
     (if (numberp dataset-indices)
	 (cond ((null composition-indices)
		(get-event-sequences dataset-indices :voices voices))
	       ((numberp composition-indices)
		(get-event-sequence dataset-indices composition-indices
				    :voices voices))
	       (t 
		(mapcar #'(lambda (c) (get-event-sequence dataset-indices c
							  :voices voices))
			composition-indices)))
	 (get-event-sequences dataset-indices :voices voices)))
    ((eq texture :harmony)
     (mapcar
      #'add-event-identifiers
      (if (numberp dataset-indices)
	  (cond ((null composition-indices)
		 (get-harmonic-sequences dataset-indices
					 :voices voices
					 :expansion polyphonic-expansion
					 :reduction harmonic-reduction
					 :slices-or-chords slices-or-chords))
		((numberp composition-indices)
		 (get-harmonic-sequence dataset-indices composition-indices
					:voices voices
					:expansion polyphonic-expansion
					:reduction harmonic-reduction
					:slices-or-chords slices-or-chords))
		(t 
		 (mapcar
		  #'(lambda (c)
		      (get-harmonic-sequence dataset-indices c
					     :voices voices
					     :expansion polyphonic-expansion
					     :reduction harmonic-reduction
					     :slices-or-chords slices-or-chords))
		  composition-indices)))
	  (get-harmonic-sequences dataset-indices
				  :voices voices :expansion polyphonic-expansion
				  :reduction harmonic-reduction
				  :slices-or-chords slices-or-chords))))
    (t 
     (error "Unrecognised texture for the music object. Current options are :melody or :harmony."))))

;;; Getting melodic sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (let ((hs (cdr (assoc :harmonic-sequence
			(composition->harmony composition :voices voices))))
        (result nil)
        (previous-event nil))
    (sequence:dosequence (slice hs (nreverse result))
      (let ((top (elt (sort slice #'> :key #'md:chromatic-pitch) 0)))
        (unless (null previous-event)
          (when (not (= (bioi top) (- (onset top) (onset previous-event))))
            (md:set-attribute top 'bioi (- (onset top) (onset previous-event))))
          (when (before previous-event top)
            (md:set-attribute top 'deltast (- (onset top)
					      (onset (end-time previous-event))))))
        ;; (print (list top (chromatic-pitch top) (length slice)))
        (setf previous-event top)
        (push top result)))))


;;; Getting harmonic sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-harmonic-sequence (dataset-index composition-index
			      &key voices (expansion :full)
				(reduction :none) (slices-or-chords :slices))
  "Gets harmonic sequences from a given composition indexed
by DATASET-INDEX and COMPOSITION-INDEX. <expansion> determines
the expansion method. <reduction> describes how (if at all)
the harmonic sequence is reduced down to underlying structure,
and can take values :none, i.e. no reduction, or :regular-harmonic-rhythm,
i.e. reduce to the level of a regular harmonic rhythm."
  (composition->harmony (get-composition
			 (lookup-composition dataset-index
					     composition-index))
			:voices voices :expansion expansion :reduction reduction
			:slices-or-chords slices-or-chords))

(defun get-harmonic-sequences (dataset-ids &key voices (expansion :full)
					     (reduction :none) (slices-or-chords :slices))
  "Gets harmonic sequences for all compositions contained within
a set of datasets indexed by DATASET-IDS"
  (utils:message
   (format nil "Getting harmonic sequences for dataset(s) ~A." dataset-ids))
  (let ((compositions '()))
    (dolist (dataset-id dataset-ids (nreverse compositions))
      (let* ((d (get-dataset (lookup-dataset dataset-id)))
	     (pb (utils:initialise-progress-bar (length d))))
        (sequence:dosequence (c d)
          (push (composition->harmony c :voices voices :expansion expansion
				      :reduction reduction :slices-or-chords slices-or-chords)
		compositions)
	  (utils:update-progress-bar pb (length compositions)))))))

(defun composition->harmony (composition &key voices (expansion :full)
					   (reduction :none) (slices-or-chords :slices))
  "Extract a sequence of harmonic slices from a composition according
   to the <voice> argument, which either should be nil (extract all voices)
   or a list of integers identifying the voices to be extracted."
  (assert (member slices-or-chords (list :slices :chords)))
  (let* ((hs (make-instance 'harmonic-sequence
			    :onset 0
			    :duration (duration composition)
			    :midc (midc composition)
			    :id (copy-identifier (get-identifier composition))
			    :description (description composition)
			    :timebase (timebase composition)
			    :bars (bars composition)
			    :bar-onsets (bar-onsets composition)
			    :description (description composition)))
	 (sorted-composition (sort composition #'< :key #'md:onset))
	 (event-list (coerce sorted-composition 'list))
	 (event-list (if (null voices) 
			 event-list 
			 (remove-if #'(lambda (x)
					(not (member x voices)))
				    event-list :key #'md:voice)))
	 (onsets (remove-duplicates (mapcar #'onset event-list)))
	 (l (length onsets))
	 (slices nil))
    ;; Extract the slices
    (dotimes (i l)
      ;; For each onset
      (let* ((onset (nth i onsets))
	     (matching-events (get-matching-events event-list
						   onset :expansion expansion))
	     ;; change onset and, if necessary, shorten duration
	     ;; to avoid overlap with next onset
	     (matching-events
	      (if (member expansion '(:continuation :full))
		  (mapcar
		   #'(lambda (x) 
		       (let ((e (md:copy-event x)))
			 (md:set-attribute e 'onset onset)
			 (if (< i (1- l))
			     (md:set-attribute
			      e 'dur (min (duration x)
					  (- (nth (1+ i) onsets)
					     onset)))
			     (md:set-attribute
			      e 'dur (apply #'max
					    (mapcar #'duration
						    matching-events))))
			 e))
		   matching-events)
		  matching-events))
	     ;; sort events by voice
	     (matching-events (sort matching-events #'< :key #'voice))
	     ;; create a slice object containing those events
	     (slice (make-instance
		     'music-slice 
		     :onset onset 
		     :duration (apply #'max (mapcar #'duration matching-events))
		     :tempo (tempo (car matching-events))
		     :barlength (barlength (car matching-events))
		     :pulses (pulses (car matching-events))
		     :keysig (key-signature (car matching-events))
		     :mode (mode (car matching-events))
		     :midc (midc composition)
		     :id (copy-identifier (get-identifier composition))
		     :description (description composition)
		     :timebase (timebase composition))))
	(sequence:adjust-sequence  slice (length matching-events)
				   :initial-contents
				   (sort matching-events #'< :key #'voice))
	(push slice slices)))
    (sequence:adjust-sequence hs (length slices)
			      :initial-contents (sort slices #'< :key #'onset))
    (let* ((hs (case reduction
		 (:none hs)
		 (:regular-harmonic-rhythm
		  (if (or (null (bars composition)) (null (bar-onsets composition)))
		      (error "Couldn't find barline information.")
		      (reduce-with-regular-harmonic-rhythm hs (bars composition)
							   (bar-onsets composition))))
		 (otherwise (error "Invalid case.")))))
      (if (eql slices-or-chords :chords)
	  (setf (%list-slot-sequence-data hs)
		(mapcar #'slice->chord (%list-slot-sequence-data hs))))
      hs)))


(defun get-matching-events (event-list onset &key (expansion :full))
  "Gets matching events from <event-list> at time <onset>
   using <expansion> method."
  (case expansion
    (:full            ; Full expansion (see e.g. Conklin, 2002)
     ;; find the events that are sounding at that onset
     (remove-if-not #'(lambda (x) 
			(and (<= (onset x) onset) 
			     (> (onset (end-time x)) onset)))
		    event-list))
    (:continuation    ; Continuation expansion (citation?)
     (let* ((begin-events (remove-if-not #'(lambda (x)
					     (and (= (onset x) onset)
						  (> (onset (end-time x)) onset)))
					 event-list))
	    ;; find the events that continue into that onset
	    (continuation-events (remove-if-not #'(lambda (x)
						    (and (< (onset x) onset)
							 (> (onset (end-time x)) onset)))
						event-list))
	    (continuation-events (mapcar #'(lambda (e)
					     (change-class e 'continuation-event))
					 continuation-events)))
       (append begin-events continuation-events)))
    (:natural
     (remove-if-not #'(lambda (x) (eql (onset x) onset)) event-list))
    (otherwise (error "<expansion> currently can only be :full or :continuation."))))

(defgeneric add-event-identifiers (seq)
  (:documentation "Iterates over a musical sequence <seq> and adds
event identifiers to every event, starting at 0."))

(defmethod add-event-identifiers ((seq harmonic-sequence))
  (let ((composition-id (get-composition-index (get-identifier seq)))
	(dataset-id (get-dataset-index (get-identifier seq)))
	(event-id 0))
    (sequence:dosequence (e seq seq)
      (setf (get-identifier e)
	    (make-event-id dataset-id composition-id event-id))
      (incf event-id))))


;;; Reducing harmonic sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reduce-with-regular-harmonic-rhythm
    (harmonic-sequence bars bar-onsets)
  (let ((slices (coerce harmonic-sequence 'list)))
    (assert (listp slices))
    (assert (listp bars))
    (assert (listp bar-onsets))
    (assert (not (or (null bars) (null bar-onsets))))
    (assert (eql (length bars) (length bar-onsets)))
    (assert (every #'(lambda (s) (typep s 'music-slice)) slices))
    (assert (every #'(lambda (x) (numberp x)) bars))
    (assert (every #'(lambda (x) (numberp x)) bar-onsets))
    (let* ((slices-by-bar (slices->bars slices bars bar-onsets))
	   (new-slices (mapcan #'reduce-bar slices-by-bar))
	   (new-sequence (utils:copy-instance harmonic-sequence
					      :check-atomic nil)))
      (setf (%list-slot-sequence-data new-sequence) new-slices)
      new-sequence)))

(defun slice->chord (slice)
  "Destructively converts a music-slice object <slice> to a music-chord object."
  (let* ((events (%list-slot-sequence-data slice))
	 (h-cpitch (remove-duplicates (mapcar #'(lambda (x)
						  (get-attribute x 'cpitch))
					      events))))
    (change-class slice 'music-chord :h-cpitch h-cpitch)))

(defun reduce-bar (bar &key (mode :canonic))
  "Takes a <bar> object as input, which is an assoc list containing
various elements including :slices, a list of slices, and :onset, the 
onset of the bar. Reduces the bar to a list of slices corresponding
to the harmonic rhythm."
  (let* ((slices (cdr (assoc :slices bar)))
	 (barlengths (mapcar #'barlength slices))
	 (pulses (mapcar #'pulses slices))
	 (timebases (mapcar #'timebase slices)))
    (assert (utils:all-eql barlengths))
    (assert (utils:all-eql pulses))
    (assert (utils:all-eql timebases))
    (let ((barlength (car barlengths)) (pulses (car pulses))
	  (timebase (car timebases)))
      (assert (not (null barlength)))
      (assert (not (null pulses)))
      (assert (not (null timebase)))
      (case mode
	(:canonic (reduce-bar-given-harmonic-rhythm
		   bar (find-canonic-harmonic-rhythm pulses barlength)))
	;; (:find-best-regular-rhythm
	;;  (let ((candidate-rhythms (find-harmonic-rhythms pulses barlength))
	;;        (candidate-reductions
	;; 	(mapcar #'(lambda (rhythm) (reduce-bar-given-harmonic-rhythm
	;; 				    bar rhythm))
	;; 		candidate-rhythms))
	;;        (best-score (apply #'max ...
	))))

(defun reduce-bar-given-harmonic-rhythm (bar harmonic-rhythm)
  "Reduces a <bar> assoc-list to a set of reduced slices on the basis 
of its <harmonic-rhythm>, also an assoc-list."
  (let* ((segment-starts-relative (cdr (assoc :segment-starts-relative
					      harmonic-rhythm)))
	 (segment-ends-relative (cdr (assoc :segment-ends-relative
					    harmonic-rhythm)))
	 (slices (cdr (assoc :slices bar)))
	 (bar-onset (cdr (assoc :onset bar)))
	 (segment-starts-abs (mapcar #'(lambda (x) (+ x bar-onset))
				     segment-starts-relative))
	 (segment-ends-abs (mapcar #'(lambda (x) (+ x bar-onset))
				   segment-ends-relative)))
    (utils:message (format nil "Number of slices in bar = ~A"
			   (length segment-starts-abs)) :detail 3)
    (utils:message (format nil "Segment starts (rel.) = ~A" segment-starts-relative)
		   :detail 3)
    (utils:message (format nil "Segment starts (abs.) = ~A" segment-starts-abs)
		   :detail 3)
    (let ((reduced-slices
	   (loop
	      for segment-start in segment-starts-abs
	      for segment-end in segment-ends-abs
	      collect
		(let* ((matching-slices (get-sounding-objects
					 slices segment-start segment-end))
		       (trimmed-slices (mapcar
					#'(lambda (s)
					    (trim s segment-start segment-end))
					matching-slices)))
		  (slices->slice trimmed-slices
				 segment-start segment-end)))))
      (remove nil reduced-slices))))


(defun slices->slice (slices onset offset &key (weight :num-segments))
  "Uses Pardo and Birmingham's (2002) chord labelling
algorithm to reduce <slices> to one <slice> with onset
<onset> and offset <offset>. If <slices> is empty, 
return nil. If <weight> is :duration, pitch classes
are weighted by their duration; if <weight> is :num-segments, pitch 
classes are weighted by the number of segments they occur in. Note that
to reproduce Pardo and Birmingham's original algorithm, the :num-segments
option should be selected for :weight."
  (assert (listp slices))
  (assert (every #'(lambda (s) (typep s 'music-slice)) slices))
  (assert (numberp onset)) (assert (numberp offset))
  (assert (> offset onset))
  (assert (member weight (list :duration :num-segments)))
  (if (null slices)
      nil
      (let* ((pc-weights (slices->pc-weights slices :weight weight))
	     (reduced-pcs (reduce-pc-weights pc-weights))
	     (bass-pc (slices+reduced-pcs->bass-pc slices reduced-pcs))
	     (non-bass-pcs (remove bass-pc reduced-pcs :test #'equalp))
	     (h-cpitch (cons (+ 48 bass-pc)
			     (mapcar #'(lambda (pc) (+ 60 pc))
				     non-bass-pcs)))
	     (dur (- offset onset))
	     (template-event (car (coerce (car slices) 'list)))
	     (template-slice (car slices))
	     (events (loop
			for cpitch in h-cpitch
			collect (let ((new-event (copy-event template-event)))
				  (set-attribute new-event 'cpitch cpitch)
				  (set-attribute new-event 'onset onset)
				  (set-attribute new-event 'dur dur)
				  (set-attribute new-event 'voice nil)
				  new-event)))	      
	     (slice (let ((new-slice (copy-event template-slice)))
		      (set-attribute new-slice 'onset onset)
		      (set-attribute new-slice 'dur dur)
		      (sequence:adjust-sequence new-slice
						(length events)
						:initial-contents events)
		      new-slice)))
	slice)))

(defun slices+reduced-pcs->bass-pc (slices reduced-pcs)
  "Given a list of pcs <reduced-pcs>, and a list of harmonic
slices <slices>, returns which pitch class is the best candidate
to be the bass note."
  (assert (listp slices))
  (assert (every #'(lambda (s) (typep s 'music-slice)) slices))
  (assert (listp reduced-pcs))
  (assert (every #'integerp reduced-pcs))
  (let* ((events (mapcan #'(lambda (slice) (coerce slice 'list)) slices))
	 (pitches (mapcar #'(lambda (e)
			      (get-attribute e 'cpitch))
			  events))
	 (min-pitch (apply #'min pitches))
	 (bass-pc (mod min-pitch 12)))
    bass-pc))

(defun slices->pc-weights (slices &key (weight :duration))
  "Gets pitch-class weights from <slices>, which should be 
a list of harmonic slices.  If <weight> is :duration, pitch classes
are weighted by their duration; if <weight> is :num-segments, pitch 
classes are weighted by the number of segments they occur in."
  (assert (listp slices))
  (assert (every #'(lambda (s) (typep s 'music-slice)) slices))
  (assert (member weight (list :duration :num-segments)))
  (let* ((events (mapcan #'(lambda (slice) (coerce slice 'list)) slices))
	 (pc-weights (make-array 12 :initial-element 0)))
    (dolist (event events pc-weights)
      (let* ((cpitch (get-attribute event 'cpitch))
	     (cpc (mod cpitch 12))
	     (round-cpc (round cpc))
	     (dur (get-attribute event 'dur)))
	(assert (equalp round-cpc cpc))
	(incf (svref pc-weights round-cpc)
	      (case weight
		(:duration dur)
		(:num-segments 1)
		(otherwise (error "Invalid <weight> argument."))))))))

(defun reduce-pc-weights (pc-weights)
  "Takes a vector of <pc-weights> and outputs an ordered list of 
pitch classes corresponding to a reduction of that chord, using
Pardo & Birmingham's (2002) algorithm."
  (assert (vectorp pc-weights))
  (assert (eql (array-dimension pc-weights 0) 12))
  ;; Get best scores
  (let ((best-score nil) (best-templates nil))
    (dolist (template *pb-all-chord-templates*)
      (let ((score (score-template pc-weights template)))
	(utils:message (format nil "S = ~A for template ~A" score template)
		       :detail 3)
	(if best-score
	    (cond ((< score best-score) nil)
		  ((= score best-score) (push template best-templates))
		  ((> score best-score) (setf best-score score
					      best-templates (list template))))
	    (progn
	      (setf best-score score)
	      (push template best-templates)))))
    (utils:message (format nil "Best templates: ~A" best-templates) :detail 3)
    (if (> (length best-templates) 1)
	(setf best-templates (tie-break-by-root-weight best-templates pc-weights)))
    (if (> (length best-templates) 1)
	(setf best-templates (tie-break-by-prior best-templates)))
    (if (> (length best-templates) 1)
	(setf best-templates (random-tie-break best-templates)))
    (let* ((best-template (car best-templates))
	   (output-pcs (cdr (assoc :pc best-template))))
      output-pcs)))

(defun tie-break-by-root-weight (best-templates pc-weights)
  (assert (every #'(lambda (x) (integerp (cdr (assoc :root x)))) best-templates))
  (assert (vectorp pc-weights))
  (assert (eql (array-dimension pc-weights 0) 12))
  (utils:message "\nTie-breaking by root weight." :detail 3)
  (utils:message (format nil "Candidate templates: ~A" best-templates)
		 :detail 3)
  (utils:message (format nil "PC weights: ~A" pc-weights) :detail 3)
  (let* ((root-weights (mapcar #'(lambda (template)
				   (svref pc-weights
					  (cdr (assoc :root template))))
			       best-templates))
	 (max-root-weight (apply #'max root-weights)))
    (utils:message (format nil "Root weights: ~A" root-weights) :detail 3)
    (utils:message (format nil "Max root weight = ~A" max-root-weight) :detail 3)
    (let ((remaining-candidates (loop
				   for template in best-templates
				   for root-weight in root-weights
				   if (= root-weight max-root-weight)
				   collect template)))
      (utils:message
       (format nil "Remaining candidates: ~A" remaining-candidates) :detail 3)
      remaining-candidates)))

(defun tie-break-by-prior (best-templates)
  (utils:message "\nTie-breaking by prior." :detail 3)
  (utils:message (format nil "Candidate templates: ~A" best-templates)
		 :detail 3)
  (let* ((priors (mapcar #'(lambda (template)
			     (cdr (assoc :prior template)))
			 best-templates))
	 (max-prior (apply #'max priors)))
    (loop
       for template in best-templates
       for prior in priors
       if (= prior max-prior)
       collect template)))

(defun random-tie-break (best-templates)
  (list (nth (mod (sxhash best-templates)
		  (length best-templates))
	     best-templates)))      

(defparameter *pb-basic-chord-templates*
  ;; These are pitch-class chord templates that assume a chord root of C.
  ;; :pc should be a list of pitch classes contained in the chord.
  ;; :name should be a symbol describing the chord.
  ;; :prior should be a prior weight for the chord, which is used
  ;; for tiebreaking between competing chord candidates (priors don't have to
  ;; sum to 1)
  (list (list (cons :pc '(0 4 7))    (cons :name :maj)  (cons :prior 0.436))
	(list (cons :pc '(0 4 7 10)) (cons :name :dom7) (cons :prior 0.219))
	(list (cons :pc '(0 3 7))    (cons :name :min)  (cons :prior 0.194))
	(list (cons :pc '(0 3 6 9))  (cons :name :dim7) (cons :prior 0.044))
	(list (cons :pc '(0 3 6 10)) (cons :name :hdim7)(cons :prior 0.037))
	(list (cons :pc '(0 3 6))    (cons :name :dim)  (cons :prior 0.018))
	;; The following chord templates are new additions.
	(list (cons :pc '(0 4 7 11)) (cons :name :maj7)    (cons :prior 0.2))
	(list (cons :pc '(0 3 7 10)) (cons :name :min7)    (cons :prior 0.2))
	(list (cons :pc '(0 4 8))    (cons :name :aug)     (cons :prior 0.02))
	(list (cons :pc '(0 7))      (cons :name :no3)     (cons :prior 0.05))
	(list (cons :pc '(0 7 10))   (cons :name :min7no3) (cons :prior 0.05))))
(if (not (every #'(lambda (ct)
		    (and (every #'(lambda (pc)
				    (and (integerp pc) (>= pc 0) (< pc 12)))
				ct)
			 (eql (length ct)
			      (length (remove-duplicates ct)))))
		(mapcar #'(lambda (x) (cdr (assoc :pc x)))
			*pb-basic-chord-templates*)))
    (error
     "PCs in *pb-basic-chord-templates* must be unique integers between 0 and 11."))

(defparameter *pb-all-chord-templates*
  ;; This comes from expanding *pb-basic-chord-templates* to all 12 chord roots.
  (mapcan
   #'identity 
   (loop
      for root from 0 to 11
      collect (loop for template in *pb-basic-chord-templates*
		 collect
		   (let* ((new-template (copy-list template))
			  (basic-pcs (cdr (assoc :pc new-template)))
			  (new-pcs (mapcar #'(lambda (pc) (mod (+ root pc) 12))
					   basic-pcs))
			  (sort-new-pcs (sort new-pcs #'<))
			  (new-template (remove :pc new-template :key #'car))
			  (new-template (acons :pc sort-new-pcs new-template))
			  (new-template (acons :root root new-template)))
		     new-template)))))

(defun score-template (pc-weights template)
  "Scores a set of <pc-weights> (which should be a vector
of length of 12) against a chord <template>, which should
be an assoc-list."
  (utils:message
   "Scoring pitch-class template according to Pardon & Birmingham, 2002"
   :detail 3)
  (utils:message (format nil "Pitch-class weights: ~A" pc-weights) :detail 3)
  (utils:message (format nil "Template: ~A" template) :detail 3)
  (assert (vectorp pc-weights))
  (assert (eql (array-dimension pc-weights 0) 12))
  (let ((template-root (cdr (assoc :root template)))
	(template-pc (cdr (assoc :pc template)))
	(template-prior (cdr (assoc :prior template))))
    (assert (not (null template-root))) (assert (not (null template-pc)))
    (assert (not (null template-prior))) (assert (integerp template-root))
    (assert (numberp template-prior)) (assert (listp template-pc))
    (let ((positive-evidence 0) (negative-evidence 0)
	  (misses (length template-pc)))
      (loop
	 for pc from 0 to 11
	 for pc-weight across pc-weights
	 do (if (and (> pc-weight 0) (member pc template-pc))
		(progn
		  (incf positive-evidence pc-weight)
		  (decf misses))
		(incf negative-evidence pc-weight)))
      (let ((score (- positive-evidence (+ misses negative-evidence))))
	(utils:message
	 (format nil "Final results: P = ~A, N = ~A, M = ~A, S = ~A"
		 positive-evidence negative-evidence misses score)
	 :detail 3)
	score))))

(defun find-canonic-harmonic-rhythm (pulses barlength &optional slices)
  "Finds the canonic harmonic rhythm for a bar given the <slices>
that make up that <bar>, and the attributes <pulses>
and <barlength> which are basic event attributes. <pulses>
corresponds to the denominator of the time signature;
if a compound time is detected, then pulses will not correspond
directly to the number of beats in the bar.
Returns two lists, the first corresponding to the starts of 
each harmonic segment in basic time units, the second 
corresponding to the ends of these harmonic segments."
  (declare (ignore slices))
  (let* ((num-pulses-in-bar (if (and (= (mod pulses 3) 0) t) ;;(> pulses 5)
				(/ pulses 3)
				pulses))
	 (pulse-length (/ barlength num-pulses-in-bar))
	 (segment-starts (loop for i from 0 to (1- num-pulses-in-bar)
			    collect (* i pulse-length)))
	 (segment-ends (loop for i from 1 to num-pulses-in-bar
			  collect (* i pulse-length))))
    (utils:message (format nil "Numerator = ~A" pulses) :detail 3)
    (utils:message (format nil "Number of pulses in bar = ~A" num-pulses-in-bar)
		   :detail 3)
    (list (cons :segment-starts-relative segment-starts)
	  (cons :segment-ends-relative segment-ends))))

(defun get-timesig (pulses barlength timebase)
  "Converts <pulses> and <barlength>, basic event attributes,
to a time signature."
  (let* ((numerator pulses)
	 (denominator (/ timebase (/ barlength numerator))))
    (values numerator denominator)))

(defun slices->bars (slices bars bar-onsets)
  "Takes as input a list of music slices, <slices>,
a list of bar numbers, <bars>, and a matching list 
giving the onsets of these bars, <bar-onsets>."
  (assert (listp slices))
  (assert (listp bars))
  (assert (listp bar-onsets))
  (assert (not (or (null bars) (null bar-onsets))))
  (assert (eql (length bars) (length bar-onsets)))
  (assert (every #'(lambda (s) (typep s 'music-slice)) slices))
  (assert (every #'(lambda (x) (numberp x)) bars))
  (assert (every #'(lambda (x) (numberp x)) bar-onsets))
  (utils:message "Converting slices into bars..." :detail 3)
  (let* ((bar-offsets (append (cdr bar-onsets)
			      (list nil)))
	 (new-bars (loop
		      for bar in bars
		      for bar-onset in bar-onsets
		      for bar-offset in bar-offsets
		      collect (let* ((matching-slices
				      (get-sounding-objects
				       slices bar-onset bar-offset))
				     (trimmed-slices
				      (mapcar #'(lambda (s)
						  (trim s bar-onset bar-offset))
					      matching-slices)))
				(list (cons :bar bar)
				      (cons :slices trimmed-slices)
				      (cons :onset bar-onset)
				      (cons :offset bar-offset))))))
    (utils:message (format nil "Number of bars = ~A" (length new-bars))
		   :detail 3)
    new-bars))

(defgeneric get-sounding-objects (sequence start end)
  (:documentation "Returns a list of all music objects within <sequence>
that are sounding at some point at or after (>=) <onset> and 
before (<) offset. One of <start> or <end> may be nil, in which 
case only the other boundary is used (e.g. if start is nil,
then the only filtering criterion is that the <event> should be 
sounding at some point before <end>."))

(defmethod get-sounding-objects
    ((events list) start end)
  (assert (or (numberp start) (null start)))
  (assert (or (numberp end) (null end)))
  (assert (or (numberp start) (numberp end)))
  (assert (if (and (numberp start) (numberp end))
	      (> (- end start) 0)
	      t))
  (remove-if-not
   #'(lambda (o)
       (let* ((onset (onset o))
	      (dur (duration o))
	      (offset (+ onset dur)))
	 (cond ((null start) (< onset end))
	       ((null end) (> offset start))
	       (t (and (> offset start)
		       (< onset end))))))
   events))

(defmethod get-sounding-objects
    ((sequence music-sequence) start end)
  (get-sounding-objects (coerce sequence 'list)
			start end))

(defgeneric trim (object onset offset)
  (:documentation "Non-destructively trims a music object
so that its onset falls at <onset> or later and its offset falls
at <offset> or earlier. One of <onset> or <offset> may be nil,
in which case the corresponding trim for that end of the object
is ignored."))

(defmethod trim ((slice music-slice) onset offset)
  (assert (or (numberp onset) (null onset)))
  (assert (or (numberp offset) (null offset)))
  (assert (or (numberp onset) (numberp offset)))
  (assert (if (and (numberp onset) (numberp offset))
  	      (> (- offset onset) 0)
  	      t))
  (let* ((original-onset (onset slice))
	 (original-dur (duration slice))
	 (original-offset (+ original-onset
			     original-dur)))
    ;; Check that all events have onsets and offsets within
    ;; the boundaries of the slice
    (assert (every #'(lambda (e)
    		       (and (>= (onset e) original-onset)
			    (<= (+ (onset e) (duration e)) original-offset)))
    		   (coerce slice 'list)))
    ;; (utils:message (format nil "Slice original onset = ~A" original-onset))
    ;; (utils:message (format nil "Slice original offset = ~A" original-offset))
    ;; (utils:message (format nil "Event original onsets = ~A"
    ;; 			   (mapcar #'(lambda (e) (onset e))
    ;; 				   (coerce slice 'list))))
    ;; (utils:message (format nil "Event original durations = ~A"
    ;; 			   (mapcar #'(lambda (e) (duration e))
    ;; 				   (coerce slice 'list))))
    ;; (utils:message (format nil "Desired slice onset = ~A" onset))
    ;; (utils:message (format nil "Desired slice offset = ~A" offset))
    (let* ((new-onset (if onset (max original-onset onset) original-onset))
	   (new-offset (if offset (min original-offset offset) original-offset))
	   (new-dur (- new-offset new-onset))
	   (new-slice (copy-event slice)))
      (assert (> new-dur 0))
      ;; Set attributes for the overall slice
      (set-attribute new-slice 'onset new-onset)
      (set-attribute new-slice 'dur new-dur)
      ;; Set attributes for the component events
      (setf (%list-slot-sequence-data new-slice)
      	    (remove nil (mapcar #'(lambda (e) (trim e onset offset))
				(%list-slot-sequence-data new-slice))))
      new-slice)))

(defmethod trim ((event music-event) onset offset)
  ;; Check for cases where a trim would produce an event of zero
  ;; duration; we throw an error if so.
  (assert (or (numberp onset) (null onset)))
  (assert (or (numberp offset) (null offset)))
  (assert (or (numberp onset) (numberp offset)))
  (assert (if (and (numberp onset) (numberp offset))
	      (> (- offset onset) 0)
	      t))
  (let* ((original-onset (onset event))
	 (original-dur (duration event))
	 (original-offset (+ original-onset
			     original-dur)))
    (let* ((new-onset (if onset (max original-onset onset) original-onset))
	   (new-offset (if offset (min original-offset offset)
			   original-offset))
	   (new-dur (- new-offset new-onset))
	   (new-event (copy-event event)))
      (when (> new-dur 0)
	(setf (onset new-event) new-onset
	      (duration new-event) new-dur)
	new-event))))

;;; Low-level database access functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-dataset (dataset-identifier))
(defgeneric get-composition (composition-identifier))
(defgeneric get-event (event-identifier))

#.(clsql:locally-enable-sql-reader-syntax)
(defmethod get-dataset ((identifier dataset-identifier))
  (let* ((dataset-id (get-dataset-index identifier))
         (where-clause [= [dataset-id] dataset-id])
         (db-dataset (car (clsql:select [*] :from [mtp-dataset]
					:where where-clause)))
         (midc (fourth db-dataset))
         (db-compositions (clsql:select [composition-id][description][timebase]
                                        :from [mtp-composition] 
                                        :order-by '(([composition-id] :asc))
                                        :where where-clause))
	 (dataset (make-instance 'music-dataset
				 :id identifier
				 :description (second db-dataset) 
				 :timebase (third db-dataset) 
				 :midc midc))
         (compositions nil))
    (when db-dataset
      ;; for each db-composition 
      (dolist (dbc db-compositions)
        (let* ((composition-id (first dbc))
	       (comp-id (make-composition-id dataset-id composition-id))
	       (composition (get-composition comp-id)))
	  (push composition compositions))))
    (sequence:adjust-sequence dataset (length compositions)
			      :initial-contents (nreverse compositions))
    dataset))
#.(clsql:restore-sql-reader-syntax-state)

;; #.(clsql:locally-enable-sql-reader-syntax)
;; (defmethod get-dataset ((identifier dataset-identifier))
;;   (let* ((dataset-id (get-dataset-index identifier))
;;          (where-clause [= [dataset-id] dataset-id])
;;          (db-dataset (car (clsql:select [*] :from [mtp-dataset] :where where-clause)))
;;          (midc (fourth db-dataset))
;;          (db-compositions (clsql:select [composition-id][description][timebase]
;;                                         :from [mtp-composition] 
;;                                         :order-by '(([composition-id] :asc))
;;                                         :where where-clause))
;;          (db-events (apply #'clsql:select 
;;                            (append *event-attributes* 
;;                                    (list :from [mtp-event] 
;;                                          :order-by '(([composition-id] :asc)
;;                                                      ([event-id] :asc))
;;                                          :where where-clause))))
;; 	 (dataset (make-instance 'music-dataset
;; 				 :id identifier
;; 				 :description (second db-dataset) 
;; 				 :timebase (third db-dataset) 
;; 				 :midc midc))
;;          (compositions nil)
;;          (events nil))
;;     (when db-dataset
;;       ;; for each db-composition 
;;       (dolist (dbc db-compositions)
;;         (let ((composition-id (first dbc))
;;               (description (second dbc))
;;               (timebase (third dbc)))
;;           ;; for each db-event 
;;           (do* ((dbes db-events (cdr dbes))
;;                 (dbe (car dbes) (car dbes))
;;                 (cid (second dbe) (second dbe)))
;;                ((or (null dbes) (not (= cid composition-id)))
;;                 (setf db-events dbes))
;;             (when dbe
;;               (push (db-event->music-event dbe timebase midc) events)))
;;           (when events
;;             (let* ((interval (onset (end-time (car events))))
;;                    (comp-id (make-composition-id dataset-id composition-id))
;;                    (composition
;;                     (make-instance 'music-composition
;;                                    :id comp-id
;;                                    :description description
;;                                    :onset 0
;;                                    :duration interval
;;                                    :midc midc
;;                                    :timebase timebase)))
;;               (sequence:adjust-sequence composition (length events)
;;                                         :initial-contents (nreverse events))
;;               (setf events nil)
;;               (push composition compositions)))))
;;       (sequence:adjust-sequence dataset (length compositions)
;;                                 :initial-contents (nreverse compositions))
;;       dataset)))
;; #.(clsql:restore-sql-reader-syntax-state)

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
         (midc (car (clsql:select [midc] :from [mtp-dataset]
				  :where [= [dataset-id] dataset-id] :flatp t)))
         (db-events (apply #'clsql:select 
                           (append *event-attributes* 
                                   (list :from [mtp-event] 
                                         :order-by '(([event-id] :asc))
                                         :where where-clause))))
	 (bar-lines (clsql:select
		     [bar] [onset] :from [mtp-barline]
		     :where [and [= [dataset-id] dataset-id]
		     [= [composition-id] composition-id]]
		     :field-names nil :flatp t))
	 (bar-nums (mapcar #'first bar-lines))
	 (bar-onsets (mapcar #'second bar-lines)) 
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
                             :timebase timebase
			     :bars bar-nums
			     :bar-onsets bar-onsets)))
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
