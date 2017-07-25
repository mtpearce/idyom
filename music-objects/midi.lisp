;;;; ======================================================================
;;;; File:       midi.lisp
;;;; Author:     Peter Harrison <p.m.c.harrison@qmul.ac.uk>
;;;; Created:    <2017-07-25 15:31:57 peter>                        
;;;; Time-stamp: <2017-07-25 17:00:31 peter>                           
;;;; ======================================================================
;;;;
;;;; DESCRIPTION 
;;;;
;;;;   For exporting music objects to MIDI.
;;;;   This code is heavily based on db2midi.
;;;;
;;;; ======================================================================

(cl:in-package #:music-data)

;;;==================
;;;* User parameters *
;;;==================

;; This user parameter determines whether voices are remapped from the
;; IDyOM order (low voice number implies low register) to the standard
;; MIDI order (low channel implies high register).
(defvar *remap-voices* t)

(defvar *encode-timesig* t)   ; whether time signatures are to be encoded in MIDI
(defvar *encode-keysig* t)    ; whether key signatures are to be encoded in MIDI

;; Path to the user's Timidity executable
(defparameter *timidity-path* "/usr/local/Cellar/timidity/2.14.0/bin/timidity")

(defparameter *musescore-path* "/Applications/MuseScore 2.app/Contents/MacOS/mscore")
(defparameter *pdf-viewer-path* "/Applications/Preview.app/Contents/MacOS/Preview")

(defvar *timebase* 96)
(defvar *midc* 60) 
(defvar *tick-multiplier* 1)
(defvar *default-tempo* 100)
(defvar *default-velocity* 100)
(defvar *default-program* 0)

;;;==================
;;;* Global variables *
;;;==================

(defvar *voice->channel-map* nil)
(defvar *environment* nil)

(defgeneric export-midi (data dir &key filename)
  (:documentation "Exports <data> to directory <dir>. If only one output file
is to be created, the string <filename> may be specified, in which case
the output file will be named after <filename> and located in <dir>."))

(defmethod export-midi ((data list) dir &key filename)
  (assert (every #'(lambda (x) (typep x 'music-sequence)) data))
  (when (and filename (> (length data) 1))
    (error "Cannot specify the argument <filename> when more than one
composition is to be exported."))
  (mapcar #'(lambda (sequence) (export-midi sequence dir :filename nil))
	  data))

(defmethod export-midi ((data harmonic-sequence) dir &key filename)
  (let* ((*timebase* (timebase data))
	 (dir-path (utils:ensure-directory dir))
	 (filename (if filename
		       filename
		       (format nil "dataset=~A_composition=~A.mid"
			       (get-dataset-index (get-identifier data))
			       (get-composition-index (get-identifier data)))))
	 (file-path (merge-pathnames dir-path (pathname filename))))
    (ensure-directories-exist dir-path)
    (events->midi (flatten-events data) file-path
		  :encode-keysig *encode-keysig*
		  :encode-timesig *encode-timesig*)
    file-path))

(defgeneric flatten-events (events)
  (:documentation "Takes a sequence of (possibly) polyphonic events
and flattens them so that each event is monophonic. Returns a list
of monophonic events."))

(defmethod flatten-events ((events harmonic-sequence))
  (mapcan #'flatten-events (coerce events 'list)))

(defmethod flatten-events ((events music-chord))
  (loop for cpitch in (h-cpitch events)
     collect (make-instance 'music-event :cpitch cpitch
			    :voice 1 :timebase (timebase events)
			    :dyn 100
			    :onset (onset events) :duration (duration events)
			    :mode (mode events) :keysig (key-signature events)
			    :pulses (pulses events) :barlength (barlength events)
			    :tempo (tempo events))))

(defmethod flatten-events ((events music-slice))
  (coerce events 'list))

(defun events->midi (events file &key (format 1) (program *default-program*)
				   encode-timesig encode-keysig)
  "Converts a list of events to a MIDI representation."
  (when *remap-voices* (update-voice->channel-map events))
  (let* ((*environment* nil)
	 (voice (voice (car events)))
	 (channel (if (null voice) 1
		      (voice->channel voice)))
         (channel-msg (make-instance 'midi:program-change-message :time 0 
                                     :status (+ #xc0 channel) 
                                     :program program))
         (tempo (tempo (car events)))
         (tempo-msg (make-instance 'midi:tempo-message :time 0
                                   :status #xff
                                   :tempo (bpm->usecs 
                                           (if tempo tempo *default-tempo*))))
         (track (mapcan #'(lambda (event)
			    (event->midi event
					 :encode-timesig encode-timesig
					 :encode-keysig encode-keysig))
			events))
	 (track (sort track #'(lambda (x y) (< (midi:message-time x)
					       (midi:message-time y)))))
         (midifile (make-instance 'midi:midifile
                                  :format format
                                  :division (* (/ *timebase* 4) *tick-multiplier*)
                                  :tracks (list 
                                           (cons tempo-msg
                                                 (cons channel-msg track))))))
    (midi:write-midi-file midifile file)
    midifile))

(defun event->midi (event &key encode-timesig encode-keysig)
  "Converts a CHARM event to a MIDI representation."
  (let* ((non-onset (round (onset event)))
         (noff-onset (round (+ non-onset (duration event))))
         (voice (voice event))
	 (channel (if (null voice) 1
		      (voice->channel voice)))
         (keynum  (round (+ (- 60 *midc*)
                            (chromatic-pitch event))))
         (velocity (dynamics event))
	 (midi-messages
	  (list (make-instance 'midi:note-on-message
			       :time (round (* non-onset *tick-multiplier*))
			       :status (+ #x90 channel)
			       :key keynum
			       :velocity (if velocity velocity
					     *default-velocity*))
		(make-instance 'midi:note-off-message
			       :time (round (* noff-onset *tick-multiplier*))
			       :status (+ #x80 channel)
			       :key keynum
			       :velocity (if velocity velocity
					     *default-velocity*)))))
    (if encode-keysig
	(let ((prev-keysig (second (assoc :keysig *environment*)))
	      (cur-keysig (key-signature event))
	      (prev-mode (second (assoc :mode *environment*)))
	      (cur-mode (mode event))
	      (first-keysig-reached (second (assoc :first-keysig-reached
						   *environment*))))
	  (when (not (null cur-keysig))
	    (if (not (integerp cur-keysig))
		(error (format nil "keysig must be an integer, but found ~A."
			       cur-keysig)))
	    (if (not (and (<= -7 cur-keysig) (<= cur-keysig 7)))
		(error (format nil "keysig must be between -7 and 7, but found ~A."
			       cur-keysig)))
	    (if (not (member cur-mode '(0 1) :test #'equal))
		(setf cur-mode 0))
	    (if (not (and (equal prev-keysig cur-keysig)
			  (equal prev-mode cur-mode)))
		(let ((keysig-msg
		       (make-instance 'midi:key-signature-message
				      :time (if first-keysig-reached
						(* non-onset *tick-multiplier*)
						0)
				      :status #xFF)))
		  (setf (slot-value keysig-msg 'midi::mi) cur-mode)
		  (setf (slot-value keysig-msg 'midi::sf) cur-keysig)
		  (push keysig-msg midi-messages)
		  (setf *environment* (utils:update-alist
				       *environment*
				       (list :keysig cur-keysig)
				       (list :mode cur-mode)
				       (list :first-keysig-reached t))))))))
    (if encode-timesig
    	(let* ((prev-pulses (second (assoc :pulses *environment*)))
    	       (cur-pulses (pulses event))
    	       (prev-barlength (second (assoc :barlength *environment*)))
    	       (cur-barlength (barlength event))
    	       (numerator (round cur-pulses))
    	       (pulse-dur (round (/ cur-barlength cur-pulses)))
    	       (pulse-whole-notes (/ pulse-dur *timebase*))
    	       (denominator (/ 1 pulse-whole-notes))
    	       (multiplier (denominator denominator))
    	       (numerator (* numerator multiplier))
    	       (denominator (* denominator multiplier))
	       (dd (round (log denominator 2))))
    	  (if (and (not (or (null cur-pulses) (null cur-barlength)))
    		     (or (not (equal cur-pulses prev-pulses))
    			 (not (equal cur-barlength prev-barlength))))
	      (let ((timesig-msg
		       (make-instance 'midi:time-signature-message
				      :time (* non-onset *tick-multiplier*)
				      :status #xFF)))
		  (setf (slot-value timesig-msg 'midi::nn) numerator)
		  (setf (slot-value timesig-msg 'midi::dd) dd)
	;;	  (setf (slot-value timesig-msg 'midi::nn) 3)
	;;	  (setf (slot-value timesig-msg 'midi::dd) 2)
		  (setf (slot-value timesig-msg 'midi::cc) (* pulse-whole-notes 96))
		  (setf (slot-value timesig-msg 'midi::bb) 8)
		  (push timesig-msg midi-messages)
		  (setf *environment* (utils:update-alist
				       *environment*
				       (list :pulses cur-pulses)
				       (list :barlength cur-barlength)))))))
    midi-messages))

(defun bpm->usecs (bpm)
  (floor (* 1000000 (/ 60 bpm))))

(defun update-voice->channel-map (events)
  "Takes a list of events and updates the global voice->channel map.
   The idea is that low voices should be mapped to high channels,
   addressing the fact that in IDyOM low voices are typically 
   low-register, whereas in MIDI low channels are typically high-register."
  (let ((voice-list nil))
    (dolist (event events voice-list)
      (pushnew (voice event) voice-list))
    (let ((descending-voice-list (sort voice-list #'>))
	  (current-channel 0))
      (setf *voice->channel-map* nil)
      (dolist (voice descending-voice-list *voice->channel-map*)
	(push (cons voice (incf current-channel)) *voice->channel-map*)))))

(defun voice->channel (voice)
  "Maps a voice to a channel."
  (if *voice->channel-map*
      (let ((channel (cdr (assoc voice *voice->channel-map* :test #'equal))))
	(if (null channel) (error "Something went wrong in *voice->channel-map*."))
	channel)
      voice))

(defgeneric preview (c &key temp-dir play-audio display-score))

(defmethod preview ((c music-sequence) &key (temp-dir "/tmp/idyom/")
					 (play-audio t) (display-score t))
  (if (or play-audio display-score)
      (let* ((dir-path (ensure-directories-exist (utils:ensure-directory temp-dir)))
	     (midi-file-path (export-midi c dir-path :filename "temp-audio.mid"))
	     (midi-file-path-string (namestring midi-file-path))
	     (pdf-file-path (merge-pathnames dir-path
					     (concatenate 'string "temp-score-"
							  (write-to-string
							   (get-internal-real-time))
							  ".pdf")))
	     (audio-process nil))
	(if display-score
	    (midi->pdf midi-file-path pdf-file-path :open-viewer t))
	(if play-audio
	    (setf audio-process (sb-ext:run-program *timidity-path*
						    (list midi-file-path-string)
						    :wait nil)))
	(utils:message
	 (format nil "Press enter to continue, or Q then enter to quit.~%")
	 :detail 1)
	(let ((char (read-char)))
	  (if (and (not (null audio-process))
		   (eql (sb-ext:process-status audio-process) :running))
	      (sb-ext:process-kill audio-process 15))
	  (if (eql char #\q) t nil)))))

(defun midi->pdf (input-file output-file &key open-viewer)
  (sb-ext:run-program *musescore-path*
		      (list (namestring input-file)
			    "-o" (namestring (ensure-directories-exist
					      output-file))))
  (if open-viewer
      (asdf:run-shell-command (concatenate 'string "open "
					   (namestring output-file))))
  (namestring output-file))
