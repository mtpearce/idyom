;;;; ======================================================================
;;;; File:       db2midi.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2005-06-09 11:01:51 marcusp>
;;;; Time-stamp: <2017-02-16 14:50:32 peter>
;;;; ======================================================================

(cl:in-package #:db2midi)

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

;;;=======================
;;;* Functions and methods *
;;;=======================

(defmethod export-data ((d idyom-db:mtp-dataset) (type (eql :mid)) dir &key filename)
  (let* ((*timebase* (idyom-db::dataset-timebase d))
	 (*midc*     (idyom-db::dataset-midc d))
	 (dir (utils:ensure-directory dir))
	 (compositions (idyom-db::dataset-compositions d))
	 (num-compositions (length compositions))
	 (filename (if (null filename)
		       nil
		       (if (equal 1 (length compositions))
			   filename
			   (progn
			     (utils:message "Warning: ignored filename because multiple files were being exported."
					    :detail 1)
			     nil)))))
    (utils:message (format nil "Exporting dataset (~A composition(s)) to MIDI format."
			   num-compositions) :detail 1)
    (let ((output-path (utils:dolist-pb (c compositions)
			 (export-data c type dir :filename filename))))
      (if (equal 1 (length compositions))
	  output-path dir))))

(defmethod export-data ((c idyom-db:mtp-composition) (type (eql :mid)) dir &key filename)
  ;; FIXME: *midc* is never set if export-data is called with a
  ;; composition directly.
  (let* ((*timebase* (idyom-db::composition-timebase c))
	 (dir-path (utils:ensure-directory dir))
	 (filename (if filename
		       filename
		       (concatenate 'string (idyom-db::composition-description c)
				    ".mid")))
	 (file-path (merge-pathnames dir-path (pathname filename))))
    (ensure-directories-exist dir-path)
    (events->midi (idyom-db::composition-events c) file-path
		  :encode-keysig *encode-keysig*
		  :encode-timesig *encode-timesig*)
    file-path))

(defmethod preview ((d idyom-db:mtp-dataset) &key (temp-dir "/tmp/idyom/")
					       (play-audio t) (display-score t))
  (let* ((compositions (idyom-db::dataset-compositions d))
	 (num-compositions (length compositions))
	 (counter 0))
    (utils:message (format nil "Previewing dataset (~A composition(s))."
			   num-compositions) :detail 1)
    (dolist (composition compositions)
      (utils:message (format nil "(~A/~A)" (incf counter) num-compositions)
		     :detail 1)
      (if (preview composition :temp-dir temp-dir
		   :play-audio play-audio :display-score display-score)
	  (return)))))

(defmethod preview ((c idyom-db:mtp-composition) &key (temp-dir "/tmp/idyom/")
						   (play-audio t) (display-score t))
  (utils:message (format nil "Previewing composition ~A."
			 (idyom-db::composition-description c))
		 :detail 1)
  (if (or play-audio display-score)
      (let* ((dir-path (ensure-directories-exist (utils:ensure-directory temp-dir)))
	     (midi-file-path (export-data c :mid dir-path :filename "temp-audio.mid"))
	     (midi-file-path-string (namestring midi-file-path))
	     (pdf-file-path (merge-pathnames dir-path
					     (concatenate 'string "temp-score"
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

(defun bpm->usecs (bpm)
  (floor (* 1000000 (/ 60 bpm))))

(defun update-voice->channel-map (events)
  "Takes a list of events and updates the global voice->channel map.
   The idea is that low voices should be mapped to high channels,
   addressing the fact that in IDyOM low voices are typically 
   low-register, whereas in MIDI low channels are typically high-register."
  (let ((voice-list nil))
    (dolist (event events voice-list)
      (pushnew (idyom-db:get-attribute event :voice) voice-list))
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

(defun events->midi (events file &key (format 1) (program *default-program*)
				   encode-timesig encode-keysig)
  "Converts a list of CHARM events to a MIDI representation."
  (if *remap-voices* (update-voice->channel-map events))
  (let* ((*environment* nil)
	 (voice (idyom-db:get-attribute (car events) :voice))
	 (channel (voice->channel voice))
         (channel-msg (make-instance 'midi:program-change-message :time 0 
                                     :status (+ #xc0 channel) 
                                     :program program))
         (tempo (idyom-db:get-attribute (car events) :tempo))
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
  (let* ((non-onset (round (idyom-db:get-attribute event :onset)))
         (noff-onset (round (+ non-onset (idyom-db:get-attribute event :dur))))
         (voice (idyom-db:get-attribute event :voice))
	 (channel (voice->channel voice))
         (keynum  (round (+ (- 60 *midc*)
                            (idyom-db:get-attribute event :cpitch))))
         (velocity (idyom-db:get-attribute event :dyn))
	 (midi-messages
	  (list (make-instance 'midi:note-on-message
			       :time (* non-onset *tick-multiplier*)
			       :status (+ #x90 channel)
			       :key keynum 
			       :velocity (if velocity velocity
					     *default-velocity*))
		(make-instance 'midi:note-off-message
			       :time (* noff-onset *tick-multiplier*)
			       :status (+ #x80 channel)
			       :key keynum
			       :velocity (if velocity velocity
					     *default-velocity*)))))
    (if encode-keysig
	(let ((prev-keysig (second (assoc :keysig *environment*)))
	      (cur-keysig (idyom-db:get-attribute event :keysig))
	      (prev-mode (second (assoc :mode *environment*)))
	      (cur-mode (idyom-db:get-attribute event :mode))
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
    	       (cur-pulses (idyom-db:get-attribute event :pulses))
    	       (prev-barlength (second (assoc :barlength *environment*)))
    	       (cur-barlength (idyom-db:get-attribute event :barlength))
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





;; (defun event-lists->midi (event-lists file &key (format 1) (program *default-program*))
;;   (let* ((tempo (idyom-db:get-attribute (car (car event-lists)) :tempo))
;;          (tempo-msg (make-instance 'midi:tempo-message :time 0
;;                                    :status #xff
;;                                    :tempo (bpm->usecs 
;;                                            (if tempo tempo *default-tempo*))))
;;          (tracks (mapcar #'(lambda (x) 
;;                              (let* ((channel (idyom-db:get-attribute (car x) :voice))
;;                                     (channel-msg (make-instance 'midi:program-change-message :time 0 
;;                                                                 :status (+ #xc0 channel) 
;;                                                                 :program program)))
;;                                (incf program)
;;                                (cons channel-msg (cons tempo-msg (mapcan #'event->midi x))))) 
;;                          event-lists))
;;          (midifile (make-instance 'midi:midifile
;;                                   :format format
;;                                   :division (* (/ *timebase* 4) *tick-multiplier*)
;;                                   :tracks tracks)))
;;     (midi:write-midi-file midifile file)))

;; (defmethod export-data ((event-list list) (type (eql :mid)) path)
;;   (let* ((first-event (car event-list))
;; 	 (first-id (identifier first-event))
;;          (title (idyom-db:get-description (dataset-id first-id)
;; 					   (composition-id first-id)))
;;          (file (concatenate 'string path "/" title ".mid")))
;;     (events->midi event-list file)))
