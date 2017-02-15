;;;; ======================================================================
;;;; File:       db2midi.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2005-06-09 11:01:51 marcusp>
;;;; Time-stamp: <2017-02-15 17:37:17 peter>
;;;; ======================================================================

(cl:in-package #:db2midi)

;;;==================
;;;* User parameters *
;;;==================

;; This user parameter determines whether voices are remapped from the
;; IDyOM order (low voice number implies low register) to the standard
;; MIDI order (low channel implies high register).
(defvar *remap-voices* t)

;; Path to the user's Timidity executable
(defvar *timidity-path* "/usr/local/Cellar/timidity/2.14.0/bin/timidity")

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
      (events->midi (idyom-db::composition-events c) file-path)
      file-path))

(defmethod play-audio ((d idyom-db:mtp-dataset) &key (temp-dir "/tmp/idyom/"))
  (let* ((compositions (idyom-db::dataset-compositions d))
	 (num-compositions (length compositions))
	 (counter 0))
    (utils:message (format nil "Playing dataset (~A composition(s))."
			   num-compositions) :detail 1)
    (dolist (composition compositions)
      (utils:message (format nil "(~A/~A)" (incf counter) num-compositions)
		     :detail 1)
      (if (play-audio composition :temp-dir temp-dir)
	  (return)))))

(defmethod play-audio ((c idyom-db:mtp-composition) &key (temp-dir "/tmp/idyom/"))
  (let* ((dir-path (ensure-directories-exist (utils:ensure-directory temp-dir)))
	 (file-path (export-data c :mid dir-path :filename "temp-audio.mid"))
	 (file-path-string (namestring file-path)))
    (utils:message (format nil "Playing composition ~A."
			   (idyom-db::composition-description c))
		   :detail 1)
    (utils:message (format nil "Saved temporary midi file to ~A."
			   (namestring file-path-string))
		   :detail 3)
    (utils:message (format nil "Playing MIDI file using application ~A."
			   *timidity-path*)
		   :detail 3)
    (let ((process (sb-ext:run-program *timidity-path* (list file-path-string) :wait nil)))
      (utils:message
       (format nil "Press enter to skip, or Q then enter to quit.~%")
	       :detail 1)
      (let ((char (read-char)))
	(if (eql (sb-ext:process-status process) :running)
	    (sb-ext:process-kill process 15))
	(if (eql char #\q) t nil)))))

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

(defun events->midi (events file &key (format 1) (program *default-program*))
  "Converts a list of CHARM events to a MIDI representation."
  (if *remap-voices* (update-voice->channel-map events))
  (let* ((voice (idyom-db:get-attribute (car events) :voice))
	 (channel (voice->channel voice))
         (channel-msg (make-instance 'midi:program-change-message :time 0 
                                     :status (+ #xc0 channel) 
                                     :program program))
         (tempo (idyom-db:get-attribute (car events) :tempo))
         (tempo-msg (make-instance 'midi:tempo-message :time 0
                                   :status #xff
                                   :tempo (bpm->usecs 
                                           (if tempo tempo *default-tempo*))))
         (track (mapcan #'event->midi events))
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

(defun event->midi (event)
  "Converts a CHARM event to a MIDI representation."
  (let* ((non-onset (round (idyom-db:get-attribute event :onset)))
         (noff-onset (round (+ non-onset (idyom-db:get-attribute event :dur))))
         (voice (idyom-db:get-attribute event :voice))
	 (channel (voice->channel voice))
         (keynum  (round (+ (- 60 *midc*)
                            (idyom-db:get-attribute event :cpitch))))
         (velocity (idyom-db:get-attribute event :dyn)))
    (list (make-instance 'midi:note-on-message
                         :time (* non-onset *tick-multiplier*)
                         :status (+ #x90 channel)
                         :key keynum 
                         :velocity (if velocity velocity *default-velocity*))
          (make-instance 'midi:note-off-message
                         :time (* noff-onset *tick-multiplier*)
                         :status (+ #x80 channel)
                         :key keynum
                         :velocity (if velocity velocity *default-velocity*)))))

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
