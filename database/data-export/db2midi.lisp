;;;; ======================================================================
;;;; File:       db2midi.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2005-06-09 11:01:51 marcusp>
;;;; Time-stamp: <2023-05-22 14:35:46 marcusp>
;;;; ======================================================================

(cl:in-package #:db2midi)

;;; User parameters

(defparameter *timebase* 96)
(defparameter *midc* 60) 

(defparameter *tick-multiplier* 1)
(defparameter *default-tempo* 100)
(defparameter *default-velocity* 100)
(defparameter *default-program* 0)

;; This user parameter determines whether voices are remapped from the
;; IDyOM order (low voice number implies low register) to the standard
;; MIDI order (low channel implies high register).
(defparameter *remap-voices* t)

;; Whether key and time signatures are to be encoded
(defparameter *encode-timesig* t)   
(defparameter *encode-keysig* t)

;;; Global variables

(defvar *voice->channel-map* nil)
(defvar *environment* nil)



;;; Export data

(defmethod export-data ((d idyom-db:mtp-dataset) (type (eql :mid)) dir &key filename)
  "Export an entire dataset from the database."
  (declare (ignore filename))
  (let* ((*timebase* (idyom-db:dataset-timebase d))
         (*midc*     (idyom-db:dataset-midc d))
         (dir-path (utils:ensure-directory dir))
         (compositions (idyom-db:dataset-compositions d))
	 (num-compositions (length compositions)))
    (ensure-directories-exist dir-path)
    (utils:message (format nil "Exporting dataset (~A composition(s)) to MIDI format."
			   num-compositions) :detail 1)
    (utils:dolist-pb (c compositions dir-path)
      (export-data c type dir-path))))

(defmethod export-data ((c idyom-db:mtp-composition) (type (eql :mid)) dir &key filename)
  "Export an individual composition from the database."
  (let* ((dir-path (utils:ensure-directory dir))
         (id (idyom-db:get-id c))
         (description (idyom-db:get-description (first id) (second id)))
         (filename (if filename filename (concatenate 'string description ".mid")))
         (file-path (merge-pathnames dir-path (pathname filename)))
         (*timebase* (idyom-db:get-timebase (first id) (second id)))
         (*midc* (idyom-db:get-midc (first id))))
    (ensure-directories-exist dir-path)
    (events->midi (idyom-db:composition-events c) file-path
                  :encode-keysig *encode-keysig*
		  :encode-timesig *encode-timesig*)
    file-path))

(defmethod export-data ((event-list list) (type (eql :mid)) dir &key filename)
  "Export a list of events, which may be database events (mtp-event)
or music objects events (music-event)."
  (let ((first-event (car event-list)))
    (typecase first-event
      (idyom-db:mtp-event
       (let* ((dir-path (utils:ensure-directory dir))
              (id (idyom-db:get-id first-event))
              (description (idyom-db:get-description (first id) (second id)))
              (filename (if filename filename (concatenate 'string description ".mid")))
              (file-path (merge-pathnames dir-path (pathname filename))))
         (ensure-directories-exist dir-path)
         (events->midi event-list file-path))))))


;;; Convert database events to midi

(defun events->midi (events file &key (format 1) (program *default-program*)
				   encode-timesig encode-keysig)
  "Converts a list of database events to a MIDI representation."
  (if *remap-voices* (update-voice->channel-map events))
  (let* ((*environment* nil)
	 (voice (idyom-db:get-attribute (car events) :voice)) ;; 
	 (channel (voice->channel voice))
         (channel-msg (make-instance 'midi:program-change-message :time 0 
                                     :status (+ #xc0 channel) 
                                     :program program))
         (tempo (idyom-db:get-attribute (car events) :tempo)) ;; 
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

;; (defun db-event-lists->midi (event-lists file &key (format 1) (program *default-program*))
;;   "Writes a list of lists of database events <event-lists> to a midi
;; file <file> with each list of events written to a separate midi
;; channel."
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
;;                                (cons channel-msg (cons tempo-msg (mapcan #'db-event->midi x))))) 
;;                          event-lists))
;;          (midifile (make-instance 'midi:midifile
;;                                   :format format
;;                                   :division (* (/ *timebase* 4) *tick-multiplier*)
;;                                   :tracks tracks)))
;;     (midi:write-midi-file midifile file)))


(defun event->midi (event &key encode-timesig encode-keysig)
  "Returns midi note on/off messages corresponding to the db event <event>."
  (let* ((non-onset (round (idyom-db:get-attribute event :onset))) ;; 
         (noff-onset (round (+ non-onset (idyom-db:get-attribute event :dur)))) ;; 
         (voice (idyom-db:get-attribute event :voice)) ;; 
         (channel (if (null voice) 1 (voice->channel voice)))
         (keynum  (round (+ (- 60 *midc*)
                            (idyom-db:get-attribute event :cpitch)))) ;; 
         (velocity (idyom-db:get-attribute event :dyn)) ;; 
         (midi-messages (list (make-instance 'midi:note-on-message
                                             :time (* non-onset *tick-multiplier*)
                                             :status (+ #x90 channel)
                                             :key keynum 
                                             :velocity (if velocity velocity *default-velocity*))
                              (make-instance 'midi:note-off-message
                                             :time (* noff-onset *tick-multiplier*)
                                             :status (+ #x80 channel)
                                             :key keynum
                                             :velocity (if velocity velocity *default-velocity*)))))
    (when encode-keysig
      (let ((keysig-msg (keysig-msg event non-onset)))
        (when keysig-msg (push keysig-msg midi-messages))))
    (when encode-timesig
      (let ((timesig-msg (timesig-msg event non-onset)))
        (when timesig-msg (push timesig-msg midi-messages))))
    midi-messages))

(defun keysig-msg (event midi-onset)
  (let* ((prev-keysig (second (assoc :keysig *environment*)))
         (cur-keysig (idyom-db:get-attribute event :keysig)) ;; 
         (prev-mode (second (assoc :mode *environment*)))
         (cur-mode (idyom-db:get-attribute event :mode)) ;; 
         (cur-mode (when cur-mode (if (= cur-mode 9) 1 0)))
         (first-keysig-reached (second (assoc :first-keysig-reached
                                              *environment*))))
    (when cur-keysig
      (when (not (integerp cur-keysig))
        (error (format nil "keysig must be an integer, but found ~A."
                       cur-keysig)))
      (when (not (and (<= -7 cur-keysig) (<= cur-keysig 7)))
        (error (format nil "keysig must be between -7 and 7, but found ~A."
                       cur-keysig)))
      (when (not (and (equal prev-keysig cur-keysig)
                      (equal prev-mode cur-mode)))
        (let ((keysig-msg
               (make-instance 'midi:key-signature-message
                              :time (if first-keysig-reached
                                        (* midi-onset *tick-multiplier*)
                                        0)
                              :status #xFF)))
          (setf (slot-value keysig-msg 'midi::mi) cur-mode)
          (setf (slot-value keysig-msg 'midi::sf) cur-keysig)
          (setf *environment* (utils:update-alist
                               *environment*
                               (list :keysig cur-keysig)
                               (list :mode cur-mode)
                               (list :first-keysig-reached t)))
          keysig-msg)))))

(defun timesig-msg (event midi-onset)
  (let* ((prev-pulses (second (assoc :pulses *environment*)))
         (cur-pulses (idyom-db:get-attribute event :pulses)) ;; 
         (prev-barlength (second (assoc :barlength *environment*)))
         (cur-barlength (idyom-db:get-attribute event :barlength)) ;; 
         (numerator (round cur-pulses))
         (pulse-dur (round (/ cur-barlength cur-pulses)))
         (pulse-whole-notes (/ pulse-dur *timebase*))
         (denominator (/ 1 pulse-whole-notes))
         (multiplier (denominator denominator))
         (numerator (* numerator multiplier))
         (denominator (* denominator multiplier))
         (dd (round (log denominator 2))))
    (when (and (not (or (null cur-pulses) (null cur-barlength)))
               (or (not (equal cur-pulses prev-pulses))
                   (not (equal cur-barlength prev-barlength))))
      (let ((timesig-msg
             (make-instance 'midi:time-signature-message
                            :time (* midi-onset *tick-multiplier*)
                            :status #xFF)))
        (setf (slot-value timesig-msg 'midi::nn) numerator)
        (setf (slot-value timesig-msg 'midi::dd) dd)
        (setf (slot-value timesig-msg 'midi::cc) (* pulse-whole-notes 96))
        (setf (slot-value timesig-msg 'midi::bb) 8)
        (setf *environment* (utils:update-alist
                             *environment*
                             (list :pulses cur-pulses)
                             (list :barlength cur-barlength)))
        timesig-msg))))

;;; Helper functions 

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
