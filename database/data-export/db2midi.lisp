;;;; ======================================================================
;;;; File:       db2midi.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2005-06-09 11:01:51 marcusp>
;;;; Time-stamp: <2017-02-15 11:55:54 peter>
;;;; ======================================================================

(cl:in-package #:db2midi)

;; This user parameter determines whether voices are remapped from the
;; IDyOM order (low voice number implies low register) to the standard
;; MIDI order (low channel implies high register).
(defvar *remap-voices* t)

(defvar *timebase* 96)
(defvar *midc* 60) 

(defvar *tick-multiplier* 1)
(defvar *default-tempo* 100)
(defvar *default-velocity* 100)
(defvar *default-program* 0)

(defvar *voice->channel-map* nil)

(defmethod export-data ((d idyom-db:mtp-dataset) (type (eql :mid)) path)
  (let ((*timebase* (idyom-db::dataset-timebase d))
        (*midc*     (idyom-db::dataset-midc d)))
    (dolist (c (idyom-db::dataset-compositions d))
      (export-data c type path))))

(defmethod export-data ((c idyom-db:mtp-composition) (type (eql :mid)) path)
  ;; FIXME: *midc* is never set if export-data is called with a
  ;; composition directly.
  (let* ((title (idyom-db::composition-description c))
         (file (concatenate 'string path "/" title ".mid"))
         (*timebase* (idyom-db::composition-timebase c)))
    (events->midi (idyom-db::composition-events c) file)))

;; (defmethod export-data ((event-list list) (type (eql :mid)) path)
;;   (let* ((first-event (car event-list))
;; 	 (first-id (identifier first-event))
;;          (title (idyom-db:get-description (dataset-id first-id)
;; 					   (composition-id first-id)))
;;          (file (concatenate 'string path "/" title ".mid")))
;;     (events->midi event-list file)))

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

(defun events->midi (events file &key (format 1) (program *default-program*))
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
  

