;;;; ======================================================================
;;;; File:       midi.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2020-02-05 07:01:51 marcusp>
;;;; Time-stamp: <2020-07-10 09:33:42 marcusp>
;;;; ======================================================================

(cl:in-package #:music-data)

(defvar *timebase* 96)
(defvar *midc* 60) 

(defvar *tick-multiplier* 1)
(defvar *default-tempo* 100)
(defvar *default-velocity* 100)
(defvar *default-program* 0)

;;; Export data

(defgeneric export-data (events type path &optional filename))

(defmethod export-data ((mo music-object) (type (eql :mid)) path &optional filename)
  (export-data (coerce mo 'list) type path filename))
                                                                  
(defmethod export-data ((event-list list) (type (eql :mid)) path &optional filename)
  "Export a list of music objects events (music-event)."
  (let* ((first-event (car event-list))
         (id (md:get-identifier first-event))
         (did (md:get-dataset-index id))
         (cid (md:get-composition-index id))
         (description (idyom-db:get-description did cid))
         (filename (concatenate 'string path "/" (if filename filename description) ".mid")))
    (print filename)
    (mo-events->midi event-list filename)))


;;; Music Objects

(defun mo-events->midi (events file &key (format 1) (program *default-program*))
  "Writes a list of database events <events> to a midi file <file>."
  (let* (;; sequence attributes
         (channel (md:get-attribute (car events) :voice))
         (tempo (md:get-attribute (car events) :tempo))
         (keysig (md:get-attribute (car events) :keysig))
         (mode (md:get-attribute (car events) :mode))
         (mode (when mode (if (= mode 9) 1 0)))
         (pulses (md:get-attribute (car events) :pulses))
         (barlength (md:get-attribute (car events) :barlength))
         (midi-dd (when (and pulses barlength) (round (- (log (/ barlength (* pulses *timebase*)) 2)))))
         ;; channel
         (channel-msg (make-instance 'midi:program-change-message :time 0 
                                     :status (+ #xc0 channel) 
                                     :program program))
         ;; tempo
         (tempo-msg (make-instance 'midi:tempo-message :time 0
                                   :status #xff
                                   :tempo (bpm->usecs 
                                           (if tempo tempo *default-tempo*))))
         ;; keysig
         (keysig-msg (make-instance 'midi:key-signature-message :time 0
                                    :status #xff))
         ;; timesig
         (timesig-msg (make-instance 'midi:time-signature-message :time 0
                                     :status #xff))
         ;; midi track
         (track (mapcan #'mo-event->midi events)))
    (push channel-msg track)
    (when (and keysig mode)
      (setf (slot-value keysig-msg 'midi::sf) keysig
            (slot-value keysig-msg 'midi::mi) mode)
      (push keysig-msg track))
    (when midi-dd
      (setf (slot-value timesig-msg 'midi::nn) pulses
            (slot-value timesig-msg 'midi::dd) midi-dd
            (slot-value timesig-msg 'midi::cc) 0
            (slot-value timesig-msg 'midi::bb) 8)
      (push timesig-msg track))
    (push tempo-msg track)
    (let ((midifile (make-instance 'midi:midifile
                                   :format format
                                   :division (* (/ *timebase* 4) *tick-multiplier*)
                                   :tracks (list track))))
      (midi:write-midi-file midifile file)
      midifile)))

(defun mo-event->midi (event)
  "Returns midi note on/off messages corresponding to the music object event <event>."
  (let* ((non-onset (md:get-attribute event :onset))
         (noff-onset (+ non-onset (md:get-attribute event :dur)))
         (channel (md:get-attribute event :voice))
         (keynum  (round (+ (- 60 *midc*)
                            (md:get-attribute event :cpitch))))
         (velocity (md:get-attribute event :dyn)))
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

;;; Helper

(defun bpm->usecs (bpm)
  (floor (* 1000000 (/ 60 bpm))))
