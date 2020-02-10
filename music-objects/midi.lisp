;;;; ======================================================================
;;;; File:       midi.lisp
;;;; Author:     Marcus Pearce <marcus.pearce@qmul.ac.uk>
;;;; Created:    <2020-02-05 07:01:51 marcusp>
;;;; Time-stamp: <2020-02-09 14:22:07 marcusp>
;;;; ======================================================================

(cl:in-package #:music-data)

(defvar *timebase* 96)
(defvar *midc* 60) 

(defvar *tick-multiplier* 1)
(defvar *default-tempo* 100)
(defvar *default-velocity* 100)
(defvar *default-program* 0)

;;; Export data

(defgeneric export-data (events type path))

(defmethod export-data ((event-list list) (type (eql :mid)) path)
  "Export a list of events, which may be database events (mtp-event)
or music objects events (music-event)."
  (let* ((first-event (car event-list))
         (id (md:get-identifier first-event))
         (did (md:get-dataset-index id))
         (cid (md:get-composition-index id))
         (description (idyom-db:get-description did cid))
         (filename (concatenate 'string path "/" description ".mid")))
    (print filename)
    (mo-events->midi event-list filename)))


;;; Music Objects

(defun mo-events->midi (events file &key (format 1) (program *default-program*))
  "Writes a list of database events <events> to a midi file <file>."
  (let* ((channel (md:get-attribute (car events) :voice))
         (channel-msg (make-instance 'midi:program-change-message :time 0 
                                     :status (+ #xc0 channel) 
                                     :program program))
         (tempo (md:get-attribute (car events) :tempo))
         (tempo-msg (make-instance 'midi:tempo-message :time 0
                                   :status #xff
                                   :tempo (bpm->usecs 
                                           (if tempo tempo *default-tempo*))))
         (track (mapcan #'mo-event->midi events))
         (midifile (make-instance 'midi:midifile
                                  :format format
                                  :division (* (/ *timebase* 4) *tick-multiplier*)
                                  :tracks (list 
                                           (cons tempo-msg
                                                 (cons channel-msg track))))))
    (midi:write-midi-file midifile file)
    midifile))

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
